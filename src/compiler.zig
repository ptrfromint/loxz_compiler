const std = @import("std");

const bytecode = @import("bytecode.zig");
const Chunk = bytecode.Chunk;
const Opcode = bytecode.Opcode;
const debug = @import("debug.zig");
const GarbageCollector = @import("garbage_collector.zig").GarbageCollector;
const Parser = @import("parser.zig").Parser;
const Scanner = @import("scanner.zig").Scanner;
const Token = @import("scanner.zig").Token;
const util = @import("util.zig");
const Value = @import("value.zig").Value;

const CompilerErrorSet = error{ CompilationError, OutOfMemory, InvalidCharacter };

pub const CallFrame = struct {
    closure: *Value.Obj,
    ip: usize,
    slot_start: usize,
};

const FunctionType = enum { function, script };

pub const FunctionState = struct {
    pub const Local = struct {
        name: Token,
        depth: ?usize,
        is_captured: bool = false,
    };

    pub const Upvalue = struct {
        pub const CaptureScope = enum(u1) { outer, local = 1 };

        index: usize,
        capture_scope: Upvalue.CaptureScope,

        pub fn eql(self: Upvalue, other: Upvalue) bool {
            return self.index == other.index and self.capture_scope == other.capture_scope;
        }
    };

    locals: std.ArrayList(Local),
    upvalues: std.ArrayList(Upvalue),
    depth: usize,
    function: *Value.Obj,
    func_type: FunctionType,
    enclosing: ?*FunctionState,

    pub fn resolveLocal(self: *FunctionState, name: Token) !?usize {
        var i = self.locals.items.len;
        while (i > 0) {
            i -= 1;
            const local = self.locals.items[i];
            if (std.mem.eql(u8, local.name.lexeme, name.lexeme)) {
                if (local.depth == null) {
                    try debug.errorAt(local.name, "Can't read local variable in it's own initializer.");
                }
                return i;
            }
        }

        return null;
    }

    pub fn resolveUpvalue(self: *FunctionState, allocator: std.mem.Allocator, name: Token) !?usize {
        if (self.enclosing) |parent_func| {
            if (try parent_func.resolveLocal(name)) |index| {
                parent_func.locals.items[index].is_captured = true;
                return try self.addUpvalue(allocator, .{ .index = index, .capture_scope = .local });
            }

            if (try parent_func.resolveUpvalue(allocator, name)) |upvalue| {
                return try self.addUpvalue(allocator, .{ .index = upvalue, .capture_scope = .outer });
            }
        }

        return null;
    }

    pub fn addUpvalue(
        self: *FunctionState,
        allocator: std.mem.Allocator,
        upvalue: Upvalue,
    ) !usize {
        for (self.upvalues.items, 0..) |current, i| {
            if (current.eql(upvalue)) {
                return i;
            }
        }

        try self.upvalues.append(allocator, upvalue);
        return self.upvalues.items.len - 1;
    }
};

pub const Compiler = struct {
    allocator: std.mem.Allocator,
    scanner: *Scanner,
    parser: *Parser,

    // We keep track of the root state to free it in deinit,
    // separate from the current state which might be on the stack.
    root_function_state: *FunctionState,
    current_function: *FunctionState,

    objects: *?*Value.Obj,

    pub const Error = error{CompilationError};

    pub const Precedence = enum {
        none,
        assignment,
        @"or",
        @"and",
        equality,
        comparison,
        term,
        factor,
        unary,
        call,
        primary,
    };

    pub fn init(allocator: std.mem.Allocator, objects: *?*Value.Obj, source: []const u8) !Compiler {
        const scanner = try allocator.create(Scanner);
        scanner.* = .init(allocator, source);

        const parser = try allocator.create(Parser);
        parser.* = .init(scanner);

        const func_state = try allocator.create(FunctionState);
        func_state.* = .{
            .locals = .empty,
            .upvalues = .empty,
            .depth = 0,
            .function = try Value.Obj.allocFunc(allocator, objects, 0, null),
            .func_type = .script,
            .enclosing = null,
        };

        // Reserve the first slot for the VM
        try func_state.locals.append(allocator, .{
            .name = .{ .lexeme = "", .line = 0, .type = .identifier },
            .depth = 0,
        });

        return .{
            .allocator = allocator,
            .scanner = scanner,
            .parser = parser,
            .root_function_state = func_state,
            .current_function = func_state,
            .objects = objects,
        };
    }

    pub fn deinit(self: *Compiler) void {
        self.scanner.deinit();
        self.allocator.destroy(self.scanner);
        self.allocator.destroy(self.parser);

        // Free the root function state logic
        self.root_function_state.locals.deinit(self.allocator);
        self.root_function_state.upvalues.deinit(self.allocator);
        self.allocator.destroy(self.root_function_state);
    }

    pub fn markRoots(self: *Compiler, gc: *GarbageCollector) !void {
        var func: ?*FunctionState = self.current_function;
        while (func) |f| {
            try gc.markObject(f.function);
            func = f.enclosing;
        }
    }

    pub fn compile(self: *Compiler) !*Value.Obj {
        try self.parser.advance();

        while (!try self.match(.eof)) try self.declaration();

        return try self.endCompiler();
    }

    pub fn emitBytes(self: *Compiler, bytes: []const u8) !void {
        std.debug.assert(self.parser.previous != null);
        try self.currentChunk().appendBytes(bytes, self.parser.previous.?.line);
    }

    pub fn emitConstant(self: *Compiler, value: Value) !void {
        try self.currentChunk().addConst(value, self.parser.previous.?.line);
    }

    fn currentChunk(self: *Compiler) *Chunk {
        return &self.current_function.function.kind.function.chunk;
    }

    fn endCompiler(self: *Compiler) !*Value.Obj {
        try self.emitReturn();

        return self.current_function.function;
    }

    fn emitReturn(self: *Compiler) !void {
        try self.emitBytes(&.{ @intFromEnum(Opcode.nil), @intFromEnum(Opcode.@"return") });
    }

    fn declaration(self: *Compiler) CompilerErrorSet!void {
        if (try self.match(.fun)) {
            try self.funDeclaration();
        } else if (try self.match(.@"var")) {
            try self.varDeclaration();
        } else {
            try self.statement();
        }
    }

    fn funDeclaration(self: *Compiler) !void {
        const global_index = try self.parseVariable("Expected function name.");
        try self.markInitialized();
        try self.function(.function);
        try self.defineVariable(global_index);
    }

    fn varDeclaration(self: *Compiler) !void {
        const global = try self.parseVariable("Expected variable name.");

        if (try self.match(.equal)) {
            try self.expression();
        } else {
            try self.emitBytes(&.{@intFromEnum(Opcode.nil)});
        }

        try self.parser.consume(.semicolon, "Expected ';' after variable declaration.");
        try self.defineVariable(global);
    }

    fn statement(self: *Compiler) CompilerErrorSet!void {
        if (try self.match(.print)) {
            try self.printStatement();
        } else if (try self.match(.@"for")) {
            try self.forStatement();
        } else if (try self.match(.@"if")) {
            try self.ifStatement();
        } else if (try self.match(.@"return")) {
            try self.returnStatement();
        } else if (try self.match(.@"while")) {
            try self.whileStatement();
        } else if (try self.match(.left_brace)) {
            try self.startScope();
            try self.block();
            try self.endScope();
        } else {
            try self.expressionStatement();
        }
    }

    fn startScope(self: *Compiler) !void {
        self.current_function.depth += 1;
    }

    fn endScope(self: *Compiler) !void {
        self.current_function.depth -= 1;

        var i = self.current_function.locals.items.len;
        while (i > 0) {
            i -= 1;
            const local = self.current_function.locals.items[i];

            if (local.depth != null and local.depth.? > self.current_function.depth) {
                if (local.is_captured) {
                    try self.emitBytes(&.{@intFromEnum(Opcode.close_upvalue)});
                } else {
                    try self.emitBytes(&.{@intFromEnum(Opcode.pop)});
                }

                _ = self.current_function.locals.pop();
            } else {
                break;
            }
        }
    }

    fn function(self: *Compiler, func_type: FunctionType) !void {
        // We use the same allocator (GC)
        const allocator = self.allocator;

        var func_state: FunctionState = .{
            .func_type = func_type,
            .enclosing = self.current_function,
            .depth = 0,
            .locals = .empty,
            .upvalues = .empty,
            .function = try Value.Obj.allocFunc(allocator, self.objects, 0, self.parser.previous.?.lexeme),
        };
        defer {
            func_state.locals.deinit(allocator);
            func_state.upvalues.deinit(allocator);
        }

        const prev_func = self.current_function;
        self.current_function = &func_state;

        try self.current_function.locals.append(allocator, .{
            .name = .{ .lexeme = "", .line = 0, .type = .identifier },
            .depth = 0,
        });

        try self.startScope();

        try self.parser.consume(.left_paren, "Expected '(' after function name.");
        if (!self.check(.right_paren)) while (true) {
            self.current_function.function.kind.function.arity += 1;

            if (self.current_function.function.kind.function.arity > 255) {
                try debug.errorAt(self.parser.current.?, "Can't have more than 255 parameters");
            }

            const paramConstant = try self.parseVariable("Expected parameter name.");
            try self.defineVariable(paramConstant);

            if (!try self.match(.comma)) break;
        };
        try self.parser.consume(.right_paren, "Expected ')' after parameters.");

        try self.parser.consume(.left_brace, "Expected '{' before function body.");
        try self.block();

        const compiled_func = try self.endCompiler();
        compiled_func.kind.function.upvalue_count = func_state.upvalues.items.len;
        self.current_function = prev_func;

        const index = try self.currentChunk().addConstant(.{ .obj = compiled_func });
        try self.emitBytes(&.{ @intFromEnum(Opcode.closure), @truncate(index) });

        for (func_state.upvalues.items) |upvalue| {
            try self.emitBytes(&.{ @intFromEnum(upvalue.capture_scope), @truncate(upvalue.index) });
        }
    }

    fn returnStatement(self: *Compiler) !void {
        if (self.current_function.func_type == .script) {
            try debug.errorAt(self.parser.previous.?, "Can't return from top-level code.");
        }

        if (try self.match(.semicolon)) {
            try self.emitReturn();
        } else {
            try self.expression();
            try self.parser.consume(.semicolon, "Expected ';' after return value.");
            try self.emitBytes(&.{@intFromEnum(Opcode.@"return")});
        }
    }

    fn block(self: *Compiler) !void {
        while (!self.check(.right_brace) and !self.check(.eof)) {
            try self.declaration();
        }

        try self.parser.consume(.right_brace, "Expected '}' after block.");
    }

    fn forStatement(self: *Compiler) !void {
        try self.startScope();

        try self.parser.consume(.left_paren, "Expected '(' after 'for'.");
        if (try self.match(.semicolon)) {} else if (try self.match(.@"var")) {
            try self.varDeclaration();
        } else {
            try self.expressionStatement();
        }

        var loop_start = self.currentChunk().code.items.len;

        var exit_jump: ?usize = null;
        if (!try self.match(.semicolon)) {
            try self.expression();
            try self.parser.consume(.semicolon, "Expected ';' after loop condition.");

            exit_jump = try self.emitPlaceholderJump(.jump_if_false);
            try self.emitBytes(&.{@intFromEnum(Opcode.pop)});
        }

        if (!try self.match(.right_paren)) {
            const body_jump = try self.emitPlaceholderJump(.jump);

            const increment_start = self.currentChunk().code.items.len;
            try self.expression();
            try self.emitBytes(&.{@intFromEnum(Opcode.pop)});
            try self.parser.consume(.right_paren, "Expected ')' after 'for' clauses.");

            try self.emitLoop(loop_start);
            loop_start = increment_start;
            try self.backpatchJumpOp(body_jump);
        }

        try self.statement();

        try self.emitLoop(loop_start);

        if (exit_jump) |offset| {
            try self.backpatchJumpOp(offset);
            try self.emitBytes(&.{@intFromEnum(Opcode.pop)});
        }

        try self.endScope();
    }

    fn whileStatement(self: *Compiler) !void {
        const loop_start = self.currentChunk().code.items.len;

        try self.parser.consume(.left_paren, "Expected '(' after 'while'.");
        try self.expression();
        try self.parser.consume(.right_paren, "Expected ')' after 'while'.");

        const exitJump = try self.emitPlaceholderJump(.jump_if_false);

        try self.emitBytes(&.{@intFromEnum(Opcode.pop)});
        try self.statement();

        try self.emitLoop(loop_start);

        try self.backpatchJumpOp(exitJump);
        try self.emitBytes(&.{@intFromEnum(Opcode.pop)});
    }

    fn emitLoop(self: *Compiler, loop_start: usize) !void {
        try self.emitBytes(&.{@intFromEnum(Opcode.loop)});

        const offset = self.currentChunk().code.items.len - loop_start + 2;
        if (offset > std.math.maxInt(u16)) @panic("Loop body too large.");

        var bytes: [2]u8 = undefined;
        util.fillU16LE(&bytes, @intCast(offset));
        try self.emitBytes(&bytes);
    }

    fn ifStatement(self: *Compiler) !void {
        try self.parser.consume(.left_paren, "Expected '(' after 'if'.");
        try self.expression();
        try self.parser.consume(.right_paren, "Expected ')' after condition.");

        const then_jump = try self.emitPlaceholderJump(.jump_if_false);
        try self.emitBytes(&.{@intFromEnum(Opcode.pop)}); // Clean up condition on TRUE path
        try self.statement(); // Compile 'then' block

        const else_jump = try self.emitPlaceholderJump(.jump);
        try self.backpatchJumpOp(then_jump);
        try self.emitBytes(&.{@intFromEnum(Opcode.pop)}); // Clean up condition on FALSE path

        if (try self.match(.@"else")) try self.statement();

        try self.backpatchJumpOp(else_jump);
    }

    fn emitPlaceholderJump(self: *Compiler, op: Opcode) !usize {
        try self.emitBytes(&.{ @intFromEnum(op), 0xff, 0xff });
        return self.currentChunk().code.items.len - 2;
    }

    fn backpatchJumpOp(self: *Compiler, offset: usize) !void {
        // NOTE: offset - 2 to account for the bytecode for the jump instruction itself
        const jump_index = self.currentChunk().code.items.len - offset - 2;

        if (jump_index > std.math.maxInt(u16)) @panic("More than 65536 bytes of code to jump over");

        util.fillU16LE(self.currentChunk().code.items[offset .. offset + 2], jump_index);
    }

    fn printStatement(self: *Compiler) !void {
        try self.expression();
        try self.parser.consume(.semicolon, "Expected ';' after value.");
        try self.emitBytes(&.{@intFromEnum(Opcode.print)});
    }

    fn expressionStatement(self: *Compiler) !void {
        try self.expression();
        try self.parser.consume(.semicolon, "Expected ';' after expression.");
        try self.emitBytes(&.{@intFromEnum(Opcode.pop)});
    }

    fn match(self: *Compiler, tok_type: Token.Type) !bool {
        if (!self.check(tok_type)) return false;

        try self.parser.advance();
        return true;
    }

    fn check(self: *Compiler, tok_type: Token.Type) bool {
        return self.parser.current.?.type == tok_type;
    }

    fn identifierConstant(self: *Compiler, name: Token) !usize {
        const allocator = self.allocator;
        return try self.currentChunk().addConstant(.{
            .obj = try Value.Obj.allocString(allocator, self.objects, name.lexeme),
        });
    }

    fn parseVariable(self: *Compiler, err_msg: []const u8) !usize {
        try self.parser.consume(.identifier, err_msg);

        try self.declareVariable();
        if (self.current_function.depth > 0) return 0;

        return self.identifierConstant(self.parser.previous.?);
    }

    fn declareVariable(self: *Compiler) !void {
        // Global variables are implicitly declared already
        if (self.current_function.depth == 0) return;

        if (self.current_function.locals.items.len < 1) return try self.addLocal(self.parser.previous.?);

        const var_name = self.parser.previous.?;
        var i: usize = self.current_function.locals.items.len - 1;
        while (i >= 0) : (i -= 1) {
            const local = self.current_function.locals.items[i];
            if (local.depth != null and local.depth.? < self.current_function.depth) {
                break;
            }

            if (std.mem.eql(u8, var_name.lexeme, local.name.lexeme)) {
                try debug.errorAt(var_name, "A variable with this name is already declared.");
            }
        }

        try self.addLocal(self.parser.previous.?);
    }

    fn defineVariable(self: *Compiler, global: usize) !void {
        if (self.current_function.depth > 0) {
            return self.markInitialized();
        }

        if (global <= std.math.maxInt(u8)) {
            var bytes: [2]u8 = .{ @intFromEnum(Opcode.make_global), @truncate(global) };
            try self.emitBytes(bytes[0..]);
        } else {
            var bytes: [4]u8 = .{ @intFromEnum(Opcode.make_global_long), 0, 0, 0 };
            util.fillU24LE(bytes[1..], global);
            try self.emitBytes(bytes[0..]);
        }
    }

    fn addLocal(self: *Compiler, name: Token) !void {
        const allocator = self.allocator;

        try self.current_function.locals.append(allocator, .{
            .name = name,
            .depth = null,
        });
    }

    fn markInitialized(self: *Compiler) !void {
        if (self.current_function.locals.items.len < 1 or self.current_function.depth == 0) return;
        const last_index = self.current_function.locals.items.len - 1;
        self.current_function.locals.items[last_index].depth = self.current_function.depth;
    }

    fn emitVariableOp(
        self: *Compiler,
        index: usize,
        comptime op: enum { set, get },
        comptime scope: enum { global, local, upvalue },
    ) !void {
        const short_op = switch (op) {
            .get => switch (scope) {
                .global => Opcode.get_global,
                .local => Opcode.get_local,
                .upvalue => Opcode.get_upvalue,
            },
            .set => switch (scope) {
                .global => Opcode.set_global,
                .local => Opcode.set_local,
                .upvalue => Opcode.set_upvalue,
            },
        };
        const long_op = switch (op) {
            .get => switch (scope) {
                .global => Opcode.get_global_long,
                .local => Opcode.get_local_long,
                .upvalue => Opcode.get_upvalue_long,
            },
            .set => switch (scope) {
                .global => Opcode.set_global_long,
                .local => Opcode.set_local_long,
                .upvalue => Opcode.set_upvalue_long,
            },
        };

        if (index <= std.math.maxInt(u8)) {
            var bytes: [2]u8 = .{ @intFromEnum(short_op), @truncate(index) };
            try self.emitBytes(bytes[0..]);
        } else {
            var bytes: [4]u8 = .{ @intFromEnum(long_op), 0, 0, 0 };
            util.fillU24LE(bytes[1..], index);
            try self.emitBytes(bytes[0..]);
        }
    }

    fn expression(self: *Compiler) !void {
        try self.parsePrecedence(.assignment);
    }

    fn parsePrecedence(self: *Compiler, precedence: Compiler.Precedence) !void {
        try self.parser.advance();

        if (getRule(self.parser.previous.?.type).prefix) |prefixFn| {
            const can_assign = @intFromEnum(precedence) <= @intFromEnum(Precedence.assignment);
            try prefixFn(self, can_assign);

            while (@intFromEnum(precedence) <= @intFromEnum(getRule(self.parser.current.?.type).precedence)) {
                try self.parser.advance();
                if (getRule(self.parser.previous.?.type).infix) |infixFn| {
                    try infixFn(self, can_assign);
                }
            }

            if (can_assign and try self.match(.equal)) {
                try debug.errorAt(self.parser.previous.?, "Invalid assignment target.");
            }

            // success: we parsed an expression
            return;
        }

        // no prefix rule: error
        return debug.errorAt(self.parser.previous.?, "Expected expression.");
    }

    fn number(self: *Compiler, can_assign: bool) !void {
        _ = can_assign;
        std.debug.assert(self.parser.previous != null);

        const value = try std.fmt.parseFloat(f64, self.parser.previous.?.lexeme);
        try self.emitConstant(.{ .float = value });
    }

    fn call(self: *Compiler, can_assign: bool) !void {
        _ = can_assign;
        const arg_count = try self.argumentList();
        try self.emitBytes(&.{ @intFromEnum(Opcode.call), arg_count });
    }

    fn argumentList(self: *Compiler) !u8 {
        var arg_count: u8 = 0;
        if (!self.check(.right_paren)) while (true) {
            try self.expression();
            arg_count += 1;
            if (arg_count == 255) try debug.errorAt(self.parser.previous.?, "Can't have more than 255 arguments.");
            if (!try self.match(.comma)) break;
        };

        try self.parser.consume(.right_paren, "Expected ')' after arguments.");
        return arg_count;
    }

    fn grouping(self: *Compiler, can_assign: bool) !void {
        _ = can_assign;
        try self.expression();
        try self.parser.consume(.right_paren, "Expected ')' after expression.");
    }

    fn unary(self: *Compiler, can_assign: bool) !void {
        _ = can_assign;
        std.debug.assert(self.parser.previous != null);

        const op_type = self.parser.previous.?.type;
        try self.parsePrecedence(.unary);

        switch (op_type) {
            .minus => try self.emitBytes(&.{@intFromEnum(Opcode.negate)}),
            .bang => try self.emitBytes(&.{@intFromEnum(Opcode.not)}),
            else => @panic("Called unary on operator other than minus."),
        }
    }

    fn binary(self: *Compiler, can_assign: bool) !void {
        _ = can_assign;
        std.debug.assert(self.parser.previous != null);
        const op_type = self.parser.previous.?.type;

        try self.parsePrecedence(@enumFromInt(@intFromEnum(getRule(op_type).precedence) + 1));

        switch (op_type) {
            .plus => try self.emitBytes(&.{@intFromEnum(Opcode.add)}),
            .minus => try self.emitBytes(&.{@intFromEnum(Opcode.subtract)}),
            .star => try self.emitBytes(&.{@intFromEnum(Opcode.multiply)}),
            .slash => try self.emitBytes(&.{@intFromEnum(Opcode.divide)}),
            .equal_equal => try self.emitBytes(&.{@intFromEnum(Opcode.equal)}),
            .greater => try self.emitBytes(&.{@intFromEnum(Opcode.greater)}),
            .less => try self.emitBytes(&.{@intFromEnum(Opcode.less)}),
            .bang_equal => try self.emitBytes(&.{ @intFromEnum(Opcode.equal), @intFromEnum(Opcode.not) }),
            .greater_equal => try self.emitBytes(&.{ @intFromEnum(Opcode.less), @intFromEnum(Opcode.not) }),
            .less_equal => try self.emitBytes(&.{ @intFromEnum(Opcode.greater), @intFromEnum(Opcode.not) }),
            else => @panic("Calling binary on operator that doesn't support it."),
        }
    }

    fn literal(self: *Compiler, can_assign: bool) !void {
        _ = can_assign;
        std.debug.assert(self.parser.previous != null);
        const op_type = self.parser.previous.?.type;

        switch (op_type) {
            .false => try self.emitBytes(&.{@intFromEnum(Opcode.false)}),
            .true => try self.emitBytes(&.{@intFromEnum(Opcode.true)}),
            .nil => try self.emitBytes(&.{@intFromEnum(Opcode.nil)}),
            else => @panic("Literal expression expected."),
        }
    }

    fn string(self: *Compiler, can_assign: bool) !void {
        _ = can_assign;
        const allocator = self.allocator;
        try self.emitConstant(.{
            .obj = try Value.Obj.allocString(allocator, self.objects, self.parser.previous.?.lexeme),
        });
    }

    fn variable(self: *Compiler, can_assign: bool) !void {
        try self.namedVariable(self.parser.previous.?, can_assign);
    }

    fn @"and"(self: *Compiler, can_assign: bool) !void {
        _ = can_assign;
        const endJump = try self.emitPlaceholderJump(.jump_if_false);

        try self.emitBytes(&.{@intFromEnum(Opcode.pop)});
        try self.parsePrecedence(.@"and");

        try self.backpatchJumpOp(endJump);
    }

    fn @"or"(self: *Compiler, can_assign: bool) !void {
        _ = can_assign;
        const elseJump = try self.emitPlaceholderJump(.jump_if_false);
        const endJump = try self.emitPlaceholderJump(.jump);

        try self.backpatchJumpOp(elseJump);
        try self.emitBytes(&.{@intFromEnum(Opcode.pop)});

        try self.parsePrecedence(.@"or");
        try self.backpatchJumpOp(endJump);
    }

    fn namedVariable(self: *Compiler, name: Token, can_assign: bool) !void {
        const allocator = self.allocator;

        if (try self.current_function.resolveLocal(name)) |local| {
            if (try self.match(.equal) and can_assign) {
                try self.expression();
                try self.emitVariableOp(local, .set, .local);
            } else {
                try self.emitVariableOp(local, .get, .local);
            }
        } else if (try self.current_function.resolveUpvalue(allocator, name)) |upvalue| {
            if (try self.match(.equal) and can_assign) {
                try self.expression();
                try self.emitVariableOp(upvalue, .set, .upvalue);
            } else {
                try self.emitVariableOp(upvalue, .get, .upvalue);
            }
        } else {
            const global = try self.identifierConstant(name);

            if (try self.match(.equal) and can_assign) {
                try self.expression();
                try self.emitVariableOp(global, .set, .global);
            } else {
                try self.emitVariableOp(global, .get, .global);
            }
        }
    }

    fn getRule(tok_type: Token.Type) Parser.Rule {
        return switch (tok_type) {
            .left_paren => .{ .prefix = Compiler.grouping, .infix = Compiler.call, .precedence = .call },
            .minus => .{ .prefix = Compiler.unary, .infix = Compiler.binary, .precedence = .term },
            .plus => .{ .prefix = null, .infix = Compiler.binary, .precedence = .term },
            .slash => .{ .prefix = null, .infix = Compiler.binary, .precedence = .factor },
            .star => .{ .prefix = null, .infix = Compiler.binary, .precedence = .factor },
            .number => .{ .prefix = Compiler.number, .infix = null, .precedence = .none },
            .true => .{ .prefix = Compiler.literal, .infix = null, .precedence = .none },
            .false => .{ .prefix = Compiler.literal, .infix = null, .precedence = .none },
            .nil => .{ .prefix = Compiler.literal, .infix = null, .precedence = .none },
            .bang => .{ .prefix = Compiler.unary, .infix = null, .precedence = .none },
            .bang_equal => .{ .prefix = null, .infix = Compiler.binary, .precedence = .equality },
            .equal_equal => .{ .prefix = null, .infix = Compiler.binary, .precedence = .equality },
            .greater => .{ .prefix = null, .infix = Compiler.binary, .precedence = .comparison },
            .greater_equal => .{ .prefix = null, .infix = Compiler.binary, .precedence = .comparison },
            .less => .{ .prefix = null, .infix = Compiler.binary, .precedence = .comparison },
            .less_equal => .{ .prefix = null, .infix = Compiler.binary, .precedence = .comparison },
            .string => .{ .prefix = Compiler.string, .infix = null, .precedence = .none },
            .identifier => .{ .prefix = Compiler.variable, .infix = null, .precedence = .none },
            .@"and" => .{ .prefix = null, .infix = Compiler.@"and", .precedence = .@"and" },
            .@"or" => .{ .prefix = null, .infix = Compiler.@"or", .precedence = .@"or" },
            else => .{ .prefix = null, .infix = null, .precedence = .none },
        };
    }

    pub fn hexdumpCode(self: *Compiler, filename: []const u8) !void {
        const file = try std.fs.cwd().createFile(filename, .{});
        defer file.close();

        const bytecode_slice = self.currentChunk().code.items;

        try file.writeAll(bytecode_slice);
    }
};
