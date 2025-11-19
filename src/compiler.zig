const std = @import("std");

const Parser = @import("parser.zig").Parser;
const Value = @import("value.zig").Value;

const Scanner = @import("scanner.zig").Scanner;
const Token = @import("scanner.zig").Token;

const bytecode = @import("bytecode.zig");
const Chunk = bytecode.Chunk;
const Opcode = bytecode.Opcode;

const debug = @import("debug.zig");
const utils = @import("util.zig");

const CompilerErrorSet = error{ CompilationError, OutOfMemory, InvalidCharacter };

pub const Scope = struct {
    pub const Local = struct {
        name: Token,
        depth: ?usize,
    };

    locals: std.ArrayList(Local),
    depth: usize,

    pub const empty_global: Scope = .{
        .locals = .empty,
        .depth = 0,
    };
};

pub const Compiler = struct {
    arena: std.heap.ArenaAllocator,
    scanner: *Scanner,
    parser: *Parser,
    current_chunk: *Chunk,
    current_scope: *Scope,

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

    pub fn init(allocator: std.mem.Allocator, source: []const u8, chunk: *Chunk) !Compiler {
        var arena: std.heap.ArenaAllocator = .init(allocator);
        const alloc = arena.allocator();

        const scanner = try alloc.create(Scanner);
        scanner.* = .init(alloc, source);

        const parser = try alloc.create(Parser);
        parser.* = .init(scanner);

        const scope = try alloc.create(Scope);
        scope.* = .empty_global;

        return .{
            .arena = arena,
            .scanner = scanner,
            .parser = parser,
            .current_chunk = chunk,
            .current_scope = scope,
        };
    }

    pub fn deinit(self: *Compiler) void {
        self.scanner.deinit();
        self.arena.allocator().destroy(self.scanner);

        self.arena.allocator().destroy(self.parser);
        self.arena.deinit();
    }

    pub fn compile(self: *Compiler) !void {
        try self.parser.advance();

        while (!try self.match(.eof)) try self.declaration();

        try self.endCompiler();
    }

    pub fn emitBytes(self: *Compiler, bytes: []const u8) !void {
        std.debug.assert(self.parser.previous != null);
        try self.currentChunk().appendBytes(bytes, self.parser.previous.?.line);
    }

    pub fn emitConstant(self: *Compiler, value: Value) !void {
        try self.currentChunk().addConst(value, self.parser.previous.?.line);
    }

    fn currentChunk(self: *Compiler) *Chunk {
        return self.current_chunk;
    }

    fn endCompiler(self: *Compiler) !void {
        try self.emitBytes(&.{@intFromEnum(Opcode.@"return")});
    }

    fn declaration(self: *Compiler) CompilerErrorSet!void {
        if (try self.match(.@"var")) {
            try self.varDeclaration();
        } else {
            try self.statement();
        }
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

    fn statement(self: *Compiler) !void {
        if (try self.match(.print)) {
            try self.printStatement();
        } else if (try self.match(.left_brace)) {
            try self.startScope();
            try self.block();
            try self.endScope();
        } else {
            try self.expressionStatement();
        }
    }

    fn startScope(self: *Compiler) !void {
        self.current_scope.depth += 1;
    }

    fn endScope(self: *Compiler) !void {
        self.current_scope.depth -= 1;

        for (self.current_scope.locals.items) |local| {
            if (local.depth.? > self.current_scope.depth) {
                _ = self.current_scope.locals.pop();
                try self.emitBytes(&.{@intFromEnum(Opcode.pop)});
            }
        }
    }

    fn block(self: *Compiler) !void {
        while (!self.check(.right_brace) and !self.check(.eof)) {
            try self.declaration();
        }

        try self.parser.consume(.right_brace, "Expected '}' after block.");
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
        const allocator = self.arena.allocator();
        const obj = try allocator.create(Value.Obj);
        obj.* = .{ .string = .{ .str = try allocator.dupe(u8, name.lexeme) } };
        return try self.currentChunk().addConstant(.{ .obj = obj });
    }

    fn parseVariable(self: *Compiler, err_msg: []const u8) !usize {
        try self.parser.consume(.identifier, err_msg);

        try self.declareVariable();
        if (self.current_scope.depth > 0) return 0;

        return self.identifierConstant(self.parser.previous.?);
    }

    fn declareVariable(self: *Compiler) !void {
        // Global variables are implicitly declared already
        if (self.current_scope.depth == 0) return;

        if (self.current_scope.locals.items.len < 1) return try self.addLocal(self.parser.previous.?);

        const var_name = self.parser.previous.?;
        var i: usize = self.current_scope.locals.items.len - 1;
        while (i >= 0) : (i -= 1) {
            const local = self.current_scope.locals.items[i];
            if (local.depth != null and local.depth.? < self.current_scope.depth) {
                break;
            }

            if (std.mem.eql(u8, var_name.lexeme, local.name.lexeme)) {
                try debug.errorAt(var_name, "A variable with this name is already declared.");
            }
        }

        try self.addLocal(self.parser.previous.?);
    }

    fn defineVariable(self: *Compiler, global: usize) !void {
        if (self.current_scope.depth > 0) {
            return self.markInitialized();
        }

        if (global <= std.math.maxInt(u8)) {
            var bytes: [2]u8 = .{ @intFromEnum(Opcode.make_global), @truncate(global) };
            try self.emitBytes(bytes[0..]);
        } else {
            var bytes: [4]u8 = .{ @intFromEnum(Opcode.make_global_long), 0, 0, 0 };
            utils.fillU24LE(bytes[1..], global);
            try self.emitBytes(bytes[0..]);
        }
    }

    fn addLocal(self: *Compiler, name: Token) !void {
        const allocator = self.arena.allocator();

        try self.current_scope.locals.append(allocator, .{
            .name = name,
            .depth = null,
        });
    }

    fn markInitialized(self: *Compiler) !void {
        if (self.current_scope.locals.items.len < 1) return;
        const last_index = self.current_scope.locals.items.len - 1;
        self.current_scope.locals.items[last_index].depth = self.current_scope.depth;
    }

    fn emitVariableOp(
        self: *Compiler,
        index: usize,
        comptime op: enum { set, get },
        comptime scope: enum { global, local },
    ) !void {
        const short_op = switch (op) {
            .get => switch (scope) {
                .global => Opcode.get_global,
                .local => Opcode.get_local,
            },
            .set => switch (scope) {
                .global => Opcode.set_global,
                .local => Opcode.set_local,
            },
        };
        const long_op = switch (op) {
            .get => switch (scope) {
                .global => Opcode.get_global_long,
                .local => Opcode.get_local_long,
            },
            .set => switch (scope) {
                .global => Opcode.set_global_long,
                .local => Opcode.set_local_long,
            },
        };

        if (index <= std.math.maxInt(u8)) {
            var bytes: [2]u8 = .{ @intFromEnum(short_op), @truncate(index) };
            try self.emitBytes(bytes[0..]);
        } else {
            var bytes: [4]u8 = .{ @intFromEnum(long_op), 0, 0, 0 };
            utils.fillU24LE(bytes[1..], index);
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
        const allocator = self.arena.allocator();
        const obj = try allocator.create(Value.Obj);
        obj.* = .{ .string = .{ .str = try allocator.dupe(u8, self.parser.previous.?.lexeme) } };
        try self.emitConstant(.{ .obj = obj });
    }

    fn variable(self: *Compiler, can_assign: bool) !void {
        try self.namedVariable(self.parser.previous.?, can_assign);
    }

    fn namedVariable(self: *Compiler, name: Token, can_assign: bool) !void {
        if (try self.resolveLocal(name)) |local| {
            if (try self.match(.equal) and can_assign) {
                try self.expression();

                std.debug.print("Here\n", .{});
                try self.emitVariableOp(local, .set, .local);
            } else {
                try self.emitVariableOp(local, .get, .local);
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

    fn resolveLocal(self: *Compiler, name: Token) !?usize {
        if (self.current_scope.locals.items.len < 1) return null;

        var i = self.current_scope.locals.items.len - 1;

        while (i >= 0) : (i -= 1) {
            const local = self.current_scope.locals.items[i];
            if (std.mem.eql(u8, local.name.lexeme, name.lexeme)) {
                if (local.depth == null) {
                    try debug.errorAt(local.name, "Can't read local variable in it's own initializer.");
                }
                return i;
            }

            if (i == 0) return null;
        }

        return null;
    }

    fn getRule(tok_type: Token.Type) Parser.Rule {
        return switch (tok_type) {
            .left_paren => .{ .prefix = Compiler.grouping, .infix = null, .precedence = .none },
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
            else => .{ .prefix = null, .infix = null, .precedence = .none },
        };
    }
};
