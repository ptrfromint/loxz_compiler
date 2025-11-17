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

pub const Compiler = struct {
    arena: std.heap.ArenaAllocator,
    scanner: *Scanner,
    parser: *Parser,
    current_chunk: *Chunk,

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

        return .{
            .arena = arena,
            .scanner = scanner,
            .parser = parser,
            .current_chunk = chunk,
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

    fn declaration(self: *Compiler) !void {
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
        } else {
            try self.expressionStatement();
        }
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
        return self.identifierConstant(self.parser.previous.?);
    }

    fn defineVariable(self: *Compiler, global: usize) !void {
        if (global <= std.math.maxInt(u8)) {
            var bytes: [2]u8 = .{ @intFromEnum(Opcode.make_global), @truncate(global) };
            try self.emitBytes(bytes[0..]);
        } else {
            var bytes: [4]u8 = .{ @intFromEnum(Opcode.make_global_long), 0, 0, 0 };
            utils.fillU24LE(bytes[1..], global);
            try self.emitBytes(bytes[0..]);
        }
    }

    fn emitGetVariable(self: *Compiler, global: usize) !void {
        if (global <= std.math.maxInt(u8)) {
            var bytes: [2]u8 = .{ @intFromEnum(Opcode.get_global), @truncate(global) };
            try self.emitBytes(bytes[0..]);
        } else {
            var bytes: [4]u8 = .{ @intFromEnum(Opcode.get_global_long), 0, 0, 0 };
            utils.fillU24LE(bytes[1..], global);
            try self.emitBytes(bytes[0..]);
        }
    }

    fn emitSetVariable(self: *Compiler, global: usize) !void {
        if (global <= std.math.maxInt(u8)) {
            var bytes: [2]u8 = .{ @intFromEnum(Opcode.set_global), @truncate(global) };
            try self.emitBytes(bytes[0..]);
        } else {
            var bytes: [4]u8 = .{ @intFromEnum(Opcode.set_global_long), 0, 0, 0 };
            utils.fillU24LE(bytes[1..], global);
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
        const global = try self.identifierConstant(name);

        if (try self.match(.equal) and can_assign) {
            try self.expression();
            try self.emitSetVariable(global);
        } else {
            try self.emitGetVariable(global);
        }
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
