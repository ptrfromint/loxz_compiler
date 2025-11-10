const std = @import("std");

const Parser = @import("parser.zig").Parser;
const Value = @import("value.zig").Value;

const Scanner = @import("scanner.zig").Scanner;
const Token = @import("scanner.zig").Token;

const bytecode = @import("bytecode.zig");
const Chunk = bytecode.Chunk;
const Opcode = bytecode.Opcode;

const debug = @import("debug.zig");

pub const Compiler = struct {
    arena: std.heap.ArenaAllocator,
    scanner: *Scanner,
    parser: *Parser,
    current_chunk: *Chunk,

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
        self.parser.advance();
        try self.expression();
        self.parser.consume(.eof, "Expected end of expression");
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

    fn expression(self: *Compiler) !void {
        try self.parsePrecedence(.assignment);
    }

    fn parsePrecedence(self: *Compiler, precedence: Compiler.Precedence) !void {
        self.parser.advance();

        if (getRule(self.parser.previous.?.type).prefix) |prefixFn| {
            try prefixFn(self);

            while (@intFromEnum(precedence) <= @intFromEnum(getRule(self.parser.current.?.type).precedence)) {
                self.parser.advance();
                if (getRule(self.parser.previous.?.type).infix) |infixFn| {
                    try infixFn(self);
                }
            }

            // success: we parsed an expression
            return;
        }

        // no prefix rule: error
        return debug.errorAt(self.parser.previous.?, "Expected expression.");
    }

    fn number(self: *Compiler) !void {
        std.debug.assert(self.parser.previous != null);

        const value = try std.fmt.parseFloat(f64, self.parser.previous.?.lexeme);
        try self.emitConstant(.{ .float = value });
    }

    fn grouping(self: *Compiler) !void {
        try self.expression();
        self.parser.consume(.right_paren, "Expected ')' after expression.");
    }

    fn unary(self: *Compiler) !void {
        std.debug.assert(self.parser.previous != null);

        const op_type = self.parser.previous.?.type;
        try self.parsePrecedence(.unary);

        switch (op_type) {
            .minus => try self.emitBytes(&.{@intFromEnum(Opcode.negate)}),
            else => @panic("Called unary on operator other than minus."),
        }
    }

    fn binary(self: *Compiler) !void {
        std.debug.assert(self.parser.previous != null);
        const op_type = self.parser.previous.?.type;

        try self.parsePrecedence(@enumFromInt(@intFromEnum(getRule(op_type).precedence) + 1));

        switch (op_type) {
            .plus => try self.emitBytes(&.{@intFromEnum(Opcode.add)}),
            .minus => try self.emitBytes(&.{@intFromEnum(Opcode.subtract)}),
            .star => try self.emitBytes(&.{@intFromEnum(Opcode.multiply)}),
            .slash => try self.emitBytes(&.{@intFromEnum(Opcode.divide)}),
            else => @panic("Calling binary on operator that doesn't support it."),
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
            else => .{ .prefix = null, .infix = null, .precedence = .none },
        };
    }
};
