const std = @import("std");
const debug = @import("debug.zig");
const Token = @import("scanner.zig").Token;
const Scanner = @import("scanner.zig").Scanner;
const Compiler = @import("compiler.zig").Compiler;

pub const Parser = struct {
    current: ?Token,
    previous: ?Token,
    scanner: *Scanner,

    pub const Rule = struct {
        prefix: ?*const fn (compiler: *Compiler) error{ InvalidCharacter, OutOfMemory }!void,
        infix: ?*const fn (compiler: *Compiler) error{ InvalidCharacter, OutOfMemory }!void,
        precedence: Compiler.Precedence,
    };

    pub fn init(scanner: *Scanner) Parser {
        return .{
            .current = null,
            .previous = null,
            .scanner = scanner,
        };
    }

    pub fn advance(self: *Parser) void {
        self.previous = self.current;

        while (true) {
            self.current = self.scanner.scanToken();
            if (self.current) |tok| switch (tok.type) {
                .@"error" => debug.errorAt(tok, tok.lexeme),
                else => break,
            };
        }
    }

    pub fn consume(self: *Parser, tok_type: Token.Type, msg: []const u8) void {
        if (self.current.?.type == tok_type) {
            return self.advance();
        }

        return debug.errorAt(self.current.?, msg);
    }
};
