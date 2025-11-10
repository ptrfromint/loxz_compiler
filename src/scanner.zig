const std = @import("std");

pub const Token = struct {
    type: Token.Type,
    lexeme: []const u8,
    line: usize,

    pub const Type = enum {
        left_paren,
        right_paren,
        left_brace,
        right_brace,
        comma,
        dot,
        minus,
        plus,
        semicolon,
        slash,
        star,

        bang,
        bang_equal,
        equal,
        equal_equal,
        greater,
        greater_equal,
        less,
        less_equal,

        identifier,
        string,
        number,

        @"if",
        @"else",
        @"and",
        @"or",
        @"for",
        @"while",
        @"var",
        @"return",
        true,
        false,
        class,
        fun,
        nil,
        print,
        super,
        this,

        @"error",
        eof,

        pub const KEYWORDS_START: comptime_int = @intFromEnum(Token.Type.@"if");
        pub const KEYWORDS_END: comptime_int = @intFromEnum(Token.Type.this);
    };

    pub fn errorToken(msg: []const u8, line: usize) Token {
        return .{
            .type = .@"error",
            .lexeme = msg,
            .line = line,
        };
    }

    pub fn make(tok_type: Token.Type, lexeme: []const u8, line: usize) Token {
        return .{
            .type = tok_type,
            .lexeme = lexeme,
            .line = line,
        };
    }
};

pub const Scanner = struct {
    start: usize,
    current: usize,
    line: usize,
    source: []const u8,
    arena: std.heap.ArenaAllocator,

    pub fn init(allocator: std.mem.Allocator, source: []const u8) Scanner {
        return .{
            .arena = .init(allocator),
            .source = source,
            .start = 0,
            .current = 0,
            .line = 1,
        };
    }

    pub fn deinit(self: *Scanner) void {
        self.arena.deinit();
    }

    pub fn scan(self: *Scanner) void {
        var last_line: usize = 0;

        while (true) {
            const token = self.scanToken();

            if (token.line != last_line) {
                std.debug.print("{:0>4} ", .{token.line});
                last_line = token.line;
            } else {
                std.debug.print("{s: >4} ", .{"^"});
            }

            std.debug.print("{s} {s}\n", .{ @tagName(token.type), token.lexeme });

            switch (token.type) {
                .eof => break,
                else => continue,
            }
        }
    }

    pub fn scanToken(self: *Scanner) Token {
        while (true) {
            if (self.current >= self.source.len) return .make(.eof, self.source[self.start..self.current], self.line);

            switch (self.source[self.current]) {
                ' ', '\r', '\t' => _ = self.advance(),
                '\n' => {
                    self.line += 1;
                    _ = self.advance();
                },
                '/' => if (self.peekNext()) |val| {
                    if (val == '/') {
                        while (self.current < self.source.len and self.source[self.current] != '\n') _ = self.advance();
                    } else break;
                } else break,
                else => break,
            }
        }

        if (self.current >= self.source.len) return .make(.eof, self.source[self.start..self.current], self.line);

        self.start = self.current;

        switch (self.advance()) {
            '(' => return .make(.left_paren, self.source[self.start..self.current], self.line),
            ')' => return .make(.right_paren, self.source[self.start..self.current], self.line),
            '{' => return .make(.left_brace, self.source[self.start..self.current], self.line),
            '}' => return .make(.right_brace, self.source[self.start..self.current], self.line),
            ';' => return .make(.semicolon, self.source[self.start..self.current], self.line),
            ',' => return .make(.comma, self.source[self.start..self.current], self.line),
            '.' => return .make(.dot, self.source[self.start..self.current], self.line),
            '-' => return .make(.minus, self.source[self.start..self.current], self.line),
            '+' => return .make(.plus, self.source[self.start..self.current], self.line),
            '/' => return .make(.slash, self.source[self.start..self.current], self.line),
            '*' => return .make(.star, self.source[self.start..self.current], self.line),
            '!' => if (self.match('=')) {
                return .make(.bang_equal, self.source[self.start..self.current], self.line);
            } else {
                return .make(.bang, self.source[self.start..self.current], self.line);
            },
            '=' => if (self.match('=')) {
                return .make(.equal_equal, self.source[self.start..self.current], self.line);
            } else {
                return .make(.equal, self.source[self.start..self.current], self.line);
            },
            '>' => if (self.match('=')) {
                return .make(.greater_equal, self.source[self.start..self.current], self.line);
            } else {
                return .make(.greater, self.source[self.start..self.current], self.line);
            },
            '<' => if (self.match('=')) {
                return .make(.less_equal, self.source[self.start..self.current], self.line);
            } else {
                return .make(.less, self.source[self.start..self.current], self.line);
            },
            '"' => {
                while (self.current < self.source.len and self.source[self.current] != '"') {
                    if (self.source[self.current] == '\n') self.line += 1;
                    _ = self.advance();
                }

                if (self.current >= self.source.len) return .errorToken("Unterminated string", self.line);

                _ = self.advance(); // Drop the closing quote
                return .make(.string, self.source[self.start..self.current], self.line);
            },
            '0'...'9' => {
                while (std.ascii.isDigit(self.source[self.current])) _ = self.advance();

                if (self.peekNext()) |next| {
                    if (self.source[self.current] == '.' and std.ascii.isDigit(next)) {
                        _ = self.advance(); // Drop the dot

                        while (std.ascii.isDigit(self.source[self.current])) _ = self.advance();
                    }
                }

                return .make(.number, self.source[self.start..self.current], self.line);
            },
            'a'...'z', 'A'...'Z', '_' => {
                while (std.ascii.isAlphanumeric(self.source[self.current]) or
                    self.source[self.current] == '_') _ = self.advance();

                const lexeme = self.source[self.start..self.current];
                inline for (@typeInfo(Token.Type).@"enum".fields) |field| {
                    if (field.value >= Token.Type.KEYWORDS_START and field.value <= Token.Type.KEYWORDS_END) {
                        if (std.mem.eql(u8, lexeme, field.name)) {
                            return .make(@enumFromInt(field.value), lexeme, self.line);
                        }
                    }
                }

                return .make(.identifier, lexeme, self.line);
            },
            else => unreachable,
        }

        return .errorToken("Unexpected charachter", self.line);
    }

    fn advance(self: *Scanner) u8 {
        self.current += 1;
        return self.source[self.current - 1];
    }

    fn match(self: *Scanner, expected: u8) bool {
        if (self.current >= self.source.len) return false;
        if (self.source[self.current] != expected) return false;

        self.current += 1;
        return true;
    }

    fn peekNext(self: *Scanner) ?u8 {
        if (self.current + 1 >= self.source.len) return null;
        return self.source[self.current + 1];
    }
};
