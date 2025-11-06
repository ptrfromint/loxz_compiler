const scanner = @import("scanner.zig");
const Token = scanner.Token;
const Scanner = scanner.Scanner;

pub const Parser = struct {
    current: Token,
    previous: Token,

    pub fn advance(self: *Parser, scanner: *Scanner) void {
        self.previous = self.current;

        while (true) {
            self.current = scanner.scanToken();
            switch (self.current.type) {
                .@"error" => errorAtCurrent(self.current.lexeme),
            }
        }
    }
};
