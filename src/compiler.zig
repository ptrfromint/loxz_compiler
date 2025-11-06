const std = @import("std");
const Scanner = @import("scanner.zig").Scanner;
const Chunk = @import("bytecode.zig").Chunk;

pub const Compiler = struct {
    arena: std.heap.ArenaAllocator,

    pub fn init(allocator: std.mem.Allocator) Compiler {
        return .{
            .arena = .init(allocator),
        };
    }

    pub fn compile(self: *Compiler, source: []const u8, chunk: *Chunk) void {
        const allocator = self.arena.allocator();

        var scanner: Scanner = .init(allocator, source);
        scanner.scan();
    }
};
