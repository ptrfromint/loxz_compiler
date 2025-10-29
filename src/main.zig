const std = @import("std");
const Chunk = @import("bytecode.zig").Chunk;
const debug = @import("debug.zig");

pub fn main() !void {
    var da: std.heap.DebugAllocator(.{}) = .{};
    defer std.debug.assert(da.deinit() == .ok);
    const allocator = da.allocator();

    var chunk: Chunk = .empty;
    defer chunk.deinit(allocator);

    try chunk.addOpcode(allocator, .@"return", 123);
    for (0..300) |i| {
        try chunk.addConst(allocator, .{ .float = @floatFromInt(i) }, @intCast(i / 10 + 10));
    }

    debug.printChunk(&chunk, "test chunk");

    var serialized = try chunk.serialize(allocator);
    defer serialized.deinit(allocator);
    std.debug.print("Serialized chunk size: {} bytes\n", .{serialized.items.len});
}
