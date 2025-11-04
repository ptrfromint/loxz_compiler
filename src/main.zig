const std = @import("std");
const Chunk = @import("bytecode.zig").Chunk;
const VirtualMachine = @import("virtual_machine.zig").VirtualMachine;
const debug = @import("debug.zig");

pub fn main() !void {
    var da: std.heap.DebugAllocator(.{}) = .{};
    defer std.debug.assert(da.deinit() == .ok);
    const allocator = da.allocator();

    var chunk: Chunk = .empty;
    defer chunk.deinit(allocator);

    try chunk.addConst(allocator, .{ .float = 6.7 }, 123);
    try chunk.addOpcode(allocator, .negate, 123);
    try chunk.addConst(allocator, .{ .float = 6.7 }, 123);
    try chunk.addOpcode(allocator, .multiply, 123);
    try chunk.addOpcode(allocator, .@"return", 123);

    var vm: VirtualMachine = .init(&chunk, allocator);
    defer vm.deinit();

    try vm.run();
}
