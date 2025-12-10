const std = @import("std");
const Alignment = std.mem.Alignment;

pub const GarbageCollector = struct {
    inner_alloc: std.mem.Allocator,

    pub fn allocator(self: *GarbageCollector) std.mem.Allocator {
        return .{
            .ptr = self,
            .vtable = &.{
                .alloc = alloc,
                .resize = resize,
                .remap = remap,
                .free = free,
            },
        };
    }

    fn alloc(ctx: *anyopaque, len: usize, alignment: Alignment, ret_addr: usize) ?[*]u8 {
        _ = len; // autofix
        _ = alignment; // autofix
        _ = ret_addr; // autofix
        const gc: *GarbageCollector = @ptrCast(@alignCast(ctx));
        _ = gc; // autofix
    }

    fn resize(ctx: *anyopaque, memory: []u8, alignment: Alignment, new_len: usize, ret_addr: usize) bool {
        _ = memory; // autofix
        _ = alignment; // autofix
        _ = new_len; // autofix
        _ = ret_addr; // autofix
        const gc: *GarbageCollector = @ptrCast(@alignCast(ctx));
        _ = gc; // autofix
    }

    fn remap(ctx: *anyopaque, memory: []u8, alignment: Alignment, new_len: usize, ret_addr: usize) ?[*]u8 {
        _ = memory; // autofix
        _ = alignment; // autofix
        _ = new_len; // autofix
        _ = ret_addr; // autofix
        const gc: *GarbageCollector = @ptrCast(@alignCast(ctx));
        _ = gc; // autofix
    }

    fn free(ctx: *anyopaque, memory: []u8, alignment: Alignment, ret_addr: usize) void {
        _ = memory; // autofix
        _ = alignment; // autofix
        _ = ret_addr; // autofix
        const gc: *GarbageCollector = @ptrCast(@alignCast(ctx));
        _ = gc; // autofix
    }
};
