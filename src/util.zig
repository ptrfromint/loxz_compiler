const std = @import("std");

/// Fill `buf[0..3]` with the little-endian 24-bit representation of `v`.
pub fn fillU24LE(buf: []u8, v: usize) void {
    std.debug.assert(buf.len >= 3);
    buf[0] = @truncate(v & 0xFF);
    buf[1] = @truncate((v >> 8) & 0xFF);
    buf[2] = @truncate((v >> 16) & 0xFF);
}

/// Read a little-endian 24-bit value from `buf[0..3)` and return it as usize.
pub fn readU24LE(buf: []const u8) usize {
    std.debug.assert(buf.len >= 3);
    return ((@as(usize, @intCast(buf[0])) |
        (@as(usize, @intCast(buf[1])) << 8) |
        (@as(usize, @intCast(buf[2])) << 16)));
}

/// Fill `buf[0..4]` with the little-endian 32-bit representation of `v`.
pub fn fillU32LE(buf: []u8, v: u32) void {
    std.debug.assert(buf.len >= 4);
    buf[0] = @truncate(v & 0xFF);
    buf[1] = @truncate((v >> 8) & 0xFF);
    buf[2] = @truncate((v >> 16) & 0xFF);
    buf[3] = @truncate((v >> 24) & 0xFF);
}

/// Read a little-endian 32-bit value from `buf[0..4)` and return it as u32.
pub fn readU32LE(buf: []const u8) u32 {
    std.debug.assert(buf.len >= 4);
    return ((@as(u32, @intCast(buf[0]))) |
        (@as(u32, @intCast(buf[1])) << 8) |
        (@as(u32, @intCast(buf[2])) << 16) |
        (@as(u32, @intCast(buf[3])) << 24));
}
