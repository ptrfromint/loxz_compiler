const std = @import("std");

const bit_utils = @import("util.zig");
const Value = @import("value.zig").Value;

pub const Opcode = enum(u8) {
    @"return",
    negate,
    add,
    subtract,
    divide,
    multiply,
    constant,
    constant_long,
    nil,
    false,
    true,
    not,
    equal,
    greater,
    less,
    print,
    pop,
    make_global,
    make_global_long,
    get_global,
    get_global_long,
    set_global,
    set_global_long,
    get_local,
    get_local_long,
    set_local,
    set_local_long,
    get_upvalue,
    get_upvalue_long,
    set_upvalue,
    set_upvalue_long,
    close_upvalue,
    jump_if_false,
    jump,
    loop,
    call,
    closure,
    class,
};

pub const LineRun = struct {
    line: usize,
    count: usize,
};

pub const Chunk = struct {
    code: std.ArrayList(u8),
    constants: std.ArrayList(Value),
    lines: std.ArrayList(LineRun),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) Chunk {
        return .{
            .code = .empty,
            .constants = .empty,
            .lines = .empty,
            .allocator = allocator,
        };
    }

    pub fn addConstant(self: *Chunk, value: Value) !usize {
        std.debug.assert(self.constants.items.len < std.math.maxInt(u24));
        try self.constants.append(self.allocator, value);
        return self.constants.items.len - 1;
    }

    /// Append raw bytes to code and update RLE runs (counts are the same as bytes)
    pub fn appendBytes(self: *Chunk, bytes: []const u8, line: usize) !void {
        for (bytes) |b| try self.code.append(self.allocator, b);

        // add to the last run if on the same line; else start a new run
        if (self.lines.items.len == 0) {
            try self.lines.append(self.allocator, .{ .line = line, .count = bytes.len });
        } else {
            var last = &self.lines.items[self.lines.items.len - 1];
            if (last.line == line) {
                last.count += bytes.len;
            } else {
                try self.lines.append(self.allocator, .{ .line = line, .count = bytes.len });
            }
        }
    }

    pub fn addOpcode(self: *Chunk, op: Opcode, line: usize) !void {
        const b: [1]u8 = .{@intFromEnum(op)};
        try self.appendBytes(b[0..], line);
    }

    pub fn addConst(self: *Chunk, value: Value, line: usize) !void {
        const index = try self.addConstant(value);

        if (index <= std.math.maxInt(u8)) {
            var bytes: [2]u8 = .{ @intFromEnum(Opcode.constant), @truncate(index) };
            try self.appendBytes(bytes[0..], line);
        } else {
            const int_val: usize = index;
            var bytes: [4]u8 = .{ @intFromEnum(Opcode.constant_long), 0, 0, 0 };
            bit_utils.fillU24LE(bytes[1..], int_val);
            try self.appendBytes(bytes[0..], line);
        }
    }

    /// Returns the source line for the given byte offset into `code`
    pub fn getLine(self: *const Chunk, byte_offset: usize) usize {
        std.debug.assert(byte_offset < self.code.items.len);

        var acc: usize = 0;
        var i: usize = 0;
        while (i < self.lines.items.len) : (i += 1) {
            const run = self.lines.items[i];
            if (byte_offset < acc + run.count) return run.line;
            acc += run.count;
        }

        return 0;
    }

    pub fn serialize(self: *const Chunk) !std.ArrayList(u8) {
        // A simple serialization:
        // 1: [code_len: u32 (little_endian)]
        // 2: [code_bytes: []u8]
        // 3: [runs_len: u32 (little_endian)]
        // 4: [for each run: { line: u32(little), count: u32(little)}]
        var out: std.ArrayList(u8) = .empty;

        // Write code_len
        const code_len_u32: u32 = @truncate(self.code.items.len);
        var tmp: [4]u8 = undefined;
        bit_utils.fillU32LE(tmp[0..], code_len_u32);
        for (tmp) |b| try out.append(self.allocator, b);

        // Write code bytes
        for (self.code.items) |b| try out.append(self.allocator, b);

        // Write runs_len
        const runs_len_u32: u32 = @truncate(self.lines.items.len);
        bit_utils.fillU32LE(tmp[0..], runs_len_u32);
        for (tmp) |b| try out.append(self.allocator, b);

        // Write for each run -> u32 line, u32 count (both little-endian)
        var buf: [4]u8 = undefined;
        for (self.lines.items) |run| {
            const line_u32: u32 = @truncate(run.line);
            bit_utils.fillU32LE(buf[0..], line_u32);
            for (buf) |b| try out.append(self.allocator, b);

            const count_u32: u32 = @truncate(run.count);
            bit_utils.fillU32LE(buf[0..], count_u32);
            for (buf) |b| try out.append(self.allocator, b);
        }

        return out;
    }

    pub fn deinit(self: *Chunk) void {
        self.code.deinit(self.allocator);
        self.constants.deinit(self.allocator);
        self.lines.deinit(self.allocator);
    }
};
