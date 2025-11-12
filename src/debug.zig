const std = @import("std");
const Chunk = @import("bytecode.zig").Chunk;
const Opcode = @import("bytecode.zig").Opcode;
const Token = @import("scanner.zig").Token;

fn printInstruction(chunk: *const Chunk, offset: usize) !usize {
    const line = chunk.getLine(offset);

    std.debug.print("{:0>8}|", .{offset});
    if (offset > 0 and line == chunk.getLine(offset - 1)) {
        std.debug.print("{s: <6}| ", .{"^"});
    } else {
        std.debug.print("{: <6}| ", .{line});
    }

    const code_len = chunk.code.items.len;
    if (offset >= code_len) @panic("offset out of bounds");

    const op_byte = chunk.code.items[offset];
    const op: Opcode = @enumFromInt(op_byte);

    switch (op) {
        .constant => {
            if (offset + 1 >= code_len) @panic("truncated constant");
            const index = chunk.code.items[offset + 1];
            const val = chunk.constants.items[@intCast(index)];
            std.debug.print("{s}, index: {}, '{f}'\n", .{ @tagName(op), index, val });
            return offset + 2;
        },
        .constant_long => {
            if (offset + 3 >= code_len) @panic("truncated constant_long");
            const b0 = chunk.code.items[offset + 1];
            const b1 = chunk.code.items[offset + 2];
            const b2 = chunk.code.items[offset + 3];
            const index = (@as(usize, b0)) | ((@as(usize, b1) << 8)) | ((@as(usize, b2) << 16));
            const val = chunk.constants.items[index];
            std.debug.print("{s}, index: {}, '{f}'\n", .{ @tagName(op), index, val });
            return offset + 4;
        },
        else => {
            std.debug.print("{s}\n", .{@tagName(op)});
            return offset + 1;
        },
    }
}

/// High-level disassembly of a bytecode chunk
pub fn printChunk(chunk: *const Chunk, name: []const u8) void {
    std.debug.print("{s}\n", .{name});
    std.debug.print(" offset | line | additional information \n", .{});
    std.debug.print("----------------------------------------\n", .{});

    var offset: usize = 0;
    while (offset < chunk.code.items.len) {
        const next = printInstruction(chunk, offset) catch @panic("printInstruction failed");
        offset = next;
    }
}

pub fn errorAt(token: Token, msg: []const u8) void {
    std.debug.print("[line {}] Error", .{token.line});

    switch (token.type) {
        .eof => std.debug.print(" at end", .{}),
        .@"error" => {},
        else => std.debug.print(" at '{s}'", .{token.lexeme}),
    }

    std.debug.print(": {s}\n", .{msg});
}
