const std = @import("std");

const Chunk = @import("bytecode.zig").Chunk;
const Compiler = @import("compiler.zig").Compiler;
const Opcode = @import("bytecode.zig").Opcode;
const Token = @import("scanner.zig").Token;
const util = @import("util.zig");
const Value = @import("value.zig").Value;

// ANSI Color codes for terminal output
pub const Color = struct {
    pub const Reset = "\x1b[0m";
    pub const Bold = "\x1b[1m";
    pub const Dim = "\x1b[2m";
    pub const Red = "\x1b[31m";
    pub const Green = "\x1b[32m";
    pub const Yellow = "\x1b[33m";
    pub const Blue = "\x1b[34m";
    pub const Magenta = "\x1b[35m";
    pub const Cyan = "\x1b[36m";
    pub const White = "\x1b[37m";
};

/// Helper to print values with syntax highlighting
fn printValue(value: Value) void {
    switch (value) {
        .float => |v| std.debug.print("{s}{d}{s}", .{ Color.Yellow, v, Color.Reset }),
        .boolean => |v| std.debug.print("{s}{}{s}", .{ Color.Magenta, v, Color.Reset }),
        .nil => std.debug.print("{s}nil{s}", .{ Color.Red, Color.Reset }),
        .obj => |o| switch (o.kind) {
            .string => |s| std.debug.print("{s}\"{s}\"{s}", .{ Color.Green, s.str, Color.Reset }),
            else => std.debug.print("{s}{f}{s}", .{ Color.Green, o.kind, Color.Reset }),
        },
    }
}

fn printInstruction(chunk: *const Chunk, offset: usize) !usize {
    const code_len = chunk.code.items.len;
    if (offset >= code_len) @panic("Offset out of bounds");

    const op_byte = chunk.code.items[offset];
    const op: Opcode = @enumFromInt(op_byte);

    // 1. Print Offset (Dimmed)
    std.debug.print("{s}{:0>4}  ", .{ Color.Dim, offset });

    // 2. Print Line Number
    const line = chunk.getLine(offset);
    if (offset > 0 and line == chunk.getLine(offset - 1)) {
        std.debug.print("   |  ", .{});
    } else {
        std.debug.print("{s}{: >4}{s}  ", .{ Color.Reset ++ Color.Bold ++ Color.Magenta, line, Color.Dim });
    }

    // 3. Print Opcode Name (Bold White)
    std.debug.print("{s}{s}{s: <16}{s}", .{ Color.Reset, Color.Bold, @tagName(op), Color.Reset });

    // 4. Print Operands based on Opcode
    switch (op) {
        .constant, .make_global, .get_global, .set_global => {
            if (offset + 1 >= code_len) @panic("truncated instruction");
            const index = chunk.code.items[offset + 1];
            const val = chunk.constants.items[@intCast(index)];

            // Print index in Cyan, Value in its own color
            std.debug.print("{s}[{d}]{s} ", .{ Color.Cyan, index, Color.Reset });
            printValue(val);
            std.debug.print("\n", .{});
            return offset + 2;
        },
        .constant_long, .make_global_long, .get_global_long, .set_global_long => {
            if (offset + 3 >= code_len) @panic("truncated instruction");
            const index = util.readU24LE(chunk.code.items[offset + 1 .. offset + 4]);
            const val = chunk.constants.items[index];

            std.debug.print("{s}[{d}]{s} ", .{ Color.Cyan, index, Color.Reset });
            printValue(val);
            std.debug.print("\n", .{});
            return offset + 4;
        },
        .get_local, .set_local => {
            if (offset + 1 >= code_len) @panic("truncated instruction");
            const index = chunk.code.items[offset + 1];
            std.debug.print("{s}slot {d}{s}\n", .{ Color.Magenta, index, Color.Reset });
            return offset + 2;
        },
        .class, .get_property, .set_property => {
            if (offset + 1 >= code_len) @panic("truncated instruction");
            const index = chunk.code.items[offset + 1];
            const val = chunk.constants.items[@intCast(index)];

            std.debug.print("{s}[{d}]{s} ", .{ Color.Cyan, index, Color.Reset });
            printValue(val);
            std.debug.print("\n", .{});
            return offset + 2;
        },
        .get_local_long, .set_local_long => {
            if (offset + 3 >= code_len) @panic("truncated instruction");
            const index = util.readU24LE(chunk.code.items[offset + 1 .. offset + 4]);
            std.debug.print("{s}slot {d}{s}\n", .{ Color.Magenta, index, Color.Reset });
            return offset + 4;
        },
        .call => {
            if (offset + 1 >= code_len) @panic("truncated instruction");
            const arg_count = chunk.code.items[offset + 1];
            std.debug.print("{d} args\n", .{arg_count});
            return offset + 2;
        },
        .closure => {
            if (offset + 1 >= code_len) @panic("truncated instruction");
            const index = chunk.code.items[offset + 1];
            const val = chunk.constants.items[index];

            std.debug.print("{s}[{d}]{s} ", .{ Color.Cyan, index, Color.Reset });
            printValue(val);
            std.debug.print("\n", .{});

            const function = val.obj.kind.function;
            var i: usize = 0;
            var current_offset = offset + 2;
            while (i < function.upvalue_count) : (i += 1) {
                const is_local = chunk.code.items[current_offset];
                const upvalue_index = chunk.code.items[current_offset + 1];
                std.debug.print("{:0>4}      |                     {s} {d}\n", .{
                    current_offset,
                    if (is_local == 1) "local" else "upvalue",
                    upvalue_index,
                });
                current_offset += 2;
            }
            return current_offset;
        },
        .jump, .jump_if_false, .loop => {
            if (offset + 2 >= code_len) @panic("truncated instruction");
            const jump = util.readU16LE(chunk.code.items[offset + 1 .. offset + 3]);
            const next_instr_offset = offset + 3;

            const target: isize = if (op == .loop)
                @as(isize, @intCast(next_instr_offset)) - @as(isize, @intCast(jump))
            else
                @as(isize, @intCast(next_instr_offset)) + @as(isize, @intCast(jump));

            // Print Jump Target
            std.debug.print("{s}{d} -> {d}{s}\n", .{ Color.Yellow, offset, target, Color.Reset });
            return offset + 3;
        },
        else => {
            std.debug.print("\n", .{});
            return offset + 1;
        },
    }
}

/// High-level disassembly of a bytecode chunk
pub fn printChunk(chunk: *const Chunk, name: []const u8) void {
    std.debug.print("{s}╭──────────────────────────────────────────────────────╮{s}\n", .{ Color.Dim, Color.Reset });
    std.debug.print("{s}│ Disassembly: {s}{s}{s: <40}{s}│{s}\n", .{ Color.Dim, Color.Reset, Color.Bold, name, Color.Dim, Color.Reset });
    std.debug.print("{s}├──────┬──────┬──────────────────┬─────────────────────┤{s}\n", .{ Color.Dim, Color.Reset });
    std.debug.print("{s}│ ADDR │ LINE │ OPCODE           │ OPERANDS            │{s}\n", .{ Color.Dim, Color.Reset });
    std.debug.print("{s}╰──────┴──────┴──────────────────┴─────────────────────╯{s}\n", .{ Color.Dim, Color.Reset });

    var offset: usize = 0;
    while (offset < chunk.code.items.len) {
        const next = printInstruction(chunk, offset) catch @panic("printInstruction failed");
        offset = next;
    }
    std.debug.print("\n", .{});
}

pub fn errorAt(token: Token, msg: []const u8) !void {
    std.debug.print("{s}[line {}] Error{s}", .{ Color.Red, token.line, Color.Reset });

    switch (token.type) {
        .eof => std.debug.print(" at end", .{}),
        .@"error" => {},
        else => std.debug.print(" at '{s}{s}{s}'", .{ Color.Bold, token.lexeme, Color.Reset }),
    }

    std.debug.print(": {s}{s}{s}\n", .{ Color.Red, msg, Color.Reset });

    return Compiler.Error.CompilationError;
}
