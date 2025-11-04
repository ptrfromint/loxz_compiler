const std = @import("std");
const bytecode = @import("bytecode.zig");
const util = @import("util.zig");
const Chunk = bytecode.Chunk;
const Opcode = bytecode.Opcode;
const Value = @import("value.zig").Value;

pub const InterpreterError = error{ CompilationError, RuntimeError };

pub const VirtualMachine = struct {
    chunk: *Chunk,
    ip: usize,
    stack: std.ArrayList(Value),
    arena: std.heap.ArenaAllocator,

    pub fn init(chunk: *Chunk, allocator: std.mem.Allocator) VirtualMachine {
        return .{
            .chunk = chunk,
            .ip = 0,
            .stack = .empty,
            .arena = .init(allocator),
        };
    }

    pub fn deinit(self: *VirtualMachine) void {
        self.arena.deinit();
    }

    pub fn run(self: *VirtualMachine) !void {
        const allocator = self.arena.allocator();

        while (true) : (self.ip += 1) {
            const instr: Opcode = @enumFromInt(self.chunk.code.items[self.ip]);
            switch (instr) {
                .@"return" => {
                    if (self.stack.pop()) |val| {
                        std.debug.print("Returned: {f}\n", .{val});
                    } else @panic("Called return with no values in stack!");
                    return;
                },
                .negate => {
                    const val: Value = .{ .float = -self.stack.items[self.stack.items.len - 1].float };
                    self.stack.items[self.stack.items.len - 1] = val;
                },
                .add, .subtract, .multiply, .divide => {
                    const a = self.stack.pop();
                    const b = self.stack.pop();

                    switch (a.?) {
                        .float => |val_a| switch (b.?) {
                            .float => |val_b| try self.stack.append(allocator, .{
                                .float = switch (instr) {
                                    .add => val_a + val_b,
                                    .subtract => val_a - val_b,
                                    .multiply => val_a * val_b,
                                    .divide => if (val_b == 0) @panic("Can't divide by zero") else val_a / val_b,
                                    else => unreachable,
                                },
                            }),
                        },
                    }
                },
                .constant => {
                    const const_index: usize = @intCast(self.chunk.code.items[self.ip + 1]);
                    const constant = self.chunk.constants.items[const_index];
                    try self.stack.append(allocator, constant);

                    self.ip += 1;
                },
                .constant_long => {
                    const long_constant = self.chunk.constants.items[self.ip];
                    try self.stack.append(allocator, long_constant);

                    self.ip += 3;
                },
            }
        }
    }
};
