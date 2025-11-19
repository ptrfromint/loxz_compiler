const std = @import("std");
const bytecode = @import("bytecode.zig");
const util = @import("util.zig");
const Chunk = bytecode.Chunk;
const Opcode = bytecode.Opcode;
const Compiler = @import("compiler.zig").Compiler;
const Value = @import("value.zig").Value;

pub const InterpreterError = error{ CompilationError, RuntimeError };

pub const VirtualMachine = struct {
    chunk: *Chunk,
    ip: usize,
    stack: std.ArrayList(Value),
    globals: std.StringHashMap(Value),
    arena: std.heap.ArenaAllocator,

    pub fn init(chunk: *Chunk, allocator: std.mem.Allocator) VirtualMachine {
        return .{
            .chunk = chunk,
            .ip = 0,
            .stack = .empty,
            .globals = .init(allocator),
            .arena = .init(allocator),
        };
    }

    pub fn deinit(self: *VirtualMachine) void {
        self.globals.deinit();
        self.arena.deinit();
    }

    pub fn interpret(self: *VirtualMachine, source: []const u8) InterpreterError!void {
        const allocator = self.arena.allocator();

        var chunk: Chunk = .empty;
        defer chunk.deinit(allocator);

        var compiler: Compiler = .init(allocator);
        compiler.compile(source, &chunk) catch return InterpreterError.CompilationError;

        self.chunk = chunk;
        self.ip = 0;

        try self.run();
    }

    pub fn run(self: *VirtualMachine) !void {
        const allocator = self.arena.allocator();

        while (self.ip < self.chunk.code.items.len) {
            const instr: Opcode = @enumFromInt(self.chunk.code.items[self.ip]);
            switch (instr) {
                .@"return" => {
                    return;
                },
                .pop => {
                    _ = self.stack.pop();
                    self.ip += 1;
                },
                .print => {
                    if (self.stack.pop()) |val| {
                        std.debug.print("{f}\n", .{val});
                    } else @panic("'print' called with no values in the stack.");
                    self.ip += 1;
                },
                .negate => {
                    const value = self.stack.items[self.stack.items.len - 1];

                    switch (value) {
                        .float => |val| self.stack.items[self.stack.items.len - 1] = .{ .float = -val },
                        else => @panic("'negate' called on an invalid operand."),
                    }
                    self.ip += 1;
                },
                .not => {
                    const value = self.stack.items[self.stack.items.len - 1];

                    switch (value) {
                        .boolean => |val| self.stack.items[self.stack.items.len - 1] = .{ .boolean = !val },
                        .nil => self.stack.items[self.stack.items.len - 1] = .{ .boolean = true },
                        else => @panic("'not' called on an invalid operand."),
                    }
                    self.ip += 1;
                },
                .add, .subtract, .multiply, .divide, .less, .greater => {
                    const b = self.stack.pop();
                    const a = self.stack.pop();

                    switch (a.?) {
                        .float => |val_a| switch (b.?) {
                            .float => |val_b| switch (instr) {
                                .add => try self.stack.append(allocator, .{ .float = val_a + val_b }),
                                .subtract => try self.stack.append(allocator, .{ .float = val_a - val_b }),
                                .multiply => try self.stack.append(allocator, .{ .float = val_a * val_b }),
                                .divide => if (val_b == 0) @panic("Can't divide by zero") else {
                                    try self.stack.append(allocator, .{ .float = val_a / val_b });
                                },
                                .less => try self.stack.append(allocator, .{ .boolean = val_a < val_b }),
                                .greater => try self.stack.append(allocator, .{ .boolean = val_a > val_b }),
                                else => unreachable,
                            },
                            else => @panic("Binary operation called on number and non-number."),
                        },
                        .obj => |obj_a| switch (b.?) {
                            .obj => |obj_b| switch (instr) {
                                .add => {
                                    const new_obj = try concatStrings(allocator, obj_a, obj_b);
                                    try self.stack.append(allocator, .{ .obj = new_obj });
                                },
                                else => @panic("Non-add operation called on two strings."),
                            },
                            .float => |multiplier| switch (instr) {
                                .multiply => {
                                    if (multiplier == 0) @panic("Can't multiply strings by zero!");

                                    const buf_len = obj_a.string.str.len * @as(usize, @intFromFloat(@round(multiplier)));
                                    const buffer = try allocator.alloc(u8, buf_len);
                                    for (0..buffer.len) |i| {
                                        buffer[i] = obj_a.string.str[i % obj_a.string.str.len];
                                    }
                                    const obj = try allocator.create(Value.Obj);
                                    obj.* = .{ .string = .{ .str = buffer } };
                                    try self.stack.append(allocator, .{ .obj = obj });
                                },
                                else => @panic("Can't multiply string with anything other than a number."),
                            },
                            else => @panic("Binary operation called on string and non-string"),
                        },
                        else => @panic("Binary opeartion called on non-number or non-string."),
                    }

                    self.ip += 1;
                },
                .constant => {
                    const const_index: usize = @intCast(self.chunk.code.items[self.ip + 1]);
                    const constant = self.chunk.constants.items[const_index];
                    try self.stack.append(allocator, constant);

                    self.ip += 2;
                },
                .constant_long => {
                    const const_index = util.readU24LE(self.chunk.code.items[self.ip + 1 .. self.ip + 4]);
                    const constant = self.chunk.constants.items[const_index];
                    try self.stack.append(allocator, constant);

                    self.ip += 4;
                },
                .make_global, .get_global, .set_global, .make_global_long, .get_global_long, .set_global_long => {
                    const index: usize = switch (instr) {
                        .make_global, .get_global, .set_global => @intCast(self.chunk.code.items[self.ip + 1]),
                        else => util.readU24LE(self.chunk.code.items[self.ip + 1 .. self.ip + 4]),
                    };
                    const global = self.chunk.constants.items[index];
                    const name = global.obj.string.str;

                    switch (instr) {
                        .make_global, .make_global_long => {
                            if (self.stack.pop()) |val| {
                                try self.globals.put(name, val);
                            } else @panic("Called make global with no value in the stack for it.");
                        },
                        else => {
                            if (!self.globals.contains(name)) {
                                std.debug.print("Undefined variable \"{s}\".", .{name});
                                return InterpreterError.RuntimeError;
                            }

                            switch (instr) {
                                .get_global, .get_global_long => try self.stack.append(allocator, self.globals.get(name).?),
                                else => try self.globals.put(global.obj.string.str, self.stack.getLast()),
                            }
                        },
                    }

                    switch (instr) {
                        .get_global, .set_global, .make_global => self.ip += 2,
                        else => self.ip += 4,
                    }
                },
                .get_local, .get_local_long, .set_local, .set_local_long => {
                    const index: usize = switch (instr) {
                        .get_local, .set_local => @intCast(self.chunk.code.items[self.ip + 1]),
                        else => util.readU24LE(self.chunk.code.items[self.ip + 1 .. self.ip + 4]),
                    };

                    switch (instr) {
                        .get_local, .get_local_long => try self.stack.append(allocator, self.stack.items[index]),
                        else => self.stack.items[index] = self.stack.getLast(),
                    }

                    switch (instr) {
                        .get_local, .set_local => self.ip += 2,
                        else => self.ip += 4,
                    }
                },
                .jump_if_false => {
                    const offset = util.readU16LE(self.chunk.code.items[self.ip + 1 .. self.ip + 3]);
                    const value = self.stack.getLast();
                    const should_jump = switch (value) {
                        .nil => true,
                        .boolean => |b| !b,
                        else => false,
                    };
                    if (should_jump) {
                        self.ip += 3 + offset;
                    } else {
                        self.ip += 3;
                    }
                },
                .jump => {
                    const offset = util.readU16LE(self.chunk.code.items[self.ip + 1 .. self.ip + 3]);
                    self.ip += 3 + offset;
                },
                .loop => {
                    const offset = util.readU16LE(self.chunk.code.items[self.ip + 1 .. self.ip + 3]);
                    self.ip = (self.ip + 3) - offset;
                },
                .false => {
                    try self.stack.append(allocator, .{ .boolean = false });
                    self.ip += 1;
                },
                .true => {
                    try self.stack.append(allocator, .{ .boolean = true });
                    self.ip += 1;
                },
                .nil => {
                    try self.stack.append(allocator, .{ .nil = {} });
                    self.ip += 1;
                },
                .equal => {
                    const b = self.stack.pop();
                    const a = self.stack.pop();
                    std.debug.assert(a != null and b != null);

                    switch (a.?) {
                        .float => |a_val| switch (b.?) {
                            .float => |b_val| try self.stack.append(allocator, .{ .boolean = a_val == b_val }),
                            else => try self.stack.append(allocator, .{ .boolean = false }),
                        },
                        .boolean => |a_val| switch (b.?) {
                            .boolean => |b_val| try self.stack.append(allocator, .{ .boolean = a_val == b_val }),
                            else => try self.stack.append(allocator, .{ .boolean = false }),
                        },
                        .nil => try self.stack.append(allocator, .{ .boolean = false }),
                        .obj => |obj_a| switch (obj_a.*) {
                            .string => |str_a| switch (b.?) {
                                .obj => |obj_b| switch (obj_b.*) {
                                    .string => |str_b| {
                                        const equal = std.mem.eql(u8, str_a.str, str_b.str);
                                        try self.stack.append(allocator, .{ .boolean = equal });
                                    },
                                },
                                else => @panic("Tried comparing string and non-string."),
                            },
                        },
                    }

                    self.ip += 1;
                },
            }
        }
    }

    fn concatStrings(allocator: std.mem.Allocator, obj_a: *Value.Obj, obj_b: *Value.Obj) !*Value.Obj {
        switch (obj_a.*) {
            .string => |str_a| switch (obj_b.*) {
                .string => |str_b| {
                    const new_str = try std.mem.concat(allocator, u8, &.{ str_a.str, str_b.str });
                    const obj = try allocator.create(Value.Obj);
                    obj.* = .{ .string = .{ .str = new_str } };
                    return obj;
                },
            },
        }

        unreachable;
    }
};
