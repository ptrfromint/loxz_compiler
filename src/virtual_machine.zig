const std = @import("std");
const bytecode = @import("bytecode.zig");
const util = @import("util.zig");
const Color = @import("debug.zig").Color;
const Chunk = bytecode.Chunk;
const Opcode = bytecode.Opcode;
const Compiler = @import("compiler.zig").Compiler;
const Value = @import("value.zig").Value;
const CallFrame = @import("compiler.zig").CallFrame;

pub const InterpreterError = error{ CompilationError, RuntimeError, OutOfMemory };

pub const VirtualMachine = struct {
    call_stack: std.ArrayList(CallFrame),
    stack: std.ArrayList(Value),
    globals: std.StringHashMap(Value),
    arena: std.heap.ArenaAllocator,

    pub fn init(allocator: std.mem.Allocator) VirtualMachine {
        return .{
            .call_stack = .empty,
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

        try self.defineNativeFunction("clock", nativeClock);

        var compiler: Compiler = try .init(allocator, source);
        const func = compiler.compile() catch return InterpreterError.CompilationError;

        const main_closure = try Value.Obj.allocClosure(allocator, func);
        try self.stack.append(allocator, .{ .obj = main_closure });
        try self.callValue(main_closure, 0);

        try self.run();
    }

    pub fn callValue(
        self: *VirtualMachine,
        callee: *Value.Obj,
        arg_count: usize,
    ) !void {
        const allocator = self.arena.allocator();

        switch (callee.*) {
            .closure => return try self.call(callee, arg_count),
            .native_function => {
                const fun = callee.native_function.function;
                const result = fun(arg_count, self.stack.items[self.stack.items.len - arg_count - 1 ..]);

                // Pop the arguments off the stack
                for (0..arg_count) |_| _ = self.stack.pop();

                // FIX: Pop the native function itself off the stack as well
                _ = self.stack.pop();

                try self.stack.append(allocator, result);
            },
            else => return self.runtimeError("Can only call functions(closures) and classes.", .{}),
        }
    }

    pub fn call(self: *VirtualMachine, closure_obj: *Value.Obj, arg_count: usize) !void {
        const function = closure_obj.closure.function.function;

        if (arg_count != function.arity) {
            return self.runtimeError("Expected {} arguments, but got {}", .{ function.arity, arg_count });
        }

        if (self.call_stack.items.len > std.math.maxInt(u8) - 1) {
            return self.runtimeError("Stack overflow.", .{});
        }

        const allocator = self.arena.allocator();
        try self.call_stack.append(allocator, .{
            .closure = closure_obj,
            .ip = 0,
            .slot_start = self.stack.items.len - arg_count - 1,
        });
    }

    pub fn run(self: *VirtualMachine) !void {
        const allocator = self.arena.allocator();

        // Load the current frame
        var frame = &self.call_stack.items[self.call_stack.items.len - 1];
        var chunk = &frame.closure.closure.function.function.chunk;
        var ip = frame.ip;

        while (true) {
            if (ip >= chunk.code.items.len) break;

            const instr: Opcode = @enumFromInt(chunk.code.items[ip]);
            ip += 1;

            switch (instr) {
                .@"return" => {
                    const result = self.stack.pop();

                    // FIX: Capture the slot_start of the function we are LEAVING
                    const close_up = frame.slot_start;

                    _ = self.call_stack.pop();

                    if (self.call_stack.items.len == 0) {
                        _ = self.stack.pop();
                        return;
                    }

                    // Restore previous frame
                    frame = &self.call_stack.items[self.call_stack.items.len - 1];
                    chunk = &frame.closure.closure.function.function.chunk;
                    ip = frame.ip;

                    // FIX: Truncate stack to the start of the function we just LEFT
                    self.stack.items.len = close_up;
                    try self.stack.append(allocator, result.?);
                },
                .pop => {
                    _ = self.stack.pop();
                },
                .print => {
                    if (self.stack.pop()) |val| {
                        std.debug.print("{f}\n", .{val});
                    } else return self.runtimeError("'print' called with no values in the stack.", .{});
                },
                .negate => {
                    const value = self.stack.items[self.stack.items.len - 1];

                    switch (value) {
                        .float => |val| self.stack.items[self.stack.items.len - 1] = .{ .float = -val },
                        else => return self.runtimeError("'negate' called on an invalid operand.", .{}),
                    }
                },
                .not => {
                    const value = self.stack.items[self.stack.items.len - 1];

                    switch (value) {
                        .boolean => |val| self.stack.items[self.stack.items.len - 1] = .{ .boolean = !val },
                        .nil => self.stack.items[self.stack.items.len - 1] = .{ .boolean = true },
                        else => return self.runtimeError("'not' called on an invalid operand.", .{}),
                    }
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
                            .obj => |obj_a| switch (obj_a.*) {
                                .string => |str_a| switch (instr) {
                                    .add => {
                                        try self.stack.append(allocator, .{
                                            .obj = try concatStringWithNum(allocator, str_a, val_a),
                                        });
                                    },
                                    .multiply => {
                                        if (val_a == 0) @panic("Can't multiply strings by zero!");
                                        const buf_len = str_a.str.len * @as(usize, @intFromFloat(@round(val_a)));
                                        const buffer = try allocator.alloc(u8, buf_len);
                                        for (0..buffer.len) |i| {
                                            buffer[i] = str_a.str[i % str_a.str.len];
                                        }

                                        try self.stack.append(allocator, .{
                                            .obj = try Value.Obj.allocString(allocator, buffer),
                                        });
                                    },
                                    else => return self.runtimeError("Can only add/multiply string with number", .{}),
                                },
                                else => return self.runtimeError("Binary operation called on number and non-number/string", .{}),
                            },
                            else => return self.runtimeError("Binary operation called on number and non-number.", .{}),
                        },
                        .obj => |obj_a| switch (b.?) {
                            .obj => |obj_b| switch (instr) {
                                .add => {
                                    switch (obj_a.*) {
                                        .string => |str_a| switch (obj_b.*) {
                                            .string => |str_b| {
                                                const new_str = try concatStrings(allocator, str_a, str_b);
                                                try self.stack.append(allocator, .{ .obj = new_str });
                                            },
                                            else => return self.runtimeError("Tried to add string and {s}", .{@tagName(obj_b.*)}),
                                        },
                                        else => return self.runtimeError("Operands must be two numbers or two strings.", .{}),
                                    }
                                },
                                else => return self.runtimeError("Non-add operation called on two strings.", .{}),
                            },
                            .float => |number| switch (instr) {
                                .multiply => {
                                    switch (obj_a.*) {
                                        .string => |str_a| {
                                            if (number == 0) @panic("Can't multiply strings by zero!");
                                            const buf_len = str_a.str.len * @as(usize, @intFromFloat(@round(number)));
                                            const buffer = try allocator.alloc(u8, buf_len);
                                            for (0..buffer.len) |i| {
                                                buffer[i] = str_a.str[i % str_a.str.len];
                                            }

                                            try self.stack.append(allocator, .{
                                                .obj = try Value.Obj.allocString(allocator, buffer),
                                            });
                                        },
                                        else => return self.runtimeError("Can't multiply non-string object.", .{}),
                                    }
                                },
                                .add => switch (obj_a.*) {
                                    .string => |str_a| {
                                        try self.stack.append(allocator, .{
                                            .obj = try concatStringWithNum(allocator, str_a, number),
                                        });
                                    },
                                    else => return self.runtimeError("Can't add string and {s}", .{@tagName(obj_a.*)}),
                                },
                                else => return self.runtimeError("Can't multiply/add {s} with {s}.", .{ @tagName(obj_a.*), @tagName(b.?) }),
                            },
                            else => return self.runtimeError("Binary operation called on string and non-string", .{}),
                        },
                        else => return self.runtimeError("Binary opeartion called on non-number or non-string.", .{}),
                    }
                },
                .constant => {
                    const const_index: usize = @intCast(chunk.code.items[ip]);
                    const constant = chunk.constants.items[const_index];
                    try self.stack.append(allocator, constant);
                    ip += 1;
                },
                .constant_long => {
                    const const_index = util.readU24LE(chunk.code.items[ip .. ip + 3]);
                    const constant = chunk.constants.items[const_index];
                    try self.stack.append(allocator, constant);
                    ip += 3;
                },
                .make_global, .get_global, .set_global, .make_global_long, .get_global_long, .set_global_long => {
                    const index: usize = switch (instr) {
                        .make_global, .get_global, .set_global => @intCast(chunk.code.items[ip]),
                        else => util.readU24LE(chunk.code.items[ip .. ip + 3]),
                    };
                    const global = chunk.constants.items[index];

                    const name = switch (global.obj.*) {
                        .string => |s| s.str,
                        else => unreachable,
                    };

                    switch (instr) {
                        .make_global, .make_global_long => {
                            if (self.stack.pop()) |val| {
                                try self.globals.put(name, val);
                            } else return self.runtimeError("Called make global with no value in the stack for it.", .{});
                        },
                        else => {
                            if (!self.globals.contains(name)) {
                                return self.runtimeError("Undefined variable \"{s}\".", .{name});
                            }

                            switch (instr) {
                                .get_global, .get_global_long => try self.stack.append(allocator, self.globals.get(name).?),
                                else => try self.globals.put(name, self.stack.getLast()),
                            }
                        },
                    }

                    switch (instr) {
                        .get_global, .set_global, .make_global => ip += 1,
                        else => ip += 3,
                    }
                },
                .get_local, .get_local_long, .set_local, .set_local_long => {
                    const index: usize = switch (instr) {
                        .get_local, .set_local => @intCast(chunk.code.items[ip]),
                        else => util.readU24LE(chunk.code.items[ip .. ip + 3]),
                    };

                    // Offset by the frame pointer
                    const slot = frame.slot_start + index;

                    switch (instr) {
                        .get_local, .get_local_long => try self.stack.append(allocator, self.stack.items[slot]),
                        else => self.stack.items[slot] = self.stack.getLast(),
                    }

                    switch (instr) {
                        .get_local, .set_local => ip += 1,
                        else => ip += 3,
                    }
                },
                .jump_if_false => {
                    const offset = util.readU16LE(chunk.code.items[ip .. ip + 2]);
                    const value = self.stack.getLast();
                    const should_jump = switch (value) {
                        .nil => true,
                        .boolean => |b| !b,
                        else => false,
                    };

                    if (should_jump) {
                        ip += 2 + offset;
                    } else {
                        ip += 2;
                    }
                },
                .jump => {
                    const offset = util.readU16LE(chunk.code.items[ip .. ip + 2]);
                    ip += 2 + offset;
                },
                .loop => {
                    const offset = util.readU16LE(chunk.code.items[ip .. ip + 2]);
                    ip = (ip + 2) - offset;
                },
                .call => {
                    const arg_count: usize = @intCast(chunk.code.items[ip]);

                    self.call_stack.items[self.call_stack.items.len - 1].ip = ip + 1;

                    const callee = self.stack.items[self.stack.items.len - 1 - arg_count];
                    try self.callValue(callee.obj, arg_count);

                    frame = &self.call_stack.items[self.call_stack.items.len - 1];
                    chunk = &frame.closure.closure.function.function.chunk;
                    ip = frame.ip;
                },
                .closure => {
                    const index: usize = @intCast(chunk.code.items[ip]);
                    const func = chunk.constants.items[index];
                    const closure = try switch (func) {
                        .obj => |obj| switch (obj.*) {
                            .function => |_| try Value.Obj.allocClosure(allocator, obj),
                            else => self.runtimeError("Closure called non-function ({s})", .{@tagName(obj.*)}),
                        },
                        else => self.runtimeError("Closure called on non-obj ({s})", .{@tagName(func)}),
                    };

                    try self.stack.append(allocator, .{ .obj = closure });
                    ip += 1;
                },
                .false => {
                    try self.stack.append(allocator, .{ .boolean = false });
                },
                .true => {
                    try self.stack.append(allocator, .{ .boolean = true });
                },
                .nil => {
                    try self.stack.append(allocator, .{ .nil = {} });
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
                                    else => try self.stack.append(allocator, .{ .boolean = false }),
                                },
                                else => try self.stack.append(allocator, .{ .boolean = false }),
                            },
                            else => try self.stack.append(allocator, .{ .boolean = false }),
                        },
                    }
                },
            }

            // Sync IP back to frame
            frame.ip = ip;
        }
    }

    fn concatStrings(allocator: std.mem.Allocator, str_a: Value.Obj.String, str_b: Value.Obj.String) !*Value.Obj {
        const new_str = try std.mem.concat(allocator, u8, &.{ str_a.str, str_b.str });
        defer allocator.free(new_str);

        return try Value.Obj.allocString(allocator, new_str);
    }

    fn concatStringWithNum(allocator: std.mem.Allocator, str: Value.Obj.String, num: f64) !*Value.Obj {
        const new_str = try std.fmt.allocPrint(allocator, "{s}{}", .{ str.str, num });
        const obj = try Value.Obj.allocString(allocator, new_str);
        return obj;
    }

    pub fn runtimeError(self: *VirtualMachine, comptime format: []const u8, args: anytype) InterpreterError {
        std.debug.print("{s}", .{Color.Red});
        std.debug.print(format, args);
        std.debug.print("{s}\n", .{Color.Reset});

        // Print the Stack Trace
        var i: usize = self.call_stack.items.len;
        while (i > 0) {
            i -= 1;
            const frame = &self.call_stack.items[i];
            const function = frame.closure;
            const instruction = frame.ip - 1;

            // We use LineRun, so we need to decode it
            const chunk = &function.function.chunk;
            var line: usize = 0;
            var offset = instruction;

            for (chunk.lines.items) |line_run| {
                if (offset < line_run.count) {
                    line = line_run.line;
                    break;
                }
                offset -= line_run.count;
            }

            std.debug.print("[line {}] in ", .{line});

            if (function.function.name) |name| {
                std.debug.print("{s}()\n", .{name.string.str});
            } else {
                std.debug.print("script\n", .{});
            }
        }

        self.resetStack();
        return InterpreterError.RuntimeError;
    }

    fn resetStack(self: *VirtualMachine) void {
        self.stack.clearRetainingCapacity();
        self.call_stack.clearRetainingCapacity();
    }

    fn defineNativeFunction(
        self: *VirtualMachine,
        name: []const u8,
        func: Value.Obj.NativeFunction.Ptr,
    ) !void {
        const allocator = self.arena.allocator();
        const native_fn = try Value.Obj.allocNativeFn(allocator, func);

        try self.globals.put(name, .{ .obj = native_fn });
    }
};

fn nativeClock(arg_count: usize, args: []Value) Value {
    _ = arg_count;
    _ = args;

    return .{ .float = @floatFromInt(std.time.milliTimestamp()) };
}
