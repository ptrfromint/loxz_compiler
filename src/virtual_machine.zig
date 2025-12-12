const std = @import("std");

const bytecode = @import("bytecode.zig");
const Chunk = bytecode.Chunk;
const Opcode = bytecode.Opcode;
const CallFrame = @import("compiler.zig").CallFrame;
const Color = @import("debug.zig").Color;
const Compiler = @import("compiler.zig").Compiler;
const GarbageCollector = @import("garbage_collector.zig").GarbageCollector;
const util = @import("util.zig");
const Value = @import("value.zig").Value;

pub const InterpreterError = error{
    CompilationError,
    RuntimeError,
    OutOfMemory,
};

pub const VirtualMachine = struct {
    call_stack: std.ArrayList(CallFrame),
    stack: std.ArrayList(Value),
    globals: std.StringHashMap(Value),
    open_upvalues: ?*Value.Obj = null,

    /// The head of the linked list of all allocated objects.
    /// The VM owns this list and is responsible for freeing it on deinit.
    objects: ?*Value.Obj = null,

    /// We store the allocator here to use for stack/call_stack growth.
    /// This is typically the GarbageCollector's allocator.
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) VirtualMachine {
        return .{
            .call_stack = .empty,
            .stack = .empty,
            .globals = .init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *VirtualMachine) void {
        self.globals.deinit();
        self.call_stack.deinit(self.allocator);
        self.stack.deinit(self.allocator);

        // Explicitly free all objects tracked by the VM.
        // We walk the linked list and free each object using the allocator.
        var obj = self.objects;
        while (obj) |o| {
            const next = o.next;
            o.free(self.allocator);
            obj = next;
        }
    }

    /// Called by the GarbageCollector to mark all roots reachable from the VM.
    pub fn markRoots(self: *VirtualMachine, gc: *GarbageCollector) !void {
        // Mark values on the Stack
        for (self.stack.items) |val| {
            try gc.markValue(val);
        }

        // Mark Global variables
        var it = self.globals.iterator();
        while (it.next()) |entry| {
            try gc.markValue(entry.value_ptr.*);
        }

        // Mark Call Frames (specifically the closures they are executing)
        for (self.call_stack.items) |frame| {
            try gc.markObject(frame.closure);
        }

        // Mark Open Upvalues (upvalues pointing to stack slots that haven't been closed yet)
        var upvalue = self.open_upvalues;
        while (upvalue) |u| {
            try gc.markObject(u);
            upvalue = u.kind.upvalue.next;
        }
    }

    pub fn interpret(self: *VirtualMachine, source: []const u8) InterpreterError!void {
        const allocator = self.allocator;

        try self.defineNativeFunction("clock", nativeClock);

        // We assume the allocator passed to init() is our GarbageCollector.
        // We register the compiler with it so that objects created during compilation
        // (but not yet assigned to the VM) are not collected.
        const gc: *GarbageCollector = @ptrCast(@alignCast(allocator.ptr));

        var compiler: Compiler = try .init(allocator, &self.objects, source);
        defer compiler.deinit();

        // Register compiler as a source of roots for the GC
        gc.compiler = &compiler;
        defer gc.compiler = null;

        const func = compiler.compile() catch return InterpreterError.CompilationError;

        const main_closure = try Value.Obj.allocClosure(allocator, &self.objects, func);
        try self.stack.append(allocator, .{ .obj = main_closure });
        try self.callValue(main_closure, 0);

        try self.run();
    }

    pub fn callValue(
        self: *VirtualMachine,
        callee: *Value.Obj,
        arg_count: usize,
    ) !void {
        const allocator = self.allocator;

        switch (callee.kind) {
            .closure => return try self.call(callee, arg_count),
            .native_function => {
                const fun = callee.kind.native_function.function;
                const result = fun(arg_count, self.stack.items[self.stack.items.len - arg_count - 1 ..]);

                // Pop the arguments off the stack
                for (0..arg_count) |_| _ = self.stack.pop();

                // Pop the native function itself off the stack as well
                _ = self.stack.pop();

                try self.stack.append(allocator, result);
            },
            .class => {
                const instance = try Value.Obj.allocInstance(allocator, &self.objects, callee);
                self.stack.items[self.stack.items.len - arg_count - 1] = .{ .obj = instance };
            },
            .bound_method => return try self.call(callee.kind.bound_method.method, arg_count),
            else => return self.runtimeError("Can only call functions(closures) and classes.", .{}),
        }
    }

    pub fn call(self: *VirtualMachine, closure_obj: *Value.Obj, arg_count: usize) !void {
        const function = closure_obj.kind.closure.function.kind.function;

        if (arg_count != function.arity) {
            return self.runtimeError("Expected {} arguments, but got {}", .{ function.arity, arg_count });
        }

        if (self.call_stack.items.len > std.math.maxInt(u8) - 1) {
            return self.runtimeError("Stack overflow.", .{});
        }

        const allocator = self.allocator;
        try self.call_stack.append(allocator, .{
            .closure = closure_obj,
            .ip = 0,
            .slot_start = self.stack.items.len - arg_count - 1,
        });
    }

    fn getUpvalue(self: *VirtualMachine, upvalue: *Value.Obj) Value {
        if (upvalue.kind.upvalue.location) |stack_index| {
            return self.stack.items[stack_index];
        } else {
            return upvalue.kind.upvalue.closed;
        }
    }

    fn setUpvalue(self: *VirtualMachine, upvalue: *Value.Obj, new_val: Value) void {
        if (upvalue.kind.upvalue.location) |stack_index| {
            self.stack.items[stack_index] = new_val;
        } else {
            upvalue.kind.upvalue.closed = new_val;
        }
    }

    pub fn run(self: *VirtualMachine) !void {
        const allocator = self.allocator;

        // Load the current frame
        var frame = &self.call_stack.items[self.call_stack.items.len - 1];
        var chunk = &frame.closure.kind.closure.function.kind.function.chunk;
        var ip = frame.ip;

        while (true) {
            if (ip >= chunk.code.items.len) break;

            const instr: Opcode = @enumFromInt(chunk.code.items[ip]);
            ip += 1;

            switch (instr) {
                .@"return" => {
                    const result = self.stack.pop();

                    const close_up = frame.slot_start;
                    self.closeUpvalues(close_up);

                    _ = self.call_stack.pop();

                    if (self.call_stack.items.len == 0) {
                        _ = self.stack.pop();
                        return;
                    }

                    // Restore previous frame
                    frame = &self.call_stack.items[self.call_stack.items.len - 1];
                    chunk = &frame.closure.kind.closure.function.kind.function.chunk;
                    ip = frame.ip;

                    // Truncate stack to the start of the function we just LEFT
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
                            .obj => |obj_a| switch (obj_a.kind) {
                                .string => |str_a| switch (instr) {
                                    .add => {
                                        try self.stack.append(allocator, .{
                                            .obj = try concatStringWithNum(allocator, &self.objects, str_a, val_a),
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
                                            .obj = try Value.Obj.allocString(allocator, &self.objects, buffer),
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
                                    switch (obj_a.kind) {
                                        .string => |str_a| switch (obj_b.kind) {
                                            .string => |str_b| {
                                                const new_str = try concatStrings(allocator, &self.objects, str_a, str_b);
                                                try self.stack.append(allocator, .{ .obj = new_str });
                                            },
                                            else => return self.runtimeError("Tried to add string and {s}", .{@tagName(obj_b.kind)}),
                                        },
                                        else => return self.runtimeError("Operands must be two numbers or two strings.", .{}),
                                    }
                                },
                                else => return self.runtimeError("Non-add operation called on two strings.", .{}),
                            },
                            .float => |number| switch (instr) {
                                .multiply => {
                                    switch (obj_a.kind) {
                                        .string => |str_a| {
                                            if (number == 0) @panic("Can't multiply strings by zero!");
                                            const buf_len = str_a.str.len * @as(usize, @intFromFloat(@round(number)));
                                            const buffer = try allocator.alloc(u8, buf_len);
                                            for (0..buffer.len) |i| {
                                                buffer[i] = str_a.str[i % str_a.str.len];
                                            }

                                            try self.stack.append(allocator, .{
                                                .obj = try Value.Obj.allocString(allocator, &self.objects, buffer),
                                            });
                                        },
                                        else => return self.runtimeError("Can't multiply number with {s}.", .{@tagName(obj_a.kind)}),
                                    }
                                },
                                .add => switch (obj_a.kind) {
                                    .string => |str_a| {
                                        try self.stack.append(allocator, .{
                                            .obj = try concatStringWithNum(allocator, &self.objects, str_a, number),
                                        });
                                    },
                                    else => return self.runtimeError("Can't add string and {s}", .{@tagName(obj_a.kind)}),
                                },
                                else => return self.runtimeError("Can't multiply/add {s} with {s}.", .{ @tagName(obj_a.kind), @tagName(b.?) }),
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

                    const name = switch (global.obj.kind) {
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
                .get_upvalue, .set_upvalue, .get_upvalue_long, .set_upvalue_long => {
                    const index: usize = switch (instr) {
                        .get_upvalue, .set_upvalue => @intCast(chunk.code.items[ip]),
                        else => util.readU24LE(chunk.code.items[ip .. ip + 3]),
                    };

                    const upvalue = frame.closure.kind.closure.upvalues.items[index];

                    if (instr == .get_upvalue or instr == .get_upvalue_long) {
                        const val = self.getUpvalue(upvalue);
                        try self.stack.append(allocator, val);
                    } else {
                        const val = self.stack.getLast();
                        self.setUpvalue(upvalue, val);
                    }

                    switch (instr) {
                        .get_upvalue, .set_upvalue => ip += 1,
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
                    switch (callee) {
                        .obj => |obj| try self.callValue(obj, arg_count),
                        else => return self.runtimeError("Can only call functions! Can't call a {s}", .{@tagName(callee)}),
                    }

                    frame = &self.call_stack.items[self.call_stack.items.len - 1];
                    chunk = &frame.closure.kind.closure.function.kind.function.chunk;
                    ip = frame.ip;
                },
                .closure => {
                    const index: usize = @intCast(chunk.code.items[ip]);
                    ip += 1;
                    const func = chunk.constants.items[index];

                    const closure = switch (func) {
                        .obj => |obj| switch (obj.kind) {
                            .function => |_| try Value.Obj.allocClosure(allocator, &self.objects, obj),
                            else => return self.runtimeError("Closure operand must be a function.", .{}),
                        },
                        else => return self.runtimeError("Closure operand must be an object.", .{}),
                    };

                    try self.stack.append(allocator, .{ .obj = closure });

                    const upvalue_count = closure.kind.closure.function.kind.function.upvalue_count;

                    for (0..upvalue_count) |_| {
                        const is_local_byte = chunk.code.items[ip];
                        const is_local = is_local_byte == 1;
                        ip += 1;

                        const index_byte: usize = @intCast(chunk.code.items[ip]);
                        ip += 1;

                        if (is_local) {
                            const slot_index = frame.slot_start + index_byte;
                            const upvalue = try self.captureUpvalue(allocator, slot_index);
                            try closure.kind.closure.upvalues.append(allocator, upvalue);
                        } else {
                            const upvalue = frame.closure.kind.closure.upvalues.items[index_byte];
                            try closure.kind.closure.upvalues.append(allocator, upvalue);
                        }
                    }
                },
                .close_upvalue => {
                    self.closeUpvalues(self.stack.items.len - 1);
                    _ = self.stack.pop();
                },
                .class => {
                    const index: usize = @intCast(chunk.code.items[ip]);
                    ip += 1;

                    const str = chunk.constants.items[index];
                    const name = switch (str.obj.kind) {
                        .string => |s| s.str,
                        else => unreachable,
                    };

                    const class = try Value.Obj.allocClass(allocator, &self.objects, name);
                    try self.stack.append(allocator, .{ .obj = class });
                },
                .get_property => {
                    const stack_value = self.stack.getLast();

                    const instance = switch (stack_value) {
                        .obj => |obj| switch (obj.kind) {
                            .instance => |inst| inst,
                            else => return self.runtimeError("Attempted to access property of non-instance", .{}),
                        },
                        else => return self.runtimeError("Attempted to access property of non-instance", .{}),
                    };

                    const index: usize = @intCast(chunk.code.items[ip]);
                    ip += 1;

                    const str = chunk.constants.items[index];
                    const field_name = switch (str.obj.kind) {
                        .string => |s| s.str,
                        else => unreachable,
                    };

                    if (instance.fields.get(field_name)) |value| {
                        _ = self.stack.pop(); // Pop the instance
                        try self.stack.append(allocator, value);
                    } else {
                        try instance.class.kind.class.bindMethod(self, field_name);
                    }
                },
                .set_property => {
                    const instance_value = self.stack.items[self.stack.items.len - 2];

                    const instance = switch (instance_value) {
                        .obj => |obj| switch (obj.kind) {
                            .instance => |*inst| inst,
                            else => return self.runtimeError("Attempted to access property of non-instance", .{}),
                        },
                        else => return self.runtimeError("Attempted to access property of non-instance", .{}),
                    };

                    const index: usize = @intCast(chunk.code.items[ip]);
                    ip += 1;

                    const str = chunk.constants.items[index];
                    const field_name = switch (str.obj.kind) {
                        .string => |s| s.str,
                        else => unreachable,
                    };

                    try instance.fields.put(field_name, self.stack.getLast());

                    const value = self.stack.pop() orelse return self.runtimeError("Popped stack with no values", .{});
                    _ = self.stack.pop(); // Pop the instance
                    try self.stack.append(allocator, value);
                },
                .method => {
                    const index: usize = @intCast(chunk.code.items[ip]);
                    ip += 1;

                    const str = chunk.constants.items[index];
                    const method_name = switch (str.obj.kind) {
                        .string => |s| s.str,
                        else => unreachable,
                    };

                    try self.defineMethod(method_name);
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
                        .obj => |obj_a| switch (obj_a.kind) {
                            .string => |str_a| switch (b.?) {
                                .obj => |obj_b| switch (obj_b.kind) {
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

    fn defineMethod(self: *VirtualMachine, name: []const u8) !void {
        const class_value = self.stack.items[self.stack.items.len - 2];

        const class = switch (class_value) {
            .obj => |obj| switch (obj.kind) {
                .class => |*class| class,
                else => return self.runtimeError("Attempted to define method on non-class", .{}),
            },
            else => return self.runtimeError("Attempted to define method on non-class", .{}),
        };

        const method = self.stack.getLast();
        try class.methods.put(name, method);

        _ = self.stack.pop(); // pop closure of the method
    }

    fn concatStrings(allocator: std.mem.Allocator, objects: *?*Value.Obj, str_a: Value.Obj.String, str_b: Value.Obj.String) !*Value.Obj {
        const new_str = try std.mem.concat(allocator, u8, &.{ str_a.str, str_b.str });
        defer allocator.free(new_str);

        return try Value.Obj.allocString(allocator, objects, new_str);
    }

    fn concatStringWithNum(allocator: std.mem.Allocator, objects: *?*Value.Obj, str: Value.Obj.String, num: f64) !*Value.Obj {
        const new_str = try std.fmt.allocPrint(allocator, "{s}{}", .{ str.str, num });
        defer allocator.free(new_str);
        const obj = try Value.Obj.allocString(allocator, objects, new_str);
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
            const closure = frame.closure;
            const instruction = frame.ip - 1;

            // We use LineRun, so we need to decode it
            const chunk = closure.kind.closure.function.kind.function.chunk;
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

            if (closure.kind.closure.function.kind.function.name) |name_obj| {
                std.debug.print("{s}()\n", .{name_obj.kind.string.str});
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
        const allocator = self.allocator;
        const native_fn = try Value.Obj.allocNativeFn(allocator, &self.objects, func);

        try self.globals.put(name, .{ .obj = native_fn });
    }

    fn captureUpvalue(self: *VirtualMachine, allocator: std.mem.Allocator, slot_index: usize) !*Value.Obj {
        var prev_upvalue: ?*Value.Obj = null;
        var upvalue = self.open_upvalues;

        while (upvalue != null and upvalue.?.kind.upvalue.location.? > slot_index) {
            prev_upvalue = upvalue;
            upvalue = upvalue.?.kind.upvalue.next;
        }

        if (upvalue != null and upvalue.?.kind.upvalue.location.? == slot_index) {
            return upvalue.?;
        }

        const created_upvalue_ptr = try Value.Obj.allocUpvalue(allocator, &self.objects, slot_index);
        created_upvalue_ptr.kind.upvalue.next = upvalue;

        if (prev_upvalue == null) {
            self.open_upvalues = created_upvalue_ptr;
        } else {
            prev_upvalue.?.kind.upvalue.next = created_upvalue_ptr;
        }

        return created_upvalue_ptr;
    }

    fn closeUpvalues(self: *VirtualMachine, last_index: usize) void {
        while (self.open_upvalues != null and self.open_upvalues.?.kind.upvalue.location.? >= last_index) {
            var upvalue = self.open_upvalues.?;

            const value_on_stack = self.stack.items[upvalue.kind.upvalue.location.?];
            upvalue.kind.upvalue.closed = value_on_stack;

            upvalue.kind.upvalue.location = null;

            self.open_upvalues = upvalue.kind.upvalue.next;
        }
    }
};

fn nativeClock(arg_count: usize, args: []Value) Value {
    _ = arg_count;
    _ = args;

    return .{ .float = @floatFromInt(std.time.milliTimestamp()) };
}
