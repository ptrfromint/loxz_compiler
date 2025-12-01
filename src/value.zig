const std = @import("std");
const Chunk = @import("bytecode.zig").Chunk;

pub const Value = union(enum) {
    pub const Obj = union(enum) {
        pub const Function = struct {
            arity: usize,
            upvalue_count: usize,
            chunk: Chunk,
            name: ?*Value.Obj,
        };

        pub const String = struct {
            str: []u8,
        };

        pub const NativeFunction = struct {
            pub const Ptr = *const fn (arg_count: usize, args: []Value) Value;

            function: NativeFunction.Ptr,
        };

        pub const Closure = struct {
            function: *Value.Obj,
            upvalues: std.ArrayList(*Value.Obj),
        };

        pub const Upvalue = struct {
            location: ?usize,
            closed: Value = .nil,
            next: ?*Value.Obj = null,
        };

        string: String,
        function: Function,
        native_function: NativeFunction,
        closure: Closure,
        upvalue: Upvalue,

        pub fn format(this: @This(), w: *std.io.Writer) !void {
            switch (this) {
                .string => |val| try w.print("{s}", .{val.str}),
                .function => |f| try w.print("<fn {s} ({})> [code len {}]", .{
                    if (f.name) |obj| obj.string.str else "<script>",
                    f.arity,
                    f.chunk.code.items.len,
                }),
                .native_function => |*nf| try w.print("<native fn @ {*}>", .{nf}),
                .closure => |*c| try w.print("<closure @ {*}>\n{f}", .{ c, c.function.* }),
                .upvalue => |_| try w.print("<upvalue>", .{}),
            }
        }

        pub fn allocClosure(allocator: std.mem.Allocator, func: *Value.Obj) !*Value.Obj {
            const closure_obj = try allocator.create(Value.Obj);
            closure_obj.* = .{ .closure = .{
                .function = func,
                .upvalues = try .initCapacity(allocator, func.function.upvalue_count),
            } };

            return closure_obj;
        }

        pub fn allocFunc(allocator: std.mem.Allocator, arity: usize, name: ?[]const u8) !*Value.Obj {
            const func_obj = try allocator.create(Value.Obj);
            func_obj.* = .{ .function = .{
                .arity = arity,
                .chunk = .init(allocator),
                .name = if (name) |val| try Value.Obj.allocString(allocator, val) else null,
                .upvalue_count = 0,
            } };

            return func_obj;
        }

        pub fn allocNativeFn(allocator: std.mem.Allocator, func: NativeFunction.Ptr) !*Value.Obj {
            const func_obj = try allocator.create(Value.Obj);
            func_obj.* = .{ .native_function = .{ .function = func } };
            return func_obj;
        }

        pub fn allocString(allocator: std.mem.Allocator, str: []const u8) !*Value.Obj {
            const str_obj = try allocator.create(Value.Obj);
            str_obj.* = .{ .string = .{ .str = try allocator.dupe(u8, str) } };
            return str_obj;
        }

        pub fn allocUpvalue(allocator: std.mem.Allocator, slot_index: usize) !*Value.Obj {
            const upvalue_obj = try allocator.create(Value.Obj);
            upvalue_obj.* = .{ .upvalue = .{ .location = slot_index } };
            return upvalue_obj;
        }

        pub fn free(self: *Value.Obj, allocator: std.mem.Allocator) void {
            switch (self.*) {
                .string => |s| {
                    allocator.free(s.str);
                },
                .function => |f| {
                    if (f.name) |obj| obj.free(allocator);
                },
                .upvalue => |up| {
                    allocator.destroy(up);
                },
                .closure => |cl| {
                    cl.upvalues.deinit(allocator);
                },
            }

            allocator.destroy(self);
        }
    };

    nil: void,
    boolean: bool,
    float: f64,
    obj: *Obj,

    pub fn format(this: @This(), w: *std.io.Writer) !void {
        switch (this) {
            .nil => try w.print("nil", .{}),
            .float => |val| try w.print("{}", .{val}),
            .boolean => |val| try w.print("{}", .{val}),
            .obj => |obj| try w.print("{f}", .{obj.*}),
        }
    }
};
