const std = @import("std");

const Chunk = @import("bytecode.zig").Chunk;

pub const Value = union(enum) {
    pub const Obj = struct {
        kind: Kind,
        is_marked: bool = false,
        next: ?*Obj = null,

        pub const Kind = union(enum) {
            function: Function,
            string: String,
            native_function: NativeFunction,
            closure: Closure,
            upvalue: Upvalue,

            pub fn format(this: *const Kind, w: *std.io.Writer) !void {
                switch (this.*) {
                    .string => |val| try w.print("{s}", .{val.str}),
                    .function => |f| try w.print("<fn {s} ({})> [code len {}]", .{
                        if (f.name) |obj| obj.kind.string.str else "<script>",
                        f.arity,
                        f.chunk.code.items.len,
                    }),
                    .native_function => |*nf| try w.print("<native fn @ {*}>", .{nf}),
                    .closure => |*c| try w.print("<closure @ {*}>\n{f}", .{ c, c.function.kind }),
                    .upvalue => |_| try w.print("<upvalue>", .{}),
                }
            }
        };

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

        pub fn format(this: *const Obj, w: *std.io.Writer) !void {
            try w.print("{f}", .{this.kind});
        }

        pub fn allocClosure(allocator: std.mem.Allocator, objects: *?*Value.Obj, func: *Value.Obj) !*Value.Obj {
            const closure_obj = try allocator.create(Value.Obj);
            closure_obj.* = .{
                .kind = .{
                    .closure = .{
                        .function = func,
                        .upvalues = try .initCapacity(allocator, func.kind.function.upvalue_count),
                    },
                },
                .next = objects.*,
            };
            objects.* = closure_obj;

            return closure_obj;
        }

        pub fn allocFunc(allocator: std.mem.Allocator, objects: *?*Value.Obj, arity: usize, name: ?[]const u8) !*Value.Obj {
            const func_obj = try allocator.create(Value.Obj);
            func_obj.* = .{
                .kind = .{
                    .function = .{
                        .arity = arity,
                        .chunk = .init(allocator),
                        .name = if (name) |val| try Value.Obj.allocString(allocator, objects, val) else null,
                        .upvalue_count = 0,
                    },
                },
                .next = objects.*,
            };
            objects.* = func_obj;

            return func_obj;
        }

        pub fn allocNativeFn(allocator: std.mem.Allocator, objects: *?*Value.Obj, func: NativeFunction.Ptr) !*Value.Obj {
            const func_obj = try allocator.create(Value.Obj);
            func_obj.* = .{
                .kind = .{
                    .native_function = .{ .function = func },
                },
                .next = objects.*,
            };
            objects.* = func_obj;
            return func_obj;
        }

        pub fn allocString(allocator: std.mem.Allocator, objects: *?*Value.Obj, str: []const u8) !*Value.Obj {
            const str_obj = try allocator.create(Value.Obj);
            str_obj.* = .{
                .kind = .{
                    .string = .{ .str = try allocator.dupe(u8, str) },
                },
                .next = objects.*,
            };
            objects.* = str_obj;
            return str_obj;
        }

        pub fn allocUpvalue(allocator: std.mem.Allocator, objects: *?*Value.Obj, slot_index: usize) !*Value.Obj {
            const upvalue_obj = try allocator.create(Value.Obj);
            upvalue_obj.* = .{
                .kind = .{
                    .upvalue = .{ .location = slot_index },
                },
                .next = objects.*,
            };
            objects.* = upvalue_obj;
            return upvalue_obj;
        }

        pub fn free(self: *Value.Obj, allocator: std.mem.Allocator) void {
            switch (self.kind) {
                .string => |s| {
                    allocator.free(s.str);
                },
                .function => |f| {
                    if (f.name) |obj| obj.free(allocator);
                    // We must also free the chunk
                    var mutable_f = f;
                    mutable_f.chunk.deinit();
                },
                .closure => |cl| {
                    cl.upvalues.deinit(allocator);
                },
                else => {},
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
