const std = @import("std");
const Chunk = @import("bytecode.zig").Chunk;

pub const Value = union(enum) {
    pub const Obj = union(enum) {
        string: struct {
            str: []u8,
        },
        function: struct {
            arity: usize,
            chunk: Chunk,
            name: *Value.Obj,
        },

        pub fn allocFunc(allocator: std.mem.Allocator, arity: usize, name: []const u8) !*Value.Obj {
            const func_obj = try allocator.create(Value.Obj);
            func_obj.* = .{ .function = .{
                .arity = arity,
                .chunk = .init(allocator),
                .name = Value.Obj.allocString(allocator, name),
            } };

            return func_obj;
        }

        pub fn allocString(allocator: std.mem.Allocator, str: []const u8) !*Value.Obj {
            const str_obj = try allocator.create(Value.Obj);
            str_obj.* = .{ .string = .{ .str = try allocator.dupe(u8, str) } };
            return str_obj;
        }

        pub fn free(self: *Value.Obj, allocator: std.mem.Allocator) void {
            switch (self.*) {
                .string => |s| {
                    allocator.free(s.str);
                    allocator.destroy(self);
                },
                .function => |f| {
                    f.name.free(allocator);
                    allocator.destroy(self);
                },
            }
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
            .obj => |obj| switch (obj.*) {
                .string => |val| try w.print("{s}", .{val.str}),
                .function => |f| try w.print("<fn {s} ({})> [code len {}]", .{ f.name.string.str, f.arity, f.chunk.code.items.len }),
            },
        }
    }
};
