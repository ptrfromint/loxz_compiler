const std = @import("std");

pub const Value = union(enum) {
    pub const Obj = union(enum) {
        string: struct {
            str: []u8,
        },
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
            },
        }
    }
};
