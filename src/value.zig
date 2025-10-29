const std = @import("std");

pub const Value = union(enum) {
    float: f64,

    pub fn format(this: @This(), w: *std.io.Writer) !void {
        switch (this) {
            .float => |val| try w.print("{}", .{val}),
        }
    }
};
