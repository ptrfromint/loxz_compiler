const std = @import("std");
const Chunk = @import("bytecode.zig").Chunk;
const VirtualMachine = @import("virtual_machine.zig").VirtualMachine;
const Compiler = @import("scanner.zig").Compiler;
const debug = @import("debug.zig");

fn repl() !void {
    var stdin = std.fs.File.stdin().reader(&.{});
    var stdout = std.fs.File.stdout().writer(&.{});

    var line: [1024]u8 = undefined;
    while (true) {
        try stdout.interface.print("> ", .{});
        const read = try stdin.read(&line);
        std.debug.print("{s}", .{line[0..read]});
    }
}

fn runFile(allocator: std.mem.Allocator, file_path: []const u8) !void {
    const source = try std.fs.cwd().readFileAlloc(allocator, file_path, std.math.maxInt(usize));
    defer allocator.free(source);
}

pub fn main() !void {
    var da: std.heap.DebugAllocator(.{}) = .{};
    defer std.debug.assert(da.deinit() == .ok);
    const allocator = da.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len == 1) {
        repl() catch |err| switch (err) {
            error.EndOfStream => std.debug.print("\nGoodbye!\n", .{}),
            else => return err,
        };
    } else if (args.len == 2) {
        runFile(allocator, args[1]) catch |err| switch (err) {
            error.FileNotFound => {
                std.debug.print("File {s} doesn't exist!\n", .{args[1]});
                return std.process.exit(1);
            },
            else => return err,
        };
    } else {
        std.debug.print("Usage: loxz_compiler [path]\n", .{});
        return std.process.exit(64);
    }
}
