const std = @import("std");

const GarbageCollector = @import("garbage_collector.zig").GarbageCollector;
const VirtualMachine = @import("virtual_machine.zig").VirtualMachine;

fn repl(allocator: std.mem.Allocator) !void {
    var gc: GarbageCollector = .init(allocator);
    defer gc.deinit();

    var vm: VirtualMachine = .init(gc.allocator());
    defer vm.deinit();

    gc.vm = &vm;

    var stdout_buf: [4096]u8 = undefined;
    var stdout_writer_state = std.fs.File.stdout().writer(&stdout_buf);
    const stdout = &stdout_writer_state.interface;

    var stdin_buf: [4096]u8 = undefined;
    var stdin_reader_state = std.fs.File.stdin().reader(&stdin_buf);
    const stdin = &stdin_reader_state.interface;

    try stdout.print("> ", .{});
    try stdout.flush();

    while (stdin.takeDelimiterExclusive('\n')) |line| {
        vm.interpret(line) catch |err| {
            try stdout.print("Error: {}\n", .{err});
        };
        try stdout.print("> ", .{});
        try stdout.flush();
    } else |err| switch (err) {
        error.EndOfStream => {
            try stdout.print("\n", .{});
            try stdout.flush();
        },
        else => return err,
    }
}

fn runFile(allocator: std.mem.Allocator, file_path: []const u8) !void {
    const source = try std.fs.cwd().readFileAlloc(allocator, file_path, std.math.maxInt(usize));
    defer allocator.free(source);

    var gc: GarbageCollector = .init(allocator);
    defer gc.deinit();

    var vm: VirtualMachine = .init(gc.allocator());
    defer vm.deinit();

    gc.vm = &vm;
    defer gc.vm = null;

    try vm.interpret(source);
}

pub fn main() !void {
    var da: std.heap.DebugAllocator(.{}) = .{};
    defer std.debug.assert(da.deinit() == .ok);
    const allocator = da.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len == 1) {
        repl(allocator) catch |err| switch (err) {
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
