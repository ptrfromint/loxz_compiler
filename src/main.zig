const std = @import("std");

const GarbageCollector = @import("garbage_collector.zig").GarbageCollector;
const VirtualMachine = @import("virtual_machine.zig").VirtualMachine;

fn repl() !void {
    var stdin = std.fs.File.stdin().reader(&.{});
    var stdout = std.fs.File.stdout().writer(&.{});

    var line: [1024]u8 = undefined;
    while (true) {
        try stdout.interface.print("> ", .{});
        const read = stdin.read(&line) catch |err| {
            // Check for end of stream or other errors
            if (err == error.EndOfStream) break;
            return err;
        };
        if (read == 0) break;
        std.debug.print("{s}", .{line[0..read]});
    }
}

fn runFile(allocator: std.mem.Allocator, file_path: []const u8) !void {
    const source = try std.fs.cwd().readFileAlloc(allocator, file_path, std.math.maxInt(usize));
    defer allocator.free(source);

    // Initialize the VM using the Garbage Collector's allocator.
    var vm: VirtualMachine = .init(allocator);

    // Register the VM with the GC so it can find roots (Stack, Globals, etc.).
    const gc: *GarbageCollector = @ptrCast(@alignCast(allocator.ptr));
    gc.vm = &vm;

    defer {
        // Unregister VM before it is destroyed to avoid dangling pointers in GC.
        gc.vm = null;
        vm.deinit();
    }

    try vm.interpret(source);
}

pub fn main() !void {
    // 1. Setup the backing allocator (GeneralPurposeAllocator).
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer {
        const check = gpa.deinit();
        if (check == .leak) std.debug.print("GPA detected leaks!\n", .{});
    }
    const parent_allocator = gpa.allocator();

    // 2. Initialize the Garbage Collector.
    var gc = GarbageCollector.init(parent_allocator);
    defer gc.deinit();

    // 3. Get the allocator interface that triggers GC logic.
    const allocator = gc.allocator();

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
