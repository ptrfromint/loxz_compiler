const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const exe_mod = b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    const exe = b.addExecutable(.{
        .name = "loxz_compiler",
        .root_module = exe_mod,
    });
    b.installArtifact(exe);

    const debug_logs = b.option(
        bool,
        "show_debug_logs",
        "Build option to display all debug logs while running.",
    ) orelse false;

    const options = b.addOptions();
    options.addOption(bool, "show_debug", debug_logs);
    exe.root_module.addOptions("build_options", options);

    const run_step = b.step("run", "Run the app");
    const run_cmd = b.addRunArtifact(exe);
    run_step.dependOn(&run_cmd.step);
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| run_cmd.addArgs(args);

    const tests = b.addTest(.{ .root_module = exe.root_module });
    const run_tests = b.addRunArtifact(tests);
    const test_step = b.step("test", "Run tests");
    test_step.dependOn(&run_tests.step);

    const exe_check = b.addExecutable(.{
        .name = "loxz_compiler_check",
        .root_module = exe_mod,
    });

    const check = b.step("check", "Check if the compiler compiles");
    check.dependOn(&exe_check.step);
}
