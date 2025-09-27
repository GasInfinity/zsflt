const std = @import("std");

pub fn build(b: *std.Build) void {
    _ = b.addModule("zsflt", .{ .root_source_file = b.path("src/zsflt.zig") });

    const zsflt_unit_tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/zsflt.zig"),
            .target = b.resolveTargetQuery(.{})
        }),
    });

    const run_zsflt_unit_tests = b.addRunArtifact(zsflt_unit_tests);

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_zsflt_unit_tests.step);
}
