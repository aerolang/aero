const std = @import("std");

const AeroStr = extern struct {
    ptr: [*]const u8,
    len: u64,
};

pub export fn aero_log(msg: AeroStr) void {
    var stdout_buffer: [4096]u8 = undefined;
    var stdout_writer = std.fs.File.stdout().writer(&stdout_buffer);
    const stdout = &stdout_writer.interface;
    const str = msg.ptr[0..msg.len];
    stdout.print("{s}\n", .{str}) catch {};
    stdout.flush() catch {};
}
