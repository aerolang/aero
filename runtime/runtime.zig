const std = @import("std");

pub export fn air_log(msg: [*:0]const u8) callconv(.c) void {
    var stdout_buffer: [4096]u8 = undefined;
    var stdout_writer = std.fs.File.stdout().writer(&stdout_buffer);
    const stdout = &stdout_writer.interface;
    stdout.print("{s}\n", .{msg}) catch {};
    stdout.flush() catch {};
}

extern fn air_main() void;

pub export fn main() callconv(.c) c_int {
    air_main();
    return 0;
}
