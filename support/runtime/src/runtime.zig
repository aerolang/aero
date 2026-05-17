const std = @import("std");

const Str = extern struct {
    ptr: *const anyopaque,
    len: u64,

    fn as_bytes(self: Str) []const u8 {
        if (self.len == 0) return &[_]u8{};
        const byte_ptr: [*]const u8 = @ptrCast(@alignCast(self.ptr));
        return byte_ptr[0..self.len];
    }
};

var global_io: std.Io.Threaded = .init_single_threaded;

export fn @"runtime$log"(msg: Str) callconv(.c) void {
    const io = global_io.io();
    const stdout = std.Io.File.stdout();
    var buf: [4096]u8 = undefined;
    var w = stdout.writer(io, &buf);
    w.interface.print("{s}\n", .{msg.as_bytes()}) catch {};
    w.interface.flush() catch {};
}
