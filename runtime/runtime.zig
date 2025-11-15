const std = @import("std");

comptime {
    @export(&aero_log, .{ .name = "runtime$log", .linkage = .strong });
    @export(&aero_str_concat, .{ .name = "runtime$str_concat", .linkage = .strong });
    @export(&aero_free, .{ .name = "runtime$free", .linkage = .strong });
}

const AeroStr = extern struct {
    ptr: *const anyopaque,
    len: u64,

    fn as_bytes(self: AeroStr) []const u8 {
        if (self.len == 0) return &[_]u8{};

        const byte_ptr: [*]const u8 = @ptrCast(@alignCast(self.ptr));
        return byte_ptr[0..self.len];
    }
};

fn aero_log(msg: AeroStr) callconv(.c) void {
    var stdout_buffer: [4096]u8 = undefined;
    var stdout_writer = std.fs.File.stdout().writer(&stdout_buffer);
    const stdout = &stdout_writer.interface;

    const str = msg.as_bytes();
    stdout.print("{s}\n", .{str}) catch {};
    stdout.flush() catch {};
}

fn aero_str_concat(a: AeroStr, b: AeroStr) callconv(.c) AeroStr {
    if (a.len == 0 and b.len == 0)
        return AeroStr{ .ptr = undefined, .len = 0 };

    const allocator = std.heap.c_allocator;
    const total_len = a.len + b.len;

    // Allocate memory for the concatenated string
    const buffer = allocator.alloc(u8, total_len) catch {
        // On allocation failure, return empty string
        return AeroStr{ .ptr = undefined, .len = 0 };
    };

    if (a.len > 0) {
        @memcpy(buffer[0..a.len], a.as_bytes());
    }

    if (b.len > 0) {
        @memcpy(buffer[a.len..total_len], b.as_bytes());
    }

    return AeroStr{
        .ptr = buffer.ptr,
        .len = total_len,
    };
}

fn aero_free(str: AeroStr) callconv(.c) void {
    if (str.len == 0) return;

    const allocator = std.heap.c_allocator;
    const byte_ptr: [*]u8 = @ptrCast(@alignCast(@constCast(str.ptr)));
    const slice = byte_ptr[0..str.len];
    allocator.free(slice);
}
