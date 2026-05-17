const std = @import("std");

const Str = extern struct {
    ptr: *const anyopaque,
    len: u64,
};

export fn @"builtins$str-free"(s: Str) callconv(.c) void {
    if (s.len == 0) return;
    const byte_ptr: [*]u8 = @ptrCast(@alignCast(@constCast(s.ptr)));
    std.heap.c_allocator.free(byte_ptr[0..s.len]);
}

export fn @"builtins$str-concat"(a: Str, b: Str) callconv(.c) Str {
    if (a.len == 0 and b.len == 0)
        return .{ .ptr = undefined, .len = 0 };

    const total_len = a.len + b.len;
    const buffer = std.heap.c_allocator.alloc(u8, total_len) catch @panic("out of memory");

    const a_ptr: [*]const u8 = @ptrCast(@alignCast(a.ptr));
    const b_ptr: [*]const u8 = @ptrCast(@alignCast(b.ptr));
    if (a.len > 0) @memcpy(buffer[0..a.len], a_ptr[0..a.len]);
    if (b.len > 0) @memcpy(buffer[a.len..total_len], b_ptr[0..b.len]);

    return .{ .ptr = buffer.ptr, .len = total_len };
}

export fn @"builtins$int-as-str"(n: i64) callconv(.c) Str {
    var buf: [32]u8 = undefined;
    const s = std.fmt.bufPrint(&buf, "{}", .{n}) catch @panic("out of memory");
    const buffer = std.heap.c_allocator.alloc(u8, s.len) catch @panic("out of memory");
    @memcpy(buffer, s);
    return .{ .ptr = buffer.ptr, .len = buffer.len };
}

export fn @"builtins$bool-as-str"(b: bool) callconv(.c) Str {
    const s: []const u8 = if (b) "true" else "false";
    const buffer = std.heap.c_allocator.alloc(u8, s.len) catch @panic("out of memory");
    @memcpy(buffer, s);
    return .{ .ptr = buffer.ptr, .len = buffer.len };
}
