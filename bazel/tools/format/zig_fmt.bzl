"""Rule to expose the zig binary as a named executable for aspect_rules_lint."""

def _zig_fmt_impl(ctx):
    info = ctx.toolchains["@rules_zig//zig:toolchain_type"].zigtoolchaininfo
    zig_exe = info.zig_files[0]
    out = ctx.actions.declare_file(ctx.label.name)
    ctx.actions.symlink(output = out, target_file = zig_exe, is_executable = True)
    return [DefaultInfo(executable = out, runfiles = ctx.runfiles(files = info.zig_files))]

zig_fmt = rule(
    implementation = _zig_fmt_impl,
    executable = True,
    toolchains = ["@rules_zig//zig:toolchain_type"],
)
