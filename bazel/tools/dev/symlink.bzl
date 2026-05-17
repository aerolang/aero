"""Macro for declaring development symlinks.

Lets BUILD files create symlinks into the Bazel output directory so that
Bazel-unaware tooling can find build artifacts at their workspace-relative paths.
"""

load("@rules_shell//shell:sh_binary.bzl", "sh_binary")

def setup_dev_symlinks(name, paths):
    """Create a target that sets up development symlinks.

    Creates symlinks from <workspace>/<path> to <workspace>/bazel-bin/<path>.

    Args:
        name: Name of the target
        paths: List of workspace-relative paths
    """
    sh_binary(
        name = name,
        srcs = ["//bazel/tools/dev:create-symlinks.sh"],
        args = paths,
        visibility = ["//:__pkg__"],
    )
