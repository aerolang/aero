build:
    bazel build //...

test:
    bazel test //...

format:
    bazel run //bazel/tools/format

dev:
    bazel run //bazel/tools/dev

