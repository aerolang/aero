#!/usr/bin/env bash
set -euo pipefail

# This script creates multiple symlinks for development.
# Args: link paths that will point from <path> to bazel-bin/<path>

if [ $# -eq 0 ]; then
  echo "Usage: $0 <link_path> [<link_path> ...]"
  exit 1
fi

cd "$BUILD_WORKSPACE_DIRECTORY"

EXCLUDE_FILE=".git/info/exclude"

add_to_exclude() {
  local path="/$1"
  if ! grep -qxF "$path" "$EXCLUDE_FILE" 2>/dev/null; then
    if ! grep -qxF "# Dev symlinks" "$EXCLUDE_FILE" 2>/dev/null; then
      echo "# Dev symlinks" >> "$EXCLUDE_FILE"
    fi
    echo "$path" >> "$EXCLUDE_FILE"
  fi
}

any_links_made=false

while [ $# -gt 0 ]; do
  LINK_PATH="$1"
  shift

  TARGET="bazel-bin/$LINK_PATH"
  LINK_DIR=$(dirname "$LINK_PATH")

  # Calculate relative path from link directory to target.
  # Count the depth of the link path.
  #
  # We do this instead of realpath because macOS doesn't have --relative-to.
  if [ "$LINK_DIR" = "." ]; then
    RELATIVE_TARGET="./bazel-bin/$LINK_PATH"
  else
    DEPTH=$(( $(tr -dc '/' <<< "$LINK_DIR" | wc -c) + 1 ))

    # Build the relative path (../../ repeated DEPTH times, then bazel-bin/...).
    RELATIVE_TARGET=""
    for ((i = 0; i < DEPTH; i++)); do
      RELATIVE_TARGET="../$RELATIVE_TARGET"
    done
    RELATIVE_TARGET="${RELATIVE_TARGET}bazel-bin/$LINK_PATH"
  fi

  if [ -L "$LINK_PATH" ]; then
    # Check if symlink already points to the correct target.
    CURRENT_TARGET=$(readlink "$LINK_PATH")
    if [ "$CURRENT_TARGET" = "$RELATIVE_TARGET" ]; then
      continue
    fi

    rm -f "$LINK_PATH"
  elif [ -e "$LINK_PATH" ]; then
    echo "Error: $LINK_PATH exists but is not a symlink"
    exit 1
  fi

  if [ "$any_links_made" = false ]; then
    echo "Adding dev symlinks:"
    any_links_made=true
  fi

  # Create parent directory if needed.
  mkdir -p "$LINK_DIR"

  # Create the symlink and exclude it from git locally.
  ln -sf "$RELATIVE_TARGET" "$LINK_PATH"
  add_to_exclude "$LINK_PATH"
  echo "  $LINK_PATH -> $RELATIVE_TARGET"
done

if [ "$any_links_made" = false ]; then
  echo "Dev symlinks are up to date."
fi
