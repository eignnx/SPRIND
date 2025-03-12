#!/bin/bash

# Get the absolute path of the repository root
REPO_ROOT="$(git rev-parse --show-toplevel)"

# Create the symbolic link using absolute paths
ln -s "$REPO_ROOT/hooks/pre-commit" "$REPO_ROOT/.git/hooks/pre-commit"