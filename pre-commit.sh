#!/bin/sh

set -e

if [ "$1" = install ]; then
    root="`git rev-parse --show-toplevel`"
    if [ -f "$root/.git/hooks/pre-commit" ]; then
        >&2 echo 'pre-commit hook already exists, aborting'
        exit 1
    fi
    ln -s ../../pre-commit.sh "$root/.git/hooks/pre-commit"
    exit 0
fi

set -x

ruff joe
mypy joe
isort --check --only-modified joe
black --quiet joe
