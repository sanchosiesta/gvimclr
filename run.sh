#!/bin/sh
set -e
cd "$(dirname "$0")"
export DISPLAY="${DISPLAY:-:0}"
export XAUTHORITY="${XAUTHORITY:-$(ls -t /run/user/$(id -u)/.mutter-Xwaylandauth* 2>/dev/null | head -1)}"

if [ -f "./dist/gvimclr" ]; then
    exec ./dist/gvimclr "$@"
else
    exec dotnet run "$@"
fi
