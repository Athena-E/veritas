#!/bin/bash
# Run a Veritas unikernel in QEMU
# Usage: ./scripts/run_unikernel.sh <source.veri>
#
# Prerequisites: qemu-system-x86_64
# Install: sudo pacman -S qemu-system-x86

set -e

if [ -z "$1" ]; then
    echo "Usage: $0 <source.veri>"
    exit 1
fi

SOURCE="$1"
BINARY="/tmp/veritas_unikernel.elf"

echo "Compiling $SOURCE to bare-metal binary..."
cargo run --release -- "$SOURCE" --verify --target-bare-metal -o "$BINARY"

echo ""
echo "Running in QEMU (serial output on stdout, Ctrl-A X to quit)..."
echo "================================================================"
qemu-system-x86_64 \
    -kernel "$BINARY" \
    -serial stdio \
    -display none \
    -no-reboot \
    -m 16M
