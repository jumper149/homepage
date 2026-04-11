#!/usr/bin/env python3
import re
import sys

hex_re = re.compile(r'#([0-9a-fA-F]{3}|[0-9a-fA-F]{6})\b')
rgb_re = re.compile(r'rgb\(\s*([^)]+)\)')

def invert_hex(h):
    h = h.group(1)

    if len(h) == 3:
        h = ''.join(c*2 for c in h)

    r = 255 - int(h[0:2], 16)
    g = 255 - int(h[2:4], 16)
    b = 255 - int(h[4:6], 16)

    return f"#{r:02x}{g:02x}{b:02x}"

def invert_rgb(m):
    parts = m.group(1).split("/")
    rgb = parts[0].strip().split()

    if len(rgb) < 3:
        return m.group(0)

    r, g, b = map(int, rgb[:3])
    r, g, b = 255 - r, 255 - g, 255 - b

    if len(parts) > 1:
        return f"rgb({r} {g} {b} / {parts[1].strip()})"

    return f"rgb({r} {g} {b})"

for line in sys.stdin:
    line = hex_re.sub(invert_hex, line)
    line = rgb_re.sub(invert_rgb, line)
    sys.stdout.write(line)
