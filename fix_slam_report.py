#!/usr/bin/env python3
content = open('app_slam.R', 'rb').read()
lines = content.split(b'\n')

# Line 1283: bytes 23-36 are: 5c 7b 5c 7b 62 6c 61 6e 6b 5c 5c 7d 5c 7d
# We need: 5c 5c 7b 5c 5c 7b 62 6c 61 6e 6b 5c 5c 7d 5c 5c 7d

line = lines[1282]
print("Original bytes 20-40:", line[20:40].hex())
print("Line repr:", repr(line))

# The bad pattern at positions 23-36:
# [23]=5c [24]=7b [25]=5c [26]=7b 62 6c 61 6e 6b [32]=5c [33]=5c [34]=7d [35]=5c [36]=7d
# We need to change [23]=5c->5c, [24]=7b stays, [25]=5c->5c, [26]=7b stays,
# [32]=5c->5c (already), [33]=5c stays, [34]=7d stays, [35]=5c->5c, [36]=7d stays
# Wait - positions 32-36 are: 5c 5c 7d 5c 7d = "\\}" which is CORRECT
# The problem is positions 23-26: 5c 7b 5c 7b = "\{\{" which is WRONG - should be "\\{\\{"

# Current: "\{\{" (positions 23-26)
# Should be: "\\{\\{" (positions 23-27, one more byte)
# So we need to INSERT one byte at position 24 (after the first backslash)

# Let's do a targeted replacement of the specific byte sequence
# Old: 5c 7b 5c 7b 62 6c 61 6e 6b 5c 5c 7d 5c 7d
# New: 5c 5c 7b 5c 5c 7b 62 6c 61 6e 6b 5c 5c 7d 5c 5c 7d

old_bytes = b'\x5c\x7b\x5c\x7b\x62\x6c\x61\x6e\x6b\x5c\x5c\x7d\x5c\x7d'
new_bytes = b'\x5c\x5c\x7b\x5c\x5c\x7b\x62\x6c\x61\x6e\x6b\x5c\x5c\x7d\x5c\x5c\x7d'

if old_bytes in line:
    lines[1282] = line.replace(old_bytes, new_bytes, 1)
    print("Fixed!")
    new_content = b'\n'.join(lines)
    open('app_slam.R', 'wb').write(new_content)
    print("Written. Verifying...")

    import subprocess
    r = subprocess.run(['Rscript', '-e', 'source("app_slam.R", echo=FALSE)'],
                       capture_output=True, text=True,
                       cwd='/home/yzhang/clawfiles/celf5_shiny')
    err = r.stderr + r.stdout
    if 'error' in err.lower() or 'unexpected' in err.lower():
        print("SYNTAX ERROR:", err[-400:])
    else:
        print("SYNTAX OK - Lines:", new_content.count(b'\n'))
else:
    print("Pattern NOT FOUND")
    print("Line hex:", line.hex())