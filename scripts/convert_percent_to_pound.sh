#!/bin/sh

# Older diffscuss files used %* and %- instead of #* and #- for header
# / body lines.  Newer versions of the code won't read these older
# files.  This script takes an old diffscuss file on stdin and prints
# it in the new version to stdout.

sed -e 's/^%/#/'
