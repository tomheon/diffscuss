#!/usr/bin/env python

from optparse import OptionParser
import sys
from textwrap import dedent

from diffcourse.walker import walk, DIFF


def strip_diffcourse(fil, outfil):
    for (elem_type, elem) in walk(fil):
        if elem_type == DIFF:
            outfil.write(elem)


if __name__ == '__main__':
    usage="""\
          %prog [options] [FILE]

          Strip all diffcourse markers out of FILE and print the results
          to standard out.

          If FILE is -, or not provided, read from standard in."""
    parser = OptionParser(usage=dedent(usage))
    (opts, args) = parser.parse_args()
    if len(args) > 1:
        parser.error("Provide at most one FILE argument.")

    if not args or args[0] == '-':
        fil = sys.stdin
        using_stdin = True
    else:
        fil = open(args[0], 'rU')
        using_stdin = False

    try:
        strip_diffcourse(fil, sys.stdout)
    finally:
        if not using_stdin:
            fil.close()

