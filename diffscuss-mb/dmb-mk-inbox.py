#!/usr/bin/env python
"""
Create an inbox.
"""

from optparse import OptionParser
from textwrap import dedent

from common import get_inbox_name, get_inbox_path, check_inited, \
     mkdir_for_keeps


def main(opts, args):
    check_inited(opts.git_exe)
    inbox = args[0]
    inbox_path = get_inbox_path(inbox, opts.git_exe)
    mkdir_for_keeps(inbox_path)


if __name__ == '__main__':
    parser = OptionParser(usage="%prog [options] inbox_name")
    parser.add_option("-g", "--git-exe", dest="git_exe", default="git",
                      help=dedent("""\
                                  Git exe (defaults to 'git'.
                                  """))
    (opts, args) = parser.parse_args()
    if len(args) != 1:
        parser.error("Must supply exactly one inbox name.")
    main(opts, args)

