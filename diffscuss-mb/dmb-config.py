#!/usr/bin/env python
"""
Set your default inbox.
"""

from optparse import OptionParser
from textwrap import dedent

from common import check_inited, set_inbox_name


def main(opts, args):
    check_inited(opts.git_exe)
    inbox = args[0]
    set_inbox_name(inbox, opts.git_exe)


if __name__ == '__main__':
    parser = OptionParser(usage=dedent("""\
                                       %prog [options] inbox-name

                                       Set inbox-name to be your default inbox
                                       for checking."""))
    parser.add_option("-g", "--git-exe", dest="git_exe", default="git",
                      help=dedent("""\
                                  Git exe (defaults to 'git'.
                                  """))
    (opts, args) = parser.parse_args()
    if len(args) != 1:
        parser.error("Must supply exactly one inbox name.")
    main(opts, args)

