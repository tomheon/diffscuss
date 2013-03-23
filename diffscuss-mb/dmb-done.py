#!/usr/bin/env python
"""
Remove a review from an inbox.
"""

from optparse import OptionParser
from textwrap import dedent

from common import dmb_done


def main(opts, args):
    review_path = dmb_done(args[0], opts.inbox, opts.git_exe)
    if opts.print_review_path:
        print review_path


if __name__ == '__main__':
    parser = OptionParser(usage="%prog [options] diffscuss_file")
    parser.add_option("-i", "--inbox", dest="inbox",
                      help=dedent("""\
                                  Inbox to remove file from (if not supplied, will use the
                                  return of 'git config --get diffscuss-mb.inbox'.
                                  """))
    parser.add_option("-p", "--print-review-path",
                      dest="print_review_path",
                      action="store_true",
                      help="Print the path of the review before exiting.")
    parser.add_option("-g", "--git-exe", dest="git_exe", default="git",
                      help=dedent("""\
                                  Git exe (defaults to 'git'.
                                  """))
    (opts, args) = parser.parse_args()
    if len(args) != 1:
        parser.error("Please specify a review.")
    main(opts, args)
