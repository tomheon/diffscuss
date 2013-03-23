#!/usr/bin/env python
"""
Post a review to one or more inboxes.
"""

from optparse import OptionParser
from textwrap import dedent

from common import dmb_post


def main(opts, args):
    review_path = dmb_post(args[0], args[1:], opts.git_exe)
    if opts.print_review_path:
        print review_path


if __name__ == '__main__':
    parser = OptionParser(usage="%prog [options] diffscuss_file recipient1 [recipient2...]")
    parser.add_option("-p", "--print-review-path",
                      dest="print_review_path",
                      action="store_true",
                      help="Print the path of the review before exiting.")
    parser.add_option("-g", "--git-exe", dest="git_exe", default="git",
                      help=dedent("""\
                                  Git exe (defaults to 'git'.
                                  """))
    (opts, args) = parser.parse_args()
    if len(args) < 2:
        parser.error("Please specify a diffscuss file and at least one recipient.")
    main(opts, args)
