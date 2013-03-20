#!/usr/bin/env python
"""
Remove a review from one inbox and post it to one or more others.
"""

from optparse import OptionParser
from textwrap import dedent

from common import get_inbox_path, check_inited, dmb_post, dmb_done


def main(opts, args):
    diffscuss_fname = args[0]
    recipients = args[1:]

    dmb_post(diffscuss_fname, recipients, opts.git_exe)
    dmb_done(diffscuss_fname, opts.inbox, opts.git_exe)


if __name__ == '__main__':
    parser = OptionParser(usage="%prog [options] diffscuss_file new_inbox_1 [new_inbox_2...]")
    parser.add_option("-i", "--inbox", dest="inbox",
                      help=dedent("""\
                                  Inbox to remove file from (if not supplied, will use the
                                  return of 'git config --get diffscuss-mb.inbox'.
                                  """))
    parser.add_option("-g", "--git-exe", dest="git_exe", default="git",
                      help=dedent("""\
                                  Git exe (defaults to 'git'.
                                  """))
    (opts, args) = parser.parse_args()
    if len(args) < 2:
        parser.error("Please specify a review.")
    main(opts, args)
