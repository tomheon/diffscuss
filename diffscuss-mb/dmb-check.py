#!/usr/bin/env python
"""
Check your inbox for code reviews.
"""

from optparse import OptionParser
import os
import sys
from textwrap import dedent

from common import get_inbox_name, get_inbox_path, check_inited


def _exit(msg, exit_code):
    print >> sys.stderr, msg
    sys.exit(exit_code)


def _format_listing(listing, emacs):
    if emacs:
        return "%s:1:1" % listing
    else:
        return listing


def main(opts, args):
    check_inited(opts.git_exe)
    inbox = opts.inbox
    if not inbox:
        try:
            inbox = get_inbox_name(opts.git_exe)
        except:
            _exit("Could not find default inbox, please run dmb-config.py", 3)
    inbox_path = get_inbox_path(inbox, opts.git_exe)
    if not os.path.exists(inbox_path):
        _exit("Inbox '%s' doesn't exist, create it with dmb-mk-inbox.py" % inbox, 2)
    for review in os.listdir(inbox_path):
        if review != '.gitkeep':
            print _format_listing(os.path.join(inbox_path, review),
                                  opts.emacs)


if __name__ == '__main__':
    parser = OptionParser(usage="%prog [options]")
    parser.add_option("-g", "--git-exe", dest="git_exe", default="git",
                      help=dedent("""\
                                  Git exe (defaults to 'git'.
                                  """))
    parser.add_option("-i", "--inbox", dest="inbox",
                      help=dedent("""\
                                  Inbox name (if not supplied, will use the return of
                                  'git config --get diffscuss-mb.inbox'.
                                  """))
    parser.add_option("-e", "--emacs", dest="emacs",
                      action="store_true",
                      help="Format for emacs compilation mode")

    (opts, args) = parser.parse_args()
    if args:
        parser.error("No arguments supported.")
    main(opts, args)

