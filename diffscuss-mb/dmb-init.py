#!/usr/bin/env python
"""
Init a diffscuss-mb code review directory.
"""

from optparse import OptionParser
import os
from textwrap import dedent

from common import get_git_root, DIFFSCUSS_MB_FILE_NAME, \
     USERS_DIR_NAME, REVIEWS_DIR_NAME, mkdir_for_keeps


def main(opts, args):
    git_root = get_git_root(opts.git_exe)
    os.chdir(git_root)

    dmb_root_dir = args[0]
    mkdir_for_keeps(dmb_root_dir)

    with open(DIFFSCUSS_MB_FILE_NAME, 'wb') as fil:
        fil.write(dmb_root_dir)

    users_dir = os.path.join(dmb_root_dir, USERS_DIR_NAME)
    mkdir_for_keeps(users_dir)

    reviews_dir = os.path.join(dmb_root_dir, REVIEWS_DIR_NAME)
    mkdir_for_keeps(reviews_dir)

if __name__ == '__main__':
    parser = OptionParser(usage=dedent("""\
                                       %prog [options] diffscuss-mailbox-dir

                                       Init a diffscuss mailbox system.  Must be run from
                                       within a git checkout.

                                       diffscuss-mailbox-dir is relative to the git root,
                                       and will be created.
                                       """))
    parser.add_option("-g", "--git-exe", dest="git_exe", default="git",
                      help=dedent("""\
                                  Git exe (defaults to 'git'.
                                  """))

    (opts, args) = parser.parse_args()
    if len(args) != 1:
        parser.error("Must provide exactly one diffscuss mailbox directory.")
    main(opts, args)

