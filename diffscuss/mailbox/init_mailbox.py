#!/usr/bin/env python
"""
Init a diffscuss-mb code review directory.
"""

import os

from diffscuss.mailbox.common import get_git_root, DIFFSCUSS_MB_FILE_NAME, \
     USERS_DIR_NAME, REVIEWS_DIR_NAME, mkdir_for_keeps


def main(args):
    git_root = get_git_root(args.git_exe)
    os.chdir(git_root)

    dmb_root_dir = args.directory
    mkdir_for_keeps(dmb_root_dir)

    with open(DIFFSCUSS_MB_FILE_NAME, 'wb') as fil:
        fil.write(dmb_root_dir)

    users_dir = os.path.join(dmb_root_dir, USERS_DIR_NAME)
    mkdir_for_keeps(users_dir)

    reviews_dir = os.path.join(dmb_root_dir, REVIEWS_DIR_NAME)
    mkdir_for_keeps(reviews_dir)
