#!/usr/bin/env python
"""
Post a review to one or more inboxes.
"""

from diffscuss.mailbox.common import dmb_post


def main(args):
    review_path = dmb_post(args.file, [args.inbox] + args.inboxes,
                           args.git_exe)
    if args.print_review_path:
        print review_path
