#!/usr/bin/env python
"""
Remove a review from an inbox.
"""

from diffscuss.mailbox.common import dmb_done


def main(args):
    review_path = dmb_done(args.file, args.from_inbox, args.git_exe)
    if args.print_review_path:
        print review_path
