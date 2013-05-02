#!/usr/bin/env python
"""
Remove a review from one inbox and post it to one or more others.
"""

from diffscuss.mailbox.common import get_inbox_path, check_inited, \
    dmb_post, dmb_done


def main(args):
    diffscuss_fname = args.file
    recipients = [args.inbox] + args.inboxes

    review_path = dmb_post(diffscuss_fname, recipients, args.git_exe)
    dmb_done(diffscuss_fname, args.from_inbox, args.git_exe)
    if args.print_review_path:
        print review_path
