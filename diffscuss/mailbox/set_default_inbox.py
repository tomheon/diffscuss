#!/usr/bin/env python
"""
Set your default inbox.
"""

from diffscuss.mailbox.common import check_inited, set_inbox_name


def main(args):
    check_inited(args.git_exe)
    inbox = args.inbox
    set_inbox_name(inbox, args.git_exe)
