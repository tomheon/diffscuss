#!/usr/bin/env python
"""
Create an inbox.
"""

from diffscuss.mailbox.common import get_inbox_name, get_inbox_path, \
    check_inited, mkdir_for_keeps


def main(args):
    check_inited(args.git_exe)
    inbox = args.inbox
    inbox_path = get_inbox_path(inbox, args.git_exe)
    mkdir_for_keeps(inbox_path)
