#!/usr/bin/env python
"""
Check your inbox for code reviews.
"""

from collections import defaultdict
from optparse import OptionParser
import os
import sys
from textwrap import dedent

from common import get_inbox_name, get_inbox_path, check_inited
from diffscuss import walker, header, dates


def _exit(msg, exit_code):
    print >> sys.stderr, msg
    sys.exit(exit_code)


def _cur_is_none(x, y):
    return x is None


def _cur_is_earlier(cur, new):
    return (cur is None or
            dates.parse_to_utc_dt(cur) < dates.parse_to_utc_dt(new))


def _maybe_update(cur, parsed_header, header_name, pred):
    if parsed_header:
        name, val = parsed_header
        if header_name == name and pred(cur, val):
            return val
    return cur


def _is_author(parsed_header):
    return parsed_header and parsed_header[0] == 'author'


def _fmt_top_commenters(author_counts):
    cnt_auth_tups = [(cnt, auth) for (auth, cnt) in author_counts.items()]
    cnt_auth_tups.sort()
    cnt_auth_tups.reverse()
    return ", ".join(["%s (%d)" % (auth, cnt)
                      for (cnt, auth)
                      in cnt_auth_tups[:3]])


def _parse(listing):
    author = None
    fname = os.path.basename(listing)
    posted_at = None
    last_comment_at = None
    comments = 0
    diff_lines = 0
    author_counts = defaultdict(int)

    with open(listing, 'rb') as fil:
        for (line_type, line) in walker.walk(fil):
            if line_type == walker.COMMENT_HEADER:
                parsed_header = header.parse_header(line)
                author = _maybe_update(author, parsed_header,
                                       'author', _cur_is_none)
                posted_at = _maybe_update(posted_at, parsed_header,
                                          'date', _cur_is_none)
                last_comment_at = _maybe_update(last_comment_at,
                                                parsed_header,
                                                'date',
                                                _cur_is_earlier)
                if _is_author(parsed_header):
                    comments += 1
                    author_counts[parsed_header[1]] += 1
            elif line_type == walker.DIFF_HEADER:
                diff_lines += 1
            elif line_type == walker.DIFF:
                diff_lines += 1
            elif line_type == walker.COMMENT_BODY:
                pass
            else:
                raise Exception("Bad line type %s" % line_type)

    return (fname, author, posted_at,
            last_comment_at, diff_lines,
            comments, _fmt_top_commenters(author_counts))

def _gen_summary(listing):
    return dedent("""\
                  File: %s
                  Review-Author: %s
                  Review-Date: %s
                  Last-Comment-Date: %s
                  Diff-Lines: %d
                  Comments: %d
                  Top-Commenters: %s
                  """ %
                  (_parse(listing)))


def _format_listing(listing, emacs, short):
    f_line = listing

    if emacs:
        f_line = "%s:1:1" % listing

    if short:
        return f_line
    else:
        return "%s\n%s\n\n" % (_gen_summary(listing),
                               f_line)


def main(opts, args):
    os.chdir(args[0])
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
                                  opts.emacs, opts.short)


if __name__ == '__main__':
    parser = OptionParser(usage="%prog [options] directory_to_check")
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
    parser.add_option("-s", "--short", dest="short",
                      action="store_true", default=False,
                      help="List only the reviews, no info about them.")

    (opts, args) = parser.parse_args()
    if len(args) != 1:
        parser.error("Directory argument required.")
    main(opts, args)

