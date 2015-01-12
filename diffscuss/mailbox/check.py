#!/usr/bin/env python
"""
Check your inbox for code reviews.
"""

from collections import defaultdict
from optparse import OptionParser
import os
import sys
from textwrap import dedent

from diffscuss.mailbox.common import get_inbox_name, get_inbox_path, \
    check_inited
from diffscuss import walker, header, dates


def _exit(msg, exit_code):
    print >> sys.stderr, msg
    sys.exit(exit_code)


class HeaderExtractor(object):

    def __init__(self, header_names):
        if not isinstance(header_names, list):
            header_names = [header_names]
        self.header_names = header_names

    def process_line(self, line_tup):
        (line_type, line) = line_tup
        if line_type == walker.COMMENT_HEADER:
            parsed_header = header.parse_header(line)
            if (parsed_header and
                parsed_header[0] in self.header_names):
                self.extract_from_header(parsed_header)

    def extract_from_header(self, parsed_header):
        # for override
        pass

    def get(self):
        # for override
        pass


class OrigAuthorExtractor(HeaderExtractor):

    def __init__(self):
        super(OrigAuthorExtractor, self).__init__('author')
        self.author = None

    def extract_from_header(self, parsed_header):
        if self.author is None:
            self.author = parsed_header[1]

    def get(self):
        return self.author


class PostedAtExtractor(HeaderExtractor):

    def __init__(self):
        super(PostedAtExtractor, self).__init__('date')
        self.posted_at = None

    def extract_from_header(self, parsed_header):
        if self.posted_at is None:
            self.posted_at = parsed_header[1]

    def get(self):
        return dates.parse_to_local_dt(self.posted_at)


class LastCommentExtractor(HeaderExtractor):

    def __init__(self):
        super(LastCommentExtractor, self).__init__(['author', 'date'])
        self.last_comment_at = None
        self.last_comment_by = None
        self.last_author_seen = None

    def extract_from_header(self, parsed_header):
        if parsed_header[0] == 'author':
            self.last_author_seen = parsed_header[1]
        if parsed_header[0] == 'date' and self._is_latest(parsed_header[1]):
            self.last_comment_at = parsed_header[1]
            self.last_comment_by = self.last_author_seen

    def _is_latest(self, dt_s):
        return (self.last_comment_at is None or
                dates.parse_to_utc_dt(self.last_comment_at) <
                dates.parse_to_utc_dt(dt_s))

    def get(self):
        return "%s by %s" % (dates.parse_to_local_dt(self.last_comment_at),
                             self.last_comment_by)


class LineCounter(object):

    def __init__(self, line_types):
        self.line_types = line_types
        self.line_count = 0

    def process_line(self, line_tup):
        (line_type, line) = line_tup
        if line_type in self.line_types:
            self.line_count += 1

    def get(self):
        return self.line_count


class TopAuthorsExtractor(HeaderExtractor):

    def __init__(self):
        super(TopAuthorsExtractor, self).__init__('author')
        self.author_counts = defaultdict(int)

    def extract_from_header(self, parsed_header):
        self.author_counts[parsed_header[1]] += 1

    def get(self):
        cnt_auth_tups = [(cnt, auth)
                         for (auth, cnt)
                         in self.author_counts.items()]
        cnt_auth_tups.sort()
        cnt_auth_tups.reverse()
        return ", ".join(["%s (%d)" % (auth, cnt)
                          for (cnt, auth)
                          in cnt_auth_tups[:3]])


class NumCommentsExtractor(HeaderExtractor):

    def __init__(self):
        super(NumCommentsExtractor, self).__init__('author')
        self.comment_count = 0

    def extract_from_header(self, parsed_header):
        self.comment_count += 1

    def get(self):
        return self.comment_count


def _parse(listing):
    fname = os.path.basename(listing)

    extractors = [OrigAuthorExtractor(),
                  PostedAtExtractor(),
                  LastCommentExtractor(),
                  LineCounter([walker.DIFF_HEADER, walker.DIFF]),
                  NumCommentsExtractor(),
                  TopAuthorsExtractor()]

    with open(listing, 'rb') as fil:
        for line_tup in walker.walk(fil):
            for extractor in extractors:
                extractor.process_line(line_tup)

    return [fname] + [e.get() for e in extractors]


def _gen_summary(listing):
    try:
        return dedent("""\
                  File: %s
                  Posted-By: %s
                  Posted-At: %s
                  Last-Comment: %s
                  Diff-Lines: %d
                  Comments: %d
                  Top-Commenters: %s
                  """ % tuple(_parse(listing)))
    except:
        return "Trouble parsing diffscuss, no summary available.\n"


def _format_listing(listing, emacs, short):
    f_line = listing

    if emacs:
        f_line = "%s:1:1" % listing

    if short:
        return f_line
    else:
        return "%s%s\n\n" % (_gen_summary(listing),
                             f_line)


def main(args):
    check_inited(args.git_exe)
    inbox = args.inbox
    if not inbox:
        try:
            inbox = get_inbox_name(args.git_exe)
        except:
            _exit("Could not find default inbox, please run "
                  "'diffscuss mailbox set-default-inbox'", 3)
    inbox_path = get_inbox_path(inbox, args.git_exe)
    if not os.path.exists(inbox_path):
        _exit("Inbox '%s' doesn't exist, create it "
              "with 'diffscuss mailbox make-inbox'" % inbox_path, 2)
    for review in os.listdir(inbox_path):
        if review != '.gitkeep':
            print _format_listing(os.path.join(inbox_path, review),
                                  args.emacs, args.short)
