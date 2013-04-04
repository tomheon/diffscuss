# -*- coding: utf-8 -*-

import itertools
import os
from StringIO import StringIO
from textwrap import dedent

from nose.tools import eq_, ok_

from diffscuss.walker import walk, MissingAuthorException, \
     EmptyCommentException, BadNestingException, \
     CommentInHeaderException, DIFF_HEADER, DIFF, \
     COMMENT_HEADER, COMMENT_BODY


def _test_fname(fname):
    return os.path.join(os.path.dirname(__file__),
                        'testfiles',
                        fname)


def _apply_to_template(template_fname, template_defaults, to_apply):
    subs = dict(template_defaults)
    for (k, v) in to_apply.items():
        if k not in ('COMMENT_TOP', 'COMMENT_BOTTOM'):
            # COMMENT_TOP and COMMENT_BOTTOM don't need a leading
            # newline, everything else does, but COMMENT_TOP and
            # COMMENT_BOTTOM also need a trailing newline
            v = '\n' + v
        else:
            v = v + '\n'
        subs[k] = v

    with open(_test_fname(template_fname), 'rb') as fil:
        template = fil.read()
        return StringIO(template.format(**subs))


def _standard_template_defaults():
    return {'COMMENT_TOP': '',
            'COMMENT_AFTER_RANGE_1': '',
            'COMMENT_AFTER_RANGE_2': '',
            'COMMENT_IN_DIFF_1': '',
            'COMMENT_IN_DIFF_2': '',
            'COMMENT_IN_DIFF_3': '',
            'COMMENT_BOTTOM': ''}


STANDARD_TEMPLATE_PARSED = \
['COMMENT_TOP',
 (DIFF_HEADER, 'diff --git a/diffscuss-mode/diffscuss-mode.el b/diffscuss-mode/diffscuss-mode.el\n'),
 (DIFF_HEADER, 'index e95bace..404b745 100644\n'),
 (DIFF_HEADER, '--- a/diffscuss-mode/diffscuss-mode.el\n'),
 (DIFF_HEADER, '+++ b/diffscuss-mode/diffscuss-mode.el\n'),
 (DIFF_HEADER, '@@ -345,6 +345,10 @@\n'),
 'COMMENT_AFTER_RANGE_1',
 (DIFF, '\n'),
 (DIFF, ' ;; insert / reply to comment commands\n'),
 (DIFF, '\n'),
 (DIFF, '+(defun diffscuss-get-date-time ()\n'),
 (DIFF, '+  "Get the current local date and time in ISO 8601."\n'),
 'COMMENT_IN_DIFF_1',
 (DIFF, '+  (format-time-string "%Y-%m-%dT%T%z"))\n'),
 (DIFF, '+\n'),
 (DIFF, ' (defun diffscuss-make-comment (leader)\n'),
 (DIFF, '   "Return a new comment."\n'),
 (DIFF, '   (let ((header (diffscuss-force-header leader)))\n'),
 'COMMENT_IN_DIFF_2',
 (DIFF_HEADER, '@@ -355,6 +359,10 @@\n'),
 'COMMENT_AFTER_RANGE_2',
 (DIFF, '             (diffscuss-get-author)\n'),
 (DIFF, '             "\\n")\n'),
 (DIFF, '             header\n'),
 (DIFF, '+            " date: "\n'),
 (DIFF, '+            (diffscuss-get-date-time)\n'),
 (DIFF, '+            "\\n"\n'),
 (DIFF, '+            header\n'),
 (DIFF, '             "\\n"\n'),
 (DIFF, '             (diffscuss-force-body leader)\n'),
 (DIFF, '             " \\n"\n'),
 (DIFF_HEADER, '@@ -384,12 +392,42 @@\n'),
 (DIFF, '   (forward-line -1)\n'),
 (DIFF, '   (end-of-line))\n'),
 (DIFF, '\n'),
 (DIFF, '+(defun diffscuss-insert-file-comment ()\n'),
 (DIFF, '+  "Insert a file-level comment."\n'),
 (DIFF, '+  (interactive)\n'),
 (DIFF, '+  (beginning-of-buffer)\n'),
 (DIFF, '+  (insert (diffscuss-make-comment "%*"))\n'),
 (DIFF, '+  (newline)\n'),
 (DIFF, '+  (forward-line -2)\n'),
 (DIFF, '+  (end-of-line))\n'),
 (DIFF, '+\n'),
 (DIFF, '+(defun diffscuss-in-header-p ()\n'),
 (DIFF, '+  "True if we\'re in the header material."\n'),
 (DIFF, "+  ;; if we travel up until we hit a meta line, we'll hit a range line\n"),
 (DIFF, "+  ;; first if we're not in a header, otherwise we'll hit a different\n"),
 (DIFF, '+  ;; meta line.\n'),
 (DIFF, '+  (save-excursion\n'),
 (DIFF, '+    (while (and (not (diffscuss-meta-line-p))\n'),
 (DIFF, '+                (zerop (forward-line -1))))\n'),
 (DIFF, '+    (not (diffscuss-range-line-p))))\n'),
 (DIFF, '+\n'),
 (DIFF, ' (defun diffscuss-comment-or-reply ()\n'),
 (DIFF, '   "Insert a comment or reply based on context."\n'),
 (DIFF, '   (interactive)\n'),
 (DIFF, '-  (if (diffscuss-parse-leader)\n'),
 (DIFF, '-      (diffscuss-reply-to-comment)\n'),
 (DIFF, '-    (diffscuss-insert-comment)))\n'),
 (DIFF, '+  ;; if at the very top of the file, insert a comment for the entire\n'),
 (DIFF, '+  ;; file (meaning before any of the diff headers or lines)\n'),
 (DIFF, '+  (if (= (point) 1)\n'),
 (DIFF, '+      (diffscuss-insert-file-comment)\n'),
 (DIFF, "+    ;; otherwise, if we're already in a comment, reply to it.\n"),
 (DIFF, '+    (if (diffscuss-parse-leader)\n'),
 (DIFF, '+        (diffscuss-reply-to-comment)\n'),
 (DIFF, "+      ;; if we're on a meta piece, go just past it\n"),
 (DIFF, '+      (if (diffscuss-in-header-p)\n'),
 (DIFF, '+          (progn (while (and (not (diffscuss-range-line-p))\n'),
 (DIFF, '+                             (zerop (forward-line 1))))\n'),
 (DIFF, '+                 (diffscuss-insert-comment))\n'),
 (DIFF, '+        ;; otherwise, new top-level comment.\n'),
 (DIFF, '+        (diffscuss-insert-comment)))))\n'),
 (DIFF, '\n'),
 (DIFF, ' ;; intelligent newline\n'),
 (DIFF, '\n'),
 (DIFF_HEADER, '@@ -442,7 +480,7 @@\n'),
 (DIFF, '   "Non nil if the current line is part of hunk\'s meta data."\n'),
 (DIFF, '   (save-excursion\n'),
 (DIFF, '       (beginning-of-line)\n'),
 (DIFF, '-      (not (looking-at "^[% +<>\\n\\\\-]"))))\n'),
 (DIFF, '+      (not (looking-at "^[% +\\n\\\\-]"))))\n'),
 (DIFF, '\n'),
 (DIFF, ' (defun diffscuss-get-source-file (old-or-new)\n'),
 (DIFF, '   "Get the name of the source file."\n'),
 (DIFF_HEADER, 'diff --git a/diffscuss/walker.py b/diffscuss/walker.py\n'),
 (DIFF_HEADER, 'index 74384c1..5852f4a 100644\n'),
 (DIFF_HEADER, '--- a/diffscuss/walker.py\n'),
 (DIFF_HEADER, '+++ b/diffscuss/walker.py\n'),
 (DIFF_HEADER, '@@ -72,10 +72,16 @@ def walk(fil):\n'),
 (DIFF, '             # level can increase by more than one....\n'),
 (DIFF, '             if line_level - cur_comment_level > 1:\n'),
 (DIFF, '                 raise BadNestingException()\n'),
 (DIFF, '+\n'),
 (DIFF, "             # or if we've changed level mid-comment...\n"),
 (DIFF, '-            if line_level != cur_comment_level and not _is_author_line(line):\n'),
 'COMMENT_IN_DIFF_3',
 (DIFF, '+            if (line_level != cur_comment_level\n'),
 (DIFF, '+                #and not _is_author_line(line)\n'),
 (DIFF, '+                and not _is_header(line)):\n'),
 (DIFF, '                 raise BadNestingException()\n'),
 (DIFF, '\n'),
 (DIFF, '+            # At this point, we accept the new line_level\n'),
 (DIFF, '+            cur_comment_level = line_level\n'),
 (DIFF, '+\n'),
 (DIFF, "             # or if this is a header line of a comment and it's not\n"),
 (DIFF, '             # either following a header or is an author line or an empty line...\n'),
 (DIFF, '             if (is_header and\n'),
 (DIFF, '\\ No newline at end of file\n'),
 'COMMENT_BOTTOM']



def _comment_in_header_defaults():
    return {'COMMENT_IN_HEADER_1': '',
            'COMMENT_IN_HEADER_2': '',
            'COMMENT_IN_HEADER_3': '',
            'COMMENT_IN_HEADER_4': '',
            'COMMENT_IN_HEADER_5': '',
            'COMMENT_IN_HEADER_6': '',
            'COMMENT_IN_HEADER_7': '',
            'COMMENT_IN_HEADER_8': ''}


MISSING_AUTHOR_COMMENT = (dedent("""\
                                 %*
                                 %* email: test@example.com
                                 %*
                                 %- yeah
                                 """),
                          MissingAuthorException)

AUTHOR_SECOND_COMMENT = (dedent("""\
                                %*
                                %* email: test@example.com
                                %* author: oh hai
                                %*
                                %- yeah
                                """),
                          MissingAuthorException)

EMPTY_COMMENT = (dedent("""\
                        %*
                        %* author: test@example.com
                        %*"""),
                 EmptyCommentException)

BAD_NESTING_COMMENT = (dedent("""\
                              %*
                              %* author: test@example.com
                              %*
                              %- body
                              %-- bad
                              %- body"""),
                       BadNestingException)

SIMPLE_COMMENT = (dedent("""\
                         %*
                         %* author: Testy McTesterson
                         %* email: hi@example.com
                         %*
                         %-
                         %- body 1 ☃
                         %-
                         %- body 2
                         %-"""),
                  [(COMMENT_HEADER, '%*\n'),
                   (COMMENT_HEADER, '%* author: Testy McTesterson\n'),
                   (COMMENT_HEADER, '%* email: hi@example.com\n'),
                   (COMMENT_HEADER, '%*\n'),
                   (COMMENT_BODY, '%-\n'),
                   (COMMENT_BODY, '%- body 1 ☃\n'),
                   (COMMENT_BODY, '%-\n'),
                   (COMMENT_BODY, '%- body 2\n'),
                   (COMMENT_BODY, '%-\n'),])

SIMPLE_THREAD = (dedent("""\
                        %*
                        %* author: Testy McTesterson
                        %* email: hi@example.com
                        %*
                        %-
                        %- body 1
                        %-
                        %- body 2
                        %-
                        %**
                        %** author: Fakity McFakerson
                        %** email: bye@example.com
                        %**
                        %--
                        %-- rbody 1
                        %--
                        %-- rbody 2
                        %--"""),
                  [(COMMENT_HEADER, '%*\n'),
                   (COMMENT_HEADER, '%* author: Testy McTesterson\n'),
                   (COMMENT_HEADER, '%* email: hi@example.com\n'),
                   (COMMENT_HEADER, '%*\n'),
                   (COMMENT_BODY, '%-\n'),
                   (COMMENT_BODY, '%- body 1\n'),
                   (COMMENT_BODY, '%-\n'),
                   (COMMENT_BODY, '%- body 2\n'),
                   (COMMENT_BODY, '%-\n'),
                   (COMMENT_HEADER, '%**\n'),
                   (COMMENT_HEADER, '%** author: Fakity McFakerson\n'),
                   (COMMENT_HEADER, '%** email: bye@example.com\n'),
                   (COMMENT_HEADER, '%**\n'),
                   (COMMENT_BODY, '%--\n'),
                   (COMMENT_BODY, '%-- rbody 1\n'),
                   (COMMENT_BODY, '%--\n'),
                   (COMMENT_BODY, '%-- rbody 2\n'),
                   (COMMENT_BODY, '%--\n'),])

TEMPLATE_TESTS = [
    ('standard_template.diffscuss', _standard_template_defaults(), STANDARD_TEMPLATE_PARSED,
     [
         MISSING_AUTHOR_COMMENT,
         EMPTY_COMMENT,
         BAD_NESTING_COMMENT,
         SIMPLE_COMMENT,
         SIMPLE_THREAD,
         AUTHOR_SECOND_COMMENT,
     ]),

    ('leading_hash_template.diffscuss', _standard_template_defaults(), STANDARD_TEMPLATE_PARSED,
     [
         MISSING_AUTHOR_COMMENT,
         EMPTY_COMMENT,
         BAD_NESTING_COMMENT,
         SIMPLE_COMMENT,
         SIMPLE_THREAD,
         AUTHOR_SECOND_COMMENT,
     ]),

     ('comment_in_header.diffscuss', _comment_in_header_defaults(), [],
      [
          (SIMPLE_COMMENT[0], CommentInHeaderException),
          (SIMPLE_THREAD[0], CommentInHeaderException),
      ]),
]


def _check_walker(template_fname, template_defaults, template_key, comment, template_parsed, expected):
    fil = _apply_to_template(template_fname, template_defaults, {template_key: comment})
    if isinstance(expected, type):
        try:
            list(walk(fil))
            ok_(False, "Expected exception %s" % expected)
        except expected:
            ok_(True)
    else:
        walked = list(walk(fil))

        expected_parsed = []
        for elem in template_parsed:
            if isinstance(elem, tuple):
                # not a template holder?  goes in as is.
                expected_parsed.append(elem)
            elif elem == template_key:
                # substitute in what we expect for the comment
                for e in expected:
                    expected_parsed.append(e)
            else:
                # it's a template key we don't care about, skip.
                pass

        eq_(expected_parsed, walked)


def test_walker():
    for (template_fname, template_defaults, template_parsed, tests) in TEMPLATE_TESTS:
        for (comment, expected) in tests:
            for template_key in template_defaults.keys():
                yield _check_walker, template_fname, template_defaults, template_key, comment, template_parsed, expected
