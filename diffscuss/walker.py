from collections import namedtuple
import re


class BadNestingException(Exception):
    pass


class MissingAuthorException(Exception):
    pass


class EmptyCommentException(Exception):
    pass


class CommentInHeaderException(Exception):
    pass


DIFF_HEADER = 'DIFF_HEADER'
DIFF = 'DIFF'
COMMENT = 'COMMENT'


Comment = namedtuple('Comment', ['header_lines', 'body_lines'])


def walk(fil):
    """
    Walk a Diffscuss file, yielding either:

    (DIFF, line)

    For each line that is not Diffscuss comment, or:

    (COMMENT, level, comment)

    for each Diffscuss comment, where level is an integer indicating
    how deeply nested the comment is (1 for a top-level comment), and
    comment is a Comment named tuple.

    @fil: a file-like object containing Diffscuss.

    The default error handler raises the following exceptions:

    MissingAuthorException: if there's no author header at the start
    of a comment.

    BadNestingException: if a comment is improperly nested.

    EmptyCommentException: if a comment has no body.

    CommentInHeaderException: if a comment appears in a diff header.
    """
    # for maintaing state
    cur_comment_level = 1
    cur_comment_lines = []
    in_diff_header = False

    for line in fil:
        if _is_diffscuss_line(line):
            if in_diff_header:
                raise CommentInHeaderException()

            line_level = _level(line)
            is_header = _is_header(line)

            # TODO: line number etc. in exceptions

            # various things can go wrong here...for example, the
            # level can increase by more than one....
            if line_level - cur_comment_level > 1:
                raise BadNestingException()

            # or if we've changed level mid-comment...
            if (line_level != cur_comment_level
                #and not _is_author_line(line)
                and not _is_header(line)):
                raise BadNestingException()

            # At this point, we accept the new line_level
            cur_comment_level = line_level

            # or if this is a header line of a comment and it's not
            # either following a header or is an author line or an empty line...
            if (is_header and
                (not cur_comment_lines or
                 not _is_header(cur_comment_lines[-1])) and
                 not _is_author_line(line) and
                 not _is_empty_header(line)):
                raise MissingAuthorException()

            # or if it's not a header and it's the first line of a
            # comment
            if not _is_header(line) and not cur_comment_lines:
                raise MissingAuthorException()

            # if we're here, we're either in a new comment, or adding
            # a header / body line to an existing comment
            if _is_author_line(line) and not all(_is_empty_header(l)
                                                 for l
                                                 in cur_comment_lines):
                if cur_comment_lines:
                    yield _process_comment(cur_comment_lines)
                    cur_comment_lines = []
            cur_comment_lines.append(line)
        elif _is_start_range_info(line):
            # if this is one, start allowing Diffscuss.
            in_diff_header = False
            yield (DIFF_HEADER, line)
        elif in_diff_header:
            yield (DIFF_HEADER, line)
        elif _is_not_diff_line(line):
            # we've moved out of range where diffscuss is legal
            in_diff_header = True
            if cur_comment_lines:
                yield _process_comment(cur_comment_lines)
                cur_comment_lines = []
            yield (DIFF_HEADER, line)
        else:
            if cur_comment_lines:
                yield _process_comment(cur_comment_lines)
                cur_comment_lines = []
            yield (DIFF, line)




    if cur_comment_lines:
        yield _process_comment(cur_comment_lines)
        cur_comment_lines = []


def _process_comment(comment_lines):
    header_lines = [line for line in comment_lines if _is_header(line)]
    body_lines = [line for line in comment_lines if not _is_header(line)]

    if not body_lines:
        raise EmptyCommentException()

    return (COMMENT,
            Comment(header_lines=header_lines,
                    body_lines=body_lines))


def _is_start_range_info(line):
    return line.startswith('@@')


def _is_diffscuss_line(line):
    return line.startswith('%')


# legal starts to a unified diff line inside a hunk
DIFF_CHARS = [' ', '+', '-', '\\']


def _is_not_diff_line(line):
    """
    Treat a totally blank line as a diff line to be flexible.
    """
    return (line.strip() and
            not any(line.startswith(diff_char) for diff_char in DIFF_CHARS))


def _level(line):
    header_match = _is_header(line)
    if header_match:
        return len(header_match.group(1)) - 1

    body_match = _is_body(line)
    if body_match:
        return len(body_match.group(1)) - 1

    return None


HEADER_RE = re.compile(r'^(%[*]+)( |$)')
EMPTY_HEADER_RE = re.compile(r'^(%[*]+)\s*$')


def _is_header(line):
    return HEADER_RE.match(line)


def _is_empty_header(line):
    return EMPTY_HEADER_RE.match(line)

AUTHOR_RE = re.compile(r'^(%[*]+) author: ')


def _is_author_line(line):
    return AUTHOR_RE.match(line)



BODY_RE = re.compile(r'^(%[-]+)( |$)')


def _is_body(line):
    return BODY_RE.match(line)
