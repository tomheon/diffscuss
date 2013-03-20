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

    For each line that is part of a diff,

    (DIFF_HEADER, line)

    For each diff header line (e.g. Index lines, range lines), or

    (COMMENT, comment)

    for each Diffscuss comment, where comment is a Comment named tuple.

    @fil: a file-like object containing Diffscuss.

    The default error handler raises the following exceptions:

    MissingAuthorException: if there's no author header at the start
    of a comment.

    BadNestingException: if a comment is improperly nested.

    EmptyCommentException: if a comment has no body.

    CommentInHeaderException: if a comment appears in a diff header.
    """
    line = fil.readline()
    in_header = False

    while True:

        if not line:
            break

        if _is_diffscuss_line(line):
            if in_header:
                raise CommentInHeaderException()
            comment, line = _read_comment(line, fil)
            yield (COMMENT, comment)
            # continue so we don't read another line at the bottom
            continue
        elif in_header or _is_not_diff_line(line):
            # check for non-diff line has to come second, since the
            # --- and +++ in the header will read as diff lines
            # otherwise
            yield (DIFF_HEADER, line)
            in_header = not _is_range_line(line)
        else:
            yield (DIFF, line)

        line = fil.readline()


def _read_comment(line, fil):
    header_lines, line = _read_header(line, fil)
    _check_header(header_lines)
    body_lines, line = _read_body(line, fil)
    _check_body(body_lines)

    return Comment(header_lines=header_lines,
                   body_lines=body_lines), line


def _check_body(body_lines):
    if not body_lines:
        raise EmptyCommentException()


def _check_header(header_lines):
    for line in header_lines:
        if _is_author_line(line):
            return
        if not _is_empty_header(line):
            raise MissingAuthorException()
    raise MissingAuthorException()


def _level(line):
    header_match = _is_header(line)
    if header_match:
        return len(header_match.group(1)) - 1

    body_match = _is_body(line)
    if body_match:
        return len(body_match.group(1)) - 1

    return None


def _read_header(line, fil):
    return _read_comment_part(line, fil, _is_header)


def _read_body(line, fil):
    return _read_comment_part(line, fil, _is_body)


def _read_comment_part(line, fil, pred):
    part_lines = []
    level = _level(line)

    while True:
        if not pred(line):
            break
        if _level(line) != level:
            raise BadNestingException()
        part_lines.append(line)
        line = fil.readline()

    return part_lines, line


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


def _is_range_line(line):
    return line.startswith('@@')


def _is_diffscuss_line(line):
    return line.startswith('%')


# legal starts to a unified diff line inside a hunk
DIFF_CHARS = (' ', '+', '-', '\\')


def _is_not_diff_line(line):
    """
    Treat a totally blank line as a diff line to be flexible, since emacs
    can strip trailing spaces.
    """
    return line.strip() and not line.startswith(DIFF_CHARS)
