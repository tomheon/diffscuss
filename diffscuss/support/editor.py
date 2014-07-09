"""
Diffscuss editor support.

For the most part there's nothing too editor-specific in here: functions
assumed they'll be passed functions they can call to get at editor-specific
stuff, or (in the common case) a buffer and cursor. (Buffers are a 0-indexed
list of lines implementing the Vim Python interface; cursors are a 2-tuple of
integers for the 1-indexed row and column.)
"""
import time
import os
import re
import subprocess


class LineProperties(object):
    """
    Determines the properties of a single line in a diffcuss file.

    The line can be one of the following types:

      * a vanilla diff metadata line (a line that is not diff content)
      * a vanilla diff range line (a diff @@ line)
      * a diffscuss header line
      * a diffscuss body line

    If the line is not one of the diffscuss types, it has a depth of 0. If it
    is a diffscuss line type, its depth is 1 for top-level diffscuss headers
    and bodies and increases by 1 for each reply level below that.
    """
    def __init__(self, line):
        self.line = line

    @property
    def is_diff_meta(self):
        """
        Returns True if the line is a metadata line from plain old diff.

        >>> LineProperties('@@ -0,0 +1,2 @@').is_diff_meta
        True
        >>> LineProperties('diff --git a/some/file').is_diff_meta
        True
        >>> LineProperties('#* This is a header line').is_diff_meta
        False
        """
        if self.line.startswith(('---', '+++')):
            return True
        return not self.line.startswith((' ', '#', '\n', '-', '+'))

    @property
    def is_diff_range(self):
        """
        Returns True if the line is a range line from plain old diff.

        >>> LineProperties('@@ -0,0 +1,2 @@').is_diff_range
        True
        >>> LineProperties('diff --git a/some/file').is_diff_range
        False
        """
        return self.line.startswith('@@')

    @property
    def is_header(self):
        """
        Returns True if the line is a diffscuss header line.

        >>> LineProperties('#* This is a header line').is_header
        True
        >>> LineProperties('#- This is a body line').is_header
        False
        """
        return self.line.startswith('#*')

    @property
    def is_body(self):
        """
        Returns True if the line is a diffscuss body line.

        >>> LineProperties('#* This is a header line').is_body
        False
        >>> LineProperties('#- This is a body line').is_body
        True
        """
        return self.line.startswith('#-')

    @property
    def is_diffscuss(self):
        """
        Returns True if the line is a diffscuss header or body line.

        >>> LineProperties('#* This is a header line').is_diffscuss
        True
        >>> LineProperties('#- This is a body line').is_diffscuss
        True
        """
        return self.depth > 0

    @property
    def depth(self):
        """
        Returns 0 if the line is not a diffscuss line, 1 if it is a top-level
        diffscuss header or body line, or an integer according to the depth of
        the reply for non-top-level diffscuss header or body lines.

        >>> LineProperties('@@ -0,0 +1,2 @@').depth
        0
        >>> LineProperties('#---- This is a deep reply body line').depth
        4
        """
        match = re.search(r'^#((\*|-)\2*)', self.line)
        if match:
            return len(match.group(1))
        return 0


### Comment insertion

def find_header_start(buf, (row, col)):
    """
    If the cursor is in a diffscuss comment, returns the row of the start of
    the header for that comment and the current column. Otherwise, returns the
    current row and column.
    """
    if not LineProperties(buf[row - 1]).is_diffscuss:
        return row, col

    # Skip body lines.
    for body_offset, line in enumerate(reversed(buf[:row])):
        if not LineProperties(line).is_body:
            break

    # Find the first non-header line.
    for offset, line in enumerate(reversed(buf[:row - body_offset])):
        if not LineProperties(line).is_header:
            return (row - body_offset - offset, col)
    return 1, col


def find_body_end(buf, (row, col)):
    """
    If the cursor is in a diffscuss comment, returns the row of the end of the
    body for that comment and the current column. Otherwise, returns the
    current row and column.
    """
    if not LineProperties(buf[row - 1]).is_diffscuss:
        return row, col

    # Skip header lines.
    for header_offset, line in enumerate(buf[row - 1:]):
        if not LineProperties(line).is_header:
            break

    # Find the first non-body line.
    for offset, line in enumerate(buf[row - 1 + header_offset:]):
        if not LineProperties(line).is_body:
            return (row + header_offset + offset - 1, col)
    return row, col


def find_subthread_end(buf, (row, col)):
    """
    If the cursor is in a diffscuss comment, returns the row of the end of the
    comment's subthread and the current column. Otherwise, returns the current
    row and column.
    """
    start_line_props = LineProperties(buf[row - 1])
    if not start_line_props.is_diffscuss:
        return row, col

    row, col = find_body_end(buf, (row, col))
    for offset, line in enumerate(buf[row:]):
        line_props = LineProperties(line)
        if line_props.depth <= start_line_props.depth:
            return row + offset, col
    return row, col


def find_thread_end(buf, (row, col)):
    """
    If the cursor is in a diffscuss comment, returns the row of the end of the
    comment's thread and the current column. Otherwise, returns the current row
    and column.
    """
    if LineProperties(buf[row - 1]).is_diffscuss:
        for offset, line in enumerate(buf[row:]):
            if not LineProperties(line).is_diffscuss:
                break
        return row + offset, col
    return row, col


def find_range(buf, (row, col)):
    """
    Returns the row of the next diff range line, if one could be found, and the
    current column. If none was found, returns the current row and column.
    """
    for offset, line in enumerate(buf[row - 1:]):
        if LineProperties(line).is_diff_range:
            return row + offset, col
    return row, col


def make_comment(depth=1):
    """
    Returns a string using the values from `config()` for a comment of depth
    `depth`.
    """
    depth = max(depth, 1)
    header = '#' + '*' * depth
    body = '#' + '-' * depth

    fields = config()
    fields['date'] = time.strftime('%Y-%m-%dT%T%z')

    lines = [header]
    for field_name in ['author', 'email', 'date']:
        field_value = fields.get(field_name, 'Unknown')
        lines.append('%s %s: %s' % (header, field_name, field_value))
    lines.extend([header, body + ' ', body])
    return lines


def inject_comment(buf, (row, col), depth=1):
    """
    Injects a comment of depth `depth` at the current cursor position, and
    returns the position to which the cursor should be moved for editing.
    """
    lines = make_comment(depth=depth)
    buf.append(lines, row)
    return (row + len(lines) - 1, len(lines[-2]))


def insert_comment(buf, (row, col), depth=1):
    """
    Inserts a comment of depth `depth` at the end of the current thread (if
    there is one) or at the current position, and returns the position to which
    the cursor should be moved for editing.
    """
    row, col = find_thread_end(buf, (row, col))
    return inject_comment(buf, (row, col), depth=depth)


def insert_file_comment(buf, (row, col)):
    """
    Inserts a new comment at the top of the file or at the end of the existing
    top-level diffscuss thread, and returns the position to which the cursor
    should be moved for editing.
    """
    row, col = 0, 0
    if LineProperties(buf[0]).is_diffscuss:
        row, col = find_thread_end(buf, (row, col))
    return inject_comment(buf, (row, col))


def reply_to_comment(buf, (row, col)):
    """
    Inserts a new reply to the current diffscuss comment at a depth one greater
    than that comment, and returns the position to which the cursor should be
    moved for editing.
    """
    depth = LineProperties(buf[row - 1]).depth
    row, col = find_subthread_end(buf, (row, col))
    return inject_comment(buf, (row, col), depth=depth + 1)


def insert_contextual_comment(buf, (row, col)):
    """
    Inserts a comment based on the current context: file-level if the cursor is
    at the top of the file, a reply if positioned in a diffscuss comment, after
    the next range line if in diff metadata, or at the current cursor position
    if none of the previous conditions were met. Returns the position to which
    the cursor should be moved for editing.
    """
    if row == 1:
        return insert_file_comment(buf, (row, col))

    line_props = LineProperties(buf[row - 1])
    if line_props.is_diffscuss:
        return reply_to_comment(buf, (row, col))
    elif line_props.is_diff_meta:
        row, col = find_range(buf, (row, col))
        return insert_comment(buf, (row, col))

    return insert_comment(buf, (row, col))


### Showing source

def show_local_source(buf, (row, col)):
    """
    Returns the line number and path of the file corresponding to the change
    under the cursor.
    """
    cmd = """
        {diffscuss} find-local -i {buffer_name} {row}
        """.format(diffscuss=_get_script(),
                   buffer_name=buf.name, row=row)
    output = _get_output(cmd)
    filename, line = output.rsplit(' ', 1)
    return '+%s %s' % (line, filename)


def show_old_source(buf, (row, col), tempfile):
    """
    Writes the old version of the file to the path given by `tempfile`, and
    returns the line number corresponding to the change under the cursor.
    """
    return show_source(buf, (row, col), tempfile, {
        'marker': '---',
        'short': '-',
        'skip': '+',
        'range_pattern': '^@@ -(\d+)',
        'index_pattern': '^index ([a-f0-9]+)'
    })


def show_new_source(buf, (row, col), tempfile):
    """
    Writes the new version of the file to the path given by `tempfile`, and
    returns the line number corresponding to the change under the cursor.
    """
    return show_source(buf, (row, col), tempfile, {
        'marker': '+++',
        'short': '+',
        'skip': '-',
        'range_pattern': '^@@ -\d+,\d+ \+(\d+)',
        'index_pattern': '^index [a-f0-9]+\.+([a-f0-9]+)'
    })


def _get_source_file(buf, (row, col), marker):
    """
    Returns the source file name from a git diff for a line starting with the
    string `marker`, working backward from the current cursor position.
    """
    for line in reversed(buf[:row - 1]):
        if line.startswith(marker):
            match = re.search('\s*(a|b)\/(.*)', line)
            if match:
                return match.group(2)
    return None


def show_source(buf, (row, col), tempfile, conf):
    """
    Writes a version of the file to the path given by `tempfile`, and returns
    the line number corresponding to the change under the cursor, as configured
    by `conf`. (See `show_old_source` and `show_new_source`.)
    """
    filename = _get_source_file(buf, (row, col), conf['marker'])

    # Skip lines we don't care about (- if showing the new version of the file,
    # + if showing the old version), so they don't mess up our final line
    # number.
    for skip_offset, line in enumerate(reversed(buf[:row - 1])):
        if not line.startswith(conf['skip']):
            break

    # Work backward to the nearest range line, counting how many lines we
    # had to move through to get there.
    range_line = None
    offset = 0
    for line in reversed(buf[:row - 1 - skip_offset]):
        props = LineProperties(line)
        if props.is_diff_range:
            range_line = line
            break
        if line.startswith((' ', conf['short'])):
            offset += 1

    if not range_line:
        raise Exception('No range line')

    # Extract the line number from the range line and add our offset to compute
    # the line number we'll want to show.
    match = re.search(conf['range_pattern'], range_line)
    lineno = offset + int(match.group(1))

    # Find the git revision from the index line.
    rev = None
    for line in reversed(buf[:row - 1]):
        match = re.search(conf['index_pattern'], line)
        if match:
            rev = match.group(1)
            break

    if not rev:
        raise Exception('No revision')

    # Use git to dump the version of the file at that revision, and return the
    # line number.
    cmd = """
        git show {rev}:{filename} > {tempfile}
        """.format(rev=rev, filename=filename, tempfile=tempfile)
    _get_output(cmd)
    return lineno


### Shelling out

def _get_script():
    """
    Returns the path to the diffscuss CLI executable, relative to the
    installation directory.
    """
    return os.path.join(config()['diffscuss_dir'], 'bin', 'diffscuss')


def _get_output(command):
    """
    Runs `command` in a shell and returns its output (from stdout), or raises
    an exception if the command failed.

    (Used instead of `check_output` for pre-2.7 compatibility.)
    """
    proc = subprocess.Popen(command, shell=True, stdout=subprocess.PIPE)
    output = proc.communicate()[0]
    if proc.wait() != 0:
        raise Exception('"%s" failed with status %d' % (command,
                                                        proc.returncode))
    return output


### Mailboxes

def mailbox_check(_buffer, _cursor, tempfile):
    """
    Writes the output of the mailbox check script to `tempfile` for preview
    display.
    """
    cmd = """
        {diffscuss} mailbox check > {tempfile}
        """.format(diffscuss=_get_script(),
                   tempfile=tempfile)
    _get_output(cmd)


def mailbox_post(buffer_name, prompt_func):
    """
    Queries the user for a list of recipients to post the review to and calls
    the mailbox post script.
    """
    recips = prompt_func('Post to: ')
    cmd = """
        {diffscuss} mailbox post -p {diffscuss_file} {recips}
        """.format(diffscuss=_get_script(),
                   diffscuss_file=buffer_name,
                   recips=recips)
    result = _get_output(cmd)
    return 'Posted %s' % result


def mailbox_bounce(buffer_name, prompt_func):
    """
    Queries the user for a list of recipients to post the review to and calls
    the mailbox bounce script.
    """
    recips = prompt_func('Bounce to: ')
    cmd = """
        {diffscuss} mailbox bounce -p {diffscuss_file} {recips}
        """.format(diffscuss=_get_script(),
                   diffscuss_file=buffer_name,
                   recips=recips)
    result = _get_output(cmd)
    return 'Bounced %s' % result


def mailbox_done(buffer_name, _prompt_func):
    """
    Calls the mailbox done script.
    """
    cmd = """
        {diffscuss} mailbox done -p {diffscuss_file}
        """.format(diffscuss=_get_script(),
                   diffscuss_file=buffer_name)
    result = _get_output(cmd)
    return 'Completed %s' % result


### Navigation

def _find_first(buf, (row, col), predicate, reverse=False):
    """
    Finds the first row for which the predicate (a function called on a single
    line) goes from False to True, moving forward or backward through the file
    according to `reverse`. Returns that row and the current column.
    """
    if not reverse:
        skip_gen = enumerate(buf[row:])
    else:
        skip_gen = enumerate(reversed(buf[:row - 1]))

    # Skip rows where the predicate returns False.
    skip_offset = 0
    for skip_offset, line in skip_gen:
        if not predicate(line):
            break

    if not reverse:
        search_gen = enumerate(buf[row + skip_offset:])
        factor = 1
    else:
        search_gen = enumerate(reversed(buf[:row - 1 - skip_offset]))
        factor = -1

    # Move through the file until we find that the predicate returns True.
    for offset, line in search_gen:
        if predicate(line):
            return row + factor * (1 + offset + skip_offset), col
    return row, col


def _find_last(buf, (row, col), predicate, reverse=False):
    """
    Finds the row for which the predicate (a function called on a single line)
    goes from True to False, moving forward or backward through the file
    according to `reverse`. Returns the row before that row (i.e. the last
    predicate-matching row) and the current column.
    """
    if not reverse:
        skip_gen = enumerate(buf[row:])
    else:
        skip_gen = enumerate(reversed(buf[:row - 1]))

    # Skip rows until the predicate returns False. If we didn't skip any, don't
    # keep track of the offset.
    skipped = False
    for skip_offset, line in skip_gen:
        if predicate(line):
            skipped = True
            break
    if not skipped:
        skip_offset = 0

    if not reverse:
        search_gen = enumerate(buf[row + skip_offset:])
        factor = 1
    else:
        search_gen = enumerate(reversed(buf[:row - 1 - skip_offset]))
        factor = -1

    # Move through the file until we find that the predicate returns False.
    for offset, line in search_gen:
        if not predicate(line):
            return row + factor * (offset + skip_offset), col
    return row, col


def find_next_comment(buf, (row, col)):
    """
    Returns the row of the start of the next diffscuss comment, and the current
    column.
    """
    predicate = lambda line: LineProperties(line).is_header
    return _find_first(buf, (row, col), predicate, reverse=False)


def find_next_comment_end(buf, (row, col)):
    """
    Returns the row of the end of the next diffscuss comment, and the current
    column.
    """
    predicate = lambda line: LineProperties(line).is_body
    return _find_last(buf, (row, col), predicate, reverse=False)


def find_next_thread(buf, (row, col)):
    """
    Returns the row of the start of the next diffscuss thread, and the current
    column.
    """
    predicate = lambda line: LineProperties(line).is_diffscuss
    return _find_first(buf, (row, col), predicate, reverse=False)


def find_next_thread_end(buf, (row, col)):
    """
    Returns the row of the end of the next diffscuss thread, and the current
    column.
    """
    predicate = lambda line: LineProperties(line).is_diffscuss
    return _find_last(buf, (row, col), predicate, reverse=False)


def find_prev_comment(buf, (row, col)):
    """
    Returns the row of the start of the previous diffscuss comment, and the
    current column.
    """
    predicate = lambda line: LineProperties(line).is_header
    return _find_last(buf, (row, col), predicate, reverse=True)


def find_prev_comment_end(buf, (row, col)):
    """
    Returns the row of the end of the previous diffscuss comment, and the
    current column.
    """
    predicate = lambda line: LineProperties(line).is_body
    return _find_first(buf, (row, col), predicate, reverse=True)


def find_prev_thread(buf, (row, col)):
    """
    Returns the row of the start of the previous diffscuss thread, and the
    current column.
    """
    predicate = lambda line: LineProperties(line).is_diffscuss
    return _find_last(buf, (row, col), predicate, reverse=True)


def find_prev_thread_end(buf, (row, col)):
    """
    Returns the row of the end of the previous diffscuss thread, and the
    current column.
    """
    predicate = lambda line: LineProperties(line).is_diffscuss
    return _find_first(buf, (row, col), predicate, reverse=True)
