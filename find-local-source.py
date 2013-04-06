#!/usr/bin/env python

"""
A utility for guessing which file and line of a local source repo are
the best referent for a line in a diffscuss file.

Intended mainly for use in editors to facilitate jumping to source
from a diffscuss file.
"""

from optparse import OptionParser
import os
import re
import sys
from textwrap import dedent

from diffscuss.walker import walk, DIFF, DIFF_HEADER


# exit codes for various error conditions
NO_GIT_DIR = 2
CANNOT_FIND_CANDIDATES = 3


def _exit(msg, exit_code):
    print >> sys.stderr, msg
    sys.exit(exit_code)


# used to indicate some default args
DEFAULT = '-'


def _fil_or_default(fname, default_f):
    if fname == DEFAULT:
        return default_f, False
    else:
        return open(fname, 'rb'), True


def _str_or_default(s, default_s):
    if s == DEFAULT:
        return default_s
    else:
        return s


def _has_git_dir(directory):
    """
    True if @directory exists and contains a '.git' subdir.
    """
    return os.path.isdir(os.path.join(directory, '.git'))


def _find_git_repo(starting_dir):
    """
    Starting from @starting_dir, find the lowest directory which
    represents the top of a git repo (that is, which contains a '.git'
    subdir).

    If no such directory is found before walking up to the root
    directory, return None.
    """
    cur_dir = os.path.abspath(starting_dir)
    while cur_dir != os.path.dirname(cur_dir):
        if _has_git_dir(cur_dir):
            return cur_dir
        cur_dir = os.path.dirname(cur_dir)
    return None


FNAME_RE = re.compile(r'^(---|\+\+\+) (.*)')
REL_FNAME_RE = re.compile(r'^(a|b)/(.*)')


def _parse_fname(line):
    """
    Parse the file name out of a git diff @line, stripping the leading
    a/ or b/ if it's present.
    """
    line = line.strip()
    fname = FNAME_RE.match(line).group(2)
    rel_match = REL_FNAME_RE.match(fname)
    if rel_match:
        return rel_match.group(2)
    else:
        return fname


def _maybe_update_fname(item, prefix, cur_fname):
    """
    If @item is a diff header which starts with @prefix, parse and
    return the file name from it.

    Otherwise, return cur_fname.
    """
    if item[0] == DIFF_HEADER and item[1].startswith(prefix):
        return _parse_fname(item[1])
    else:
        return cur_fname


def _maybe_update_old_fname(item, cur_old_fname):
    """
    If @item is a diff header which starts with ---, parse and return
    the file name from it.

    Otherwise, return cur_fname.
    """
    return _maybe_update_fname(item, '---', cur_old_fname)


def _maybe_update_new_fname(item, cur_new_fname):
    """
    If @item is a diff header which starts with +++, parse and return
    the file name from it.

    Otherwise, return cur_fname.
    """
    return _maybe_update_fname(item, '+++', cur_new_fname)


RANGE_RE = re.compile(r'^@@ -(\d+),\d+ \+(\d+),\d+ @@')


def _line_applies(item, old_or_new):
    """
    Return 1 if the line in @item applies in the context of
    @old_or_new, else 0.

    A line applies in the case that it's a DIFF line and:

    - if @old_or_new is 'old', the diff line doesn't begin with +

    - if @old_or_new is 'new', the diff line doesn't begin with -
    """
    if old_or_new == 'old':
        exclude = '+'
    else:
        exclude = '-'

    if item[0] == DIFF and not item[1].startswith(exclude):
        return 1
    else:
        return 0


def _maybe_update_diff_line_num(item, cur_line_num, old_or_new):
    """
    Return the possibly adjusted @cur_line_num, where the adjustment
    rules are:

    - bumping it by 1 if @item represents a diff line in the
      @old_or_new context.

    - if @item represents a range line in a diff header, parsing out
      the appropriate line number for the @old_or_new context
    """
    if old_or_new == 'old':
        group_num = 1
    else:
        group_num = 2

    if item[0] == DIFF_HEADER:
        match = RANGE_RE.match(item[1])
        if match:
            return int(match.group(group_num))

    if item[0] == DIFF:
        return cur_line_num + _line_applies(item, old_or_new)
    else:
        return cur_line_num


def _maybe_update_old_line_num(item, cur_old_line_num):
    """
    Return @cur_old_line_num, possibly bumped by one if @item is a an
    old line.
    """
    return _maybe_update_diff_line_num(item, cur_old_line_num, 'old')


def _maybe_update_new_line_num(item, cur_new_line_num):
    """
    Return @cur_new_line_num, possibly bumped by one if @item is a a
    new line.
    """
    return _maybe_update_diff_line_num(item, cur_new_line_num, 'new')


def _safe_decr(line_num):
    """
    Return @line_num decremented by 1, if @line_num is non None, else
    None.
    """
    if line_num is not None:
        return line_num - 1


def _maybe_update_old_line(item, cur_old_line):
    """
    Return the line from @item if it's an old line, otherwise return
    @cur_old_line.
    """
    if _line_applies(item, 'old'):
        return item[1]
    else:
        return cur_old_line


def _maybe_update_new_line(item, cur_new_line):
    """
    Return the line from @item if it's a new line, otherwise return
    @cur_new_line.
    """
    if _line_applies(item, 'new'):
        return item[1]
    else:
        return cur_new_line


def _find_candidates(input_f, line_number):
    """
    If @line_number is a line number in the diffscuss file in
    @input_f, return a tuple of two tuples of form:

    (file name, line number, line text)

    One for the new version of the source in the diffscuss file, and
    one for the old version of the source in the diffscuss file.

    The new version is always listed first.

    If line_number ends up being greater than the number of lines in
    input_f, returns an empty tuple.
    """
    cur_old_fname = None
    cur_new_fname = None
    cur_old_line_num = None
    cur_new_line_num = None
    cur_old_line = None
    cur_new_line = None

    for (index, item) in enumerate(walk(input_f)):
        # walk the diffscuss file line by line, maintaing the updated
        # file names, line numbers, and line texts for both the old
        # and new versions of the source
        cur_old_fname = _maybe_update_old_fname(item, cur_old_fname)
        cur_new_fname = _maybe_update_new_fname(item, cur_new_fname)
        cur_old_line_num = _maybe_update_old_line_num(item, cur_old_line_num)
        cur_new_line_num = _maybe_update_new_line_num(item, cur_new_line_num)
        cur_old_line = _maybe_update_old_line(item, cur_old_line)
        cur_new_line = _maybe_update_new_line(item, cur_new_line)
        cur_line_num = index + 1

        # once we've walked past the line number in the diffscuss
        # file, maintaining the file / line number context for both
        # old and new versions of the source as we went, we have our
        # candidates for matching source.
        if cur_line_num >= line_number:
            return ((cur_new_fname,
                      _safe_decr(cur_new_line_num),
                      cur_new_line),
                    (cur_old_fname,
                     _safe_decr(cur_old_line_num),
                     cur_old_line))

    return tuple()


def _candidate_exists(candidate_fname, git_repo):
    """
    Return true if the @candidate_fname is not /dev/null, and exists
    under git_repo.
    """
    # this is how git diffs represent a file that didn't exist in an
    # earlier revision
    if candidate_fname == '/dev/null':
        return False
    try:
        with open(os.path.join(git_repo, candidate_fname), 'rb'):
            return True
    except IOError:
        return False


def _best_candidate(candidates, git_repo):
    """
    Given @candidates, a collection of tuples of

    (fname, line num, line)

    scraped from a diffscuss file, return the best candidate.

    Currently this means always returning the first candidate whose
    fname actually exists in local source.

    Since the candidates should always be listed with the new version
    of the source first, this means we always consider the new version
    if the file name is correct.  This covers the "commit then review"
    case well, but could stand to be blown up and improved to better
    cover the "review then commit" case.
    """
    for candidate_fname, line_num, line in candidates:
        if _candidate_exists(candidate_fname, git_repo):
            return (candidate_fname, line_num, line)
    return None


def _closest_line_num(fil, orig_line_num, orig_line):
    """
    Find the line in @fil that best matches the @orig_line found at
    @orig_line_num.

    This is currently done by:

    - finding all the lines in @fil that, when stripped, match the
      @orig_line exactly

    - return the number the matching line with the smallest absolutely
      distance from @orig_line_num

    - if no matching lines are found, returning @orig_line_num

    This works decently for a broad number of cases, but could also be
    improved for cases in which the @orig_line has subsequently been
    modified.
    """
    # skip the first char, which is either +, -, or ' ', since
    # orig_line is a diff line
    orig_line = orig_line[1:].strip()
    matching_line_nums = []

    for ind, line in enumerate(fil):
        line = line.strip()
        if orig_line == line:
            matching_line_nums.append(ind + 1)

    if not matching_line_nums:
        return orig_line_num

    matching_line_nums = [(abs(line_num - orig_line_num), line_num)
                          for line_num
                          in matching_line_nums]
    matching_line_nums.sort()

    return matching_line_nums[0][1]


def _best_line_num_for_candidate(best_candidate, git_repo):
    """
    Given @best_candidate, a tuple of fname, line num, and line text,
    find the closest line matching that candidate in on-disk source.
    """
    candidate_fname, candidate_line_num, candidate_line = best_candidate
    with open(os.path.join(git_repo, candidate_fname), 'rU') as fil:
        return _closest_line_num(fil, candidate_line_num, candidate_line)


def main(directory, input_fname, output_fname, line_number):
    input_f, close_input = _fil_or_default(input_fname, sys.stdin)
    output_f, close_output = _fil_or_default(output_fname, sys.stdout)
    directory = _str_or_default(directory, os.getcwd())

    git_repo = _find_git_repo(directory)
    if git_repo is None:
        _exit("Cannot find git repo at or above %s" % directory, NO_GIT_DIR)

    candidates = _find_candidates(input_f, line_number)
    best_candidate = _best_candidate(candidates, git_repo)
    if not best_candidate:
        _exit("Cannot find any candidate files.", CANNOT_FIND_CANDIDATES)

    best_line = _best_line_num_for_candidate(best_candidate, git_repo)
    output_f.write("%s %d" % (os.path.join(git_repo,
                                           best_candidate[0]), best_line))

    if close_input:
        input_f.close()
    if close_output:
        output_f.close()


if __name__ == '__main__':
    parser = OptionParser(usage=dedent(
        """\
        %prog [options] line_num

        Accepts a diffscuss file (either through the -i argument or
        stdin) and a line number within that file, and outputs,
        on either the file specified by -o or stdout, the path
        and line number of the best-guess local source file.

        The output format is

        file_path line_number

        The line number is 1 based.

        If no source can be found, a blank file name and -1 for the line.
        """))
    parser.add_option('-i', '--input-file', dest='input_fname',
                      default='-',
                      help="Diffscuss file to read, stdin if not provided or -")
    parser.add_option('-o', '--output-file', dest='output_fname',
                      default='-',
                      help="File to write results to, stdout if not provided or -")
    parser.add_option('-d', '--directory', dest='directory',
                      default='-',
                      help='Directory in which to start searching for the local source, cwd if not provided or -.')

    opts, args = parser.parse_args()

    if len(args) != 1:
        parser.error("Must provide (exactly one) line number.")

    main(directory=opts.directory,
         input_fname=opts.input_fname,
         output_fname=opts.output_fname,
         line_number=int(args[0]))
