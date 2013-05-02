"""
A utility for guessing which file and line of a local source repo are
the best referent for a line in a diffscuss file.

Intended mainly for use in editors to facilitate jumping to source
from a diffscuss file.
"""
from collections import namedtuple
from optparse import OptionParser
import os
import re
import sys
from textwrap import dedent

from diffscuss.walker import walk, DIFF, DIFF_HEADER


# exit codes for various error conditions
NO_GIT_DIR = 2
CANNOT_FIND_CANDIDATES = 3


Candidate = namedtuple('Candidate', 'fname line_num line_text')


LocalCandidate = namedtuple('LocalCandidate',
                            'found_match local_line_num candidate')


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
    @input_f, return a list of two Candidate tuples:

    - one for the new version of the source in the diffscuss file

    - one for the old version of the source in the diffscuss file

    The new version is always listed first.

    If line_number ends up being greater than the number of lines in
    input_f, returns an empty list.
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
            return [Candidate(fname=cur_new_fname,
                              line_num=_safe_decr(cur_new_line_num),
                              line_text=cur_new_line),
                    Candidate(fname=cur_old_fname,
                              line_num=_safe_decr(cur_old_line_num),
                              line_text=cur_old_line)]

    return list()


def _candidate_exists(candidate, git_repo):
    """
    Return true if the @candidate.fname is not /dev/null, and exists
    under git_repo.
    """
    # this is how git diffs represent a file that didn't exist in an
    # earlier revision
    if not candidate.fname or candidate.fname == '/dev/null':
        return False
    try:
        with open(os.path.join(git_repo, candidate.fname), 'rb'):
            return True
    except IOError:
        return False


def _best_local_candidate(local_candidates, git_repo):
    """
    Given @local_candidates, a list of LocalCandidate named tuples,
    scraped from a diffscuss file, return the best candidate.

    The best candidate is:

    * the earliest candidate in the list where the matching line was
      found

    * or the earliest candidate in the list, if none of the matching
      lines were found
    """
    best_candidate = None

    for candidate in local_candidates:
        if best_candidate is None:
            best_candidate = candidate
        elif candidate.found_match and not best_candidate.found_match:
            best_candidate = candidate

    return best_candidate


def _closest_line_num(fil, orig_line_num, orig_line):
    """
    Find the line in @fil that best matches the @orig_line found at
    @orig_line_num.

    This is currently done by:

    - finding all the lines in @fil that, when stripped, match the
      @orig_line exactly

    - returning the number the matching line with the smallest
      absolutely distance from @orig_line_num, in a tuple (True,
      line_num)

    - if no matching lines are found, returning (False, @orig_line_num)

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
        if orig_line and orig_line == line:
            matching_line_nums.append(ind + 1)

    if not matching_line_nums:
        return (False, orig_line_num)

    matching_line_nums = [(abs(line_num - orig_line_num), line_num)
                          for line_num
                          in matching_line_nums]
    matching_line_nums.sort()

    return (True, matching_line_nums[0][1])


def _localize_candidate(candidate, git_repo):
    """
    Given @candidate, return a LocalCandidate, which includes the best
    guess for a local_line_num matching the line_text in @candidate,
    and whether or not the local line matches the text exactly.
    """
    with open(os.path.join(git_repo, candidate.fname), 'rU') as fil:
        (found_match, local_line_num) = _closest_line_num(fil,
                                                          candidate.line_num,
                                                          candidate.line_text)
        return LocalCandidate(found_match=found_match,
                              local_line_num=local_line_num,
                              candidate=candidate)


def main(args):
    directory = args.directory
    input_fname = args.input_file
    output_fname = args.output_file
    line_number = args.line_number
    input_f, close_input = _fil_or_default(input_fname, sys.stdin)
    output_f, close_output = _fil_or_default(output_fname, sys.stdout)
    directory = _str_or_default(directory, os.getcwd())

    git_repo = _find_git_repo(directory)
    if git_repo is None:
        _exit("Cannot find git repo at or above %s" % directory, NO_GIT_DIR)

    candidates = _find_candidates(input_f, line_number)
    existing_candidates = [c
                           for c
                           in candidates
                           if _candidate_exists(c, git_repo)]
    local_candidates = [_localize_candidate(c, git_repo)
                        for c
                        in existing_candidates]
    best_candidate = _best_local_candidate(local_candidates, git_repo)

    if not best_candidate:
        _exit("Cannot find any candidate files.", CANNOT_FIND_CANDIDATES)

    output_f.write("%s %d" % (os.path.join(git_repo,
                                           best_candidate.candidate.fname),
                              best_candidate.local_line_num))

    if close_input:
        input_f.close()
    if close_output:
        output_f.close()


