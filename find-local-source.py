#!/usr/bin/env python

from optparse import OptionParser
import os
import re
import sys
from textwrap import dedent

from diffscuss.walker import walk, DIFF, COMMENT, DIFF_HEADER

# TODO: handle weirdness when someone chooses a line in a diff header

DEFAULT = '-'
NO_GIT_DIR = 2
CANNOT_FIND_CANDIDATES = 3


def _exit(msg, exit_code):
    print >> sys.stderr, msg
    sys.exit(exit_code)


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
    return os.path.isdir(os.path.join(directory, '.git'))


def _find_git_repo(starting_dir):
    cur_dir = os.path.abspath(starting_dir)
    while cur_dir != os.path.dirname(cur_dir):
        if _has_git_dir(cur_dir):
            return cur_dir
        cur_dir = os.path.dirname(cur_dir)
    return None


def _update_line_num(item, line_num):
    if item[0] == COMMENT:
        return line_num + len(item[1].header_lines) + len(item[1].body_lines)
    else:
        return line_num + 1


FNAME_RE = re.compile(r'^(---|\+\+\+) (.*)')
REL_FNAME_RE = re.compile(r'^(a|b)/(.*)')


def _parse_fname(line):
    line = line.strip()
    fname = FNAME_RE.match(line).group(2)
    rel_match = REL_FNAME_RE.match(fname)
    if rel_match:
        return rel_match.group(2)
    else:
        return fname


def _maybe_update_fname(item, prefix, cur_fname):
    if item[0] == DIFF_HEADER and item[1].startswith(prefix):
        return _parse_fname(item[1])
    else:
        return cur_fname


def _maybe_update_old_fname(item, cur_old_fname):
    return _maybe_update_fname(item, '---', cur_old_fname)


def _maybe_update_new_fname(item, cur_new_fname):
    return _maybe_update_fname(item, '+++', cur_new_fname)


RANGE_RE = re.compile(r'^@@ -(\d+),\d+ \+(\d+),\d+ @@')


def _line_applies(item, old_or_new):
    if old_or_new == 'old':
        exclude = '+'
    else:
        exclude = '-'

    if item[0] == DIFF and not item[1].startswith(exclude):
        return 1
    else:
        return 0


def _maybe_update_diff_line_num(item, cur_line_num, old_or_new):
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
    return _maybe_update_diff_line_num(item, cur_old_line_num, 'old')


def _maybe_update_new_line_num(item, cur_new_line_num):
    return _maybe_update_diff_line_num(item, cur_new_line_num, 'new')


def _safe_decr(line_num):
    if line_num is not None:
        return line_num - 1


def _maybe_update_old_line(item, cur_old_line):
    if _line_applies(item, 'old'):
        return item[1]
    else:
        return cur_old_line


def _maybe_update_new_line(item, cur_new_line):
    if _line_applies(item, 'new'):
        return item[1]
    else:
        return cur_new_line


def _find_candidates(input_f, line_number):
    cur_old_fname = None
    cur_new_fname = None
    cur_old_line_num = None
    cur_new_line_num = None
    cur_line_num = 0
    cur_old_line = None
    cur_new_line = None

    for item in walk(input_f):
        cur_old_fname = _maybe_update_old_fname(item, cur_old_fname)
        cur_new_fname = _maybe_update_new_fname(item, cur_new_fname)
        cur_old_line_num = _maybe_update_old_line_num(item, cur_old_line_num)
        cur_new_line_num = _maybe_update_new_line_num(item, cur_new_line_num)
        cur_line_num = _update_line_num(item, cur_line_num)
        cur_old_line = _maybe_update_old_line(item, cur_old_line)
        cur_new_line = _maybe_update_new_line(item, cur_new_line)

        if cur_line_num >= line_number:
            return ((cur_new_fname,
                      _safe_decr(cur_new_line_num),
                      cur_new_line),
                    (cur_old_fname,
                     _safe_decr(cur_old_line_num),
                     cur_old_line))

    return tuple()


def _candidate_exists(candidate_fname, git_repo):
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
    for candidate_fname, line_num, line in candidates:
        if _candidate_exists(candidate_fname, git_repo):
            return (candidate_fname, line_num, line)
    return None


def _closest_line_num(fil, orig_line_num, orig_line):
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


def _best_line_num_in_candidate(best_candidate, git_repo):
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

    best_line = _best_line_num_in_candidate(best_candidate, git_repo)
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
