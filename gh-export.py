#!/usr/bin/env python

import errno
import itertools
from optparse import OptionParser
import os
import shutil
from StringIO import StringIO
import sys
from textwrap import dedent, wrap

from github import Github, GithubException
import requests

from diffscuss.walker import walk, COMMENT, DIFF, DIFF_HEADER


def _mkdir_p(path):
    try:
        os.makedirs(path)
    except OSError, e:
        if e.errno != errno.EEXIST or not os.path.isdir(path):
            raise


def _password(passfile):
    with open(passfile, 'rb') as passfil:
        return passfil.read().rstrip()


def _pull_requests_from_repo(repo):
    for state in ['open', 'closed']:
        for pull_request in repo.get_pulls(state=state):
            yield pull_request


def _user_or_org(gh, user_or_org_name):
    # note that gh will not return private users for an org, even if
    # the api user has access, unless you access the organization as
    # an organization, so first try to treat the name as an org, and
    # fall back to user.
    try:
        user_or_org = gh.get_organization(user_or_org_name)
    except GithubException, e:
        if e.status == 404:
            user_or_org = gh.get_user(user_or_org_name)
        else:
            raise
    return user_or_org


def _pull_requests_from_spec(gh, spec):
    if ':' in spec:
        repo_name, pr_id = spec.split(':')
        repo = gh.get_repo(repo_name)
        user_or_org = _user_or_org(gh, repo_name.split('/')[0])
        yield user_or_org, repo, repo.get_pull(int(pr_id))
    elif '/' in spec:
        repo = gh.get_repo(spec)
        user_or_org = _user_or_org(gh, spec.split('/')[0])
        for pull_request in _pull_requests_from_repo(repo):
            yield user_or_org, repo, pull_request
    else:
        user_or_org = _user_or_org(gh, spec)
        for repo in user_or_org.get_repos():
            for pull_request in _pull_requests_from_repo(repo):
                yield user_or_org, repo, pull_request


def _pull_requests_from_specs(gh, args):
    for spec in args:
        for user_or_org, repo, pull_request in _pull_requests_from_spec(gh, spec):
            yield user_or_org, repo, pull_request


def _echo(line):
    return lambda: line


def _diff_canvas(username, password, pull_request):
    resp = requests.get(pull_request.diff_url, auth=(username, password))
    if not resp.ok:
        raise Exception("Error pulling %s: %s" % (pull_request.diff_url,
                                                  resp))
    # we want to maintain things like trailing newline or not, so use
    # readline instead the line iterators
    diff_s = StringIO(resp.text)
    diff_canvas = []
    line = diff_s.readline()
    while line != '':
        diff_canvas.append(_echo(line))
        line = diff_s.readline()
    return diff_canvas, resp.text


def _render_canvas(diff_canvas, user_or_org, repo, pull_request, output_dir):
    for line_func in diff_canvas:
        yield line_func()


def _gh_time_to_diffscuss_time(gh_time):
    # gh times are all zulu, and come in w/out timezones through
    # PyGithub, so we hardcode the offset
    return gh_time.strftime("%Y-%m-%dT%T-0000")


def _make_header_line(depth, header, value):
    if header is None:
        return "%%%s \n" % ('*' * depth)
    return "%%%s %s: %s\n" % ('*' * depth, header, value)


def _make_body_line(depth, body):
    return "%%%s %s\n" % ('-' * depth, body)


def _make_comment(depth, body, headers):
    header_lines = [_make_header_line(depth, None, None)]
    body_lines = []
    for header, value in headers:
        header_lines.append(_make_header_line(depth, header, value))
    header_lines.append(_make_header_line(depth, None, None))
    body_s = StringIO(body)
    for body_line in body_s:
        body_line = body_line.strip()
        if body_line:
            for wrapped_body_line in wrap(body_line.rstrip()):
                body_lines.append(_make_body_line(depth, wrapped_body_line))
        else:
            body_lines.append(_make_body_line(depth, ''))
    body_lines.append(_make_body_line(depth, ''))
    return ''.join(header_lines + body_lines)


def _compose(line_func_one, line_func_two):
    return lambda: ''.join([line_func_one(), line_func_two()])


def _overlay_pr_top_level(diff_canvas, gh, pull_request):
    """
    At the top of the diffscuss file, build a thread out of:

    - the title and initial body of the pull request
    - the comments on the associated issue
    """
    init_comment = _make_comment(
        depth=1,
        body=(pull_request.title + "\n\n" + pull_request.body),
        headers=[('author', pull_request.user.login),
                 ('email', pull_request.user.email),
                 ('date', _gh_time_to_diffscuss_time(pull_request.created_at)),
                 ('x-github-pull-request-url', pull_request.url),
                 ('x-github-updated-at',
                  _gh_time_to_diffscuss_time(pull_request.updated_at)),])
    init_thread = _compose(_echo(init_comment),
                           _echo(_make_thread(pull_request.get_issue_comments(),
                                              init_offset=1)))
    diff_canvas[0] = _compose(init_thread, diff_canvas[0])


def _overlay_pr_comments(diff_canvas, pristine_canvas, pull_request):
    """
    Get the inline comments into the diffscuss file (github makes
    these contextual comments available as "review comments" as
    opposed to "issue comments."
    """
    for (path, path_comments) in itertools.groupby(pull_request.get_review_comments(),
                                                   lambda rc: rc.path):
        _overlay_path_comments(diff_canvas, pristine_canvas, path, path_comments)


def _is_range_line(tagged_line):
    return tagged_line[0] == DIFF_HEADER and tagged_line[1].startswith('@@')


def _path_match(diff_header_line, path):
    return diff_header_line.startswith(('--- a/%s' % path,
                                        '+++ b/%s' % path))


def _is_target_path(tagged_line, path):
    return tagged_line[0] == DIFF_HEADER and _path_match(tagged_line[1],
                                                         path)


def _find_base_target_idx(orig_diff, path):
    looking_for_range_line = False

    for (i, tagged_line) in enumerate(walk(StringIO(orig_diff))):
        assert(tagged_line[0] != COMMENT)
        if looking_for_range_line and _is_range_line(tagged_line):
            return i
        if _is_target_path(tagged_line, path):
            looking_for_range_line = True
    return None


def _make_thread(gh_comments, init_offset=0):
    comments = []
    for (i, gh_comment) in enumerate(gh_comments):
        comment = _make_comment(
            depth=i + 1 + init_offset,
            body=(gh_comment.body),
            headers=[('author', gh_comment.user.login),
                     ('email', gh_comment.user.email),
                     ('date', _gh_time_to_diffscuss_time(gh_comment.created_at)),
                     ('x-github-comment-url', gh_comment.url),
                     ('x-github-updated-at',
                      _gh_time_to_diffscuss_time(gh_comment.updated_at)),])
        comments.append(comment)
    return ''.join(comments)


def _overlay_path_comments(diff_canvas, orig_diff, path, path_comments):
    base_target_idx = _find_base_target_idx(orig_diff, path)
    if base_target_idx is None:
        # Until I figure out what circumstances this can happen in,
        # just blow up.
        raise Exception("Couldn't find target for path %s" % path)
    for (position, position_comments) in itertools.groupby(path_comments,
                                                           lambda pc: pc.position):
        if position is None:
            # TODO: warn that we're skipping outdated information
            continue
        canvas_idx = base_target_idx + position
        diff_canvas[canvas_idx] = _compose(
            diff_canvas[canvas_idx],
            _echo(_make_thread(sorted(list(position_comments),
                                      key=lambda pc: pc.created_at))))


def _export_to_diffscuss(gh, username, password, user_or_org, repo, pull_request, output_dir):
    diff_canvas, orig_diff = _diff_canvas(username, password, pull_request)
    pristine_canvas = list(diff_canvas)
    _overlay_pr_top_level(diff_canvas, gh, pull_request)
    _overlay_pr_comments(diff_canvas, orig_diff, pull_request)

    dest_dir = os.path.join(output_dir, user_or_org.login, repo.name)
    _mkdir_p(dest_dir)
    dest_fname = os.path.join(dest_dir, "%s.diffscuss" % pull_request.number)
    dest_fname_partial = "%s.partial" % dest_fname

    with open(dest_fname_partial, 'wb') as dest_fil:
        for line in _render_canvas(diff_canvas, user_or_org, repo,
                                   pull_request, output_dir):
            dest_fil.write(line.encode('utf-8'))

    shutil.move(dest_fname_partial, dest_fname)


def main(opts, args):
    password = _password(opts.passfile)
    gh = Github(opts.username, password)

    for user_or_org, repo, pull_request in _pull_requests_from_specs(gh, args):
        print >> sys.stderr, "Exporting %s/%s/%s" % (user_or_org.login,
                                                     repo.name,
                                                     pull_request.number)
        _export_to_diffscuss(gh, opts.username, password, user_or_org, repo, pull_request,
                             opts.output_dir)
    return 0


if __name__ == '__main__':
    parser = OptionParser(usage=dedent("""\
                                       %prog [options] pr_spec_1 [pr_spec_2 ...]

                                       Specify pull requests as either:

                                       * user_or_org/repo_name:pr_id

                                         to export a single pull request, or

                                       * user_or_org/repo_name

                                         to export all pull requests for a repo.

                                       * user_or_org

                                         to export all pull requests for a user or org.

                                       E.g.:

                                       hut8labs/diffscuss/15
                                       tomheon/git_by_a_bus
                                       mpapi
                                       """))
    parser.add_option("-p", "--passfile",
                      help="File containg github password to use for api",
                      dest="passfile")
    parser.add_option("-u", "--username",
                      help="Github user name to use for api",
                      dest="username")
    parser.add_option("-o", "--output",
                      help="Directory in which to put output (defaults to 'output')",
                      dest="output_dir", default="output")

    (opts, args) = parser.parse_args()

    if not opts.username:
        parser.error("Username is required.")

    if not opts.passfile:
        parser.error("Passfile is required.")

    if not args:
        parser.error("At least one pull request is required.")

    sys.exit(main(opts, args))
