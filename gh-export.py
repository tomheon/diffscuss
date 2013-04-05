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

from diffscuss.walker import walk, DIFF, DIFF_HEADER, COMMENT_HEADER, \
    COMMENT_BODY


def _echo(line):
    return lambda: line


def _compose(line_func_one, line_func_two):
    return lambda: u''.join([line_func_one(), line_func_two()])


class DiffscussComposer(object):
    """
    Allows us to insert content into the original diff without
    changing the line numbers, so we can sync up positions in the
    diffscuss with positions in the diff even after adding comments.
    """

    def __init__(self, orig_diff):
        self.orig_diff = orig_diff
        self.composed_lines = []
        self.top_matter = _echo('')

        # we want to maintain things like trailing newline or not, so
        # use readline instead of the line iterators
        diff_s = StringIO(orig_diff)
        line = diff_s.readline()

        while line != u'':
            self.composed_lines.append(_echo(line))
            line = diff_s.readline()

    def append_at(self, index, text):
        if index == -1:
            self.top_matter = _compose(self.top_matter,
                                       _echo(text))
        elif index >= 0:
            self.composed_lines[index] = _compose(self.composed_lines[index],
                                                  _echo(text))
        else:
            raise Exception("Index must be >= -1.")

    def render(self):
        yield self.top_matter()
        for line_func in self.composed_lines:
            yield line_func()


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


def _get_diff_text(username, password, pull_request):
    resp = requests.get(pull_request.diff_url, auth=(username, password))
    if not resp.ok:
        raise Exception("Error pulling %s: %s" % (pull_request.diff_url,
                                                  resp))
    return resp.text


def _gh_time_to_diffscuss_time(gh_time):
    # gh times are all zulu, and come in w/out timezones through
    # PyGithub, so we hardcode the offset
    return unicode(gh_time.strftime("%Y-%m-%dT%T-0000"))


def _make_header_line(depth, header, value):
    if header is None:
        return u"%%%s \n" % (u'*' * depth)
    return u"%%%s %s: %s\n" % (u'*' * depth, header, value)


def _make_body_line(depth, body):
    return u"%%%s %s\n" % (u'-' * depth, body)


def _make_comment(depth, body, headers):
    header_lines = [_make_header_line(depth, None, None)]
    body_lines = []
    for header, value in headers:
        header_lines.append(_make_header_line(depth, header, value))
    header_lines.append(_make_header_line(depth, None, None))
    body_s = StringIO(body)
    wrap_body_lines_at = 80 - depth - 2 # 2 for the % and space just
    # in case there's some amazingly deep test or something.
    wrap_body_lines_at = max(wrap_body_lines_at, 40)
    for body_line in body_s:
        body_line = body_line.strip()
        if body_line:
            for wrapped_body_line in wrap(body_line.rstrip(),
                                          width=wrap_body_lines_at):
                body_lines.append(_make_body_line(depth, wrapped_body_line))
        else:
            body_lines.append(_make_body_line(depth, u''))
    body_lines.append(_make_body_line(depth, u''))
    return u''.join(header_lines + body_lines)


def _overlay_pr_top_level(composer, gh, pull_request):
    """
    At the top of the diffscuss file, build a thread out of:

    - the title and initial body of the pull request
    - the comments on the associated issue
    """
    init_comment = _make_comment(
        depth=1,
        body=(pull_request.title + u"\n\n" + pull_request.body),
        headers=[(u'author', pull_request.user.login),
                 (u'email', pull_request.user.email),
                 (u'date', _gh_time_to_diffscuss_time(pull_request.created_at)),
                 (u'x-github-pull-request-url', pull_request.url),
                 (u'x-github-updated-at',
                  _gh_time_to_diffscuss_time(pull_request.updated_at)),])
    init_thread = init_comment + _make_thread(sorted(list(pull_request.get_issue_comments()),
                                                     key=lambda ic: ic.created_at),
                                              init_offset=1)
    composer.append_at(-1, init_thread)


def _overlay_pr_comments(composer, pull_request):
    """
    Get the inline comments into the diffscuss file (github makes
    these contextual comments available as "review comments" as
    opposed to "issue comments."
    """
    _overlay_comments(composer,
                      pull_request.get_review_comments())


def _overlay_comments(composer, comments):
    get_path = lambda rc: rc.path
    for (path, path_comments) in itertools.groupby(sorted(list(comments),
                                                          key=get_path),
                                                   get_path):
        if path:
            _overlay_path_comments(composer, path, path_comments)
        else:
            # if there's no path, make a new thread at the top of the
            # review.
            _overlay_review_level_comments(composer, path_comments)


def _overlay_review_level_comments(composer, comments):
    thread = _make_thread(sorted(list(comments),
                                 key=lambda ic: ic.created_at))
    # note that we're assuming here that the pr thread has already
    # been created.
    composer.append_at(-1, thread)


def _is_range_line(tagged_line):
    return tagged_line[0] == DIFF_HEADER and tagged_line[1].startswith(u'@@')


def _path_match(diff_header_line, path):
    return diff_header_line.startswith((u'--- a/%s' % path,
                                        u'+++ b/%s' % path))


def _is_target_path(tagged_line, path):
    return tagged_line[0] == DIFF_HEADER and _path_match(tagged_line[1],
                                                         path)


def _find_base_target_idx(orig_diff, path):
    looking_for_range_line = False

    for (i, tagged_line) in enumerate(walk(StringIO(orig_diff))):
        assert(tagged_line[0] not in [COMMENT_HEADER, COMMENT_BODY])
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
            headers=[(u'author', gh_comment.user.login),
                     (u'email', gh_comment.user.email),
                     (u'date', _gh_time_to_diffscuss_time(gh_comment.created_at)),
                     (u'x-github-comment-url', gh_comment.url),
                     (u'x-github-updated-at',
                      _gh_time_to_diffscuss_time(gh_comment.updated_at)),])
        comments.append(comment)
    return u''.join(comments)


def _overlay_path_comments(composer, path, path_comments):
    base_target_idx = _find_base_target_idx(composer.orig_diff, path)
    if base_target_idx is None:
        # TODO log better
        print "Couldn't find target for path %s (likely outdated diff)" % path
        return

    get_position = lambda pc: pc.position
    for (position, position_comments) in itertools.groupby(sorted(list(path_comments),
                                                                  key=get_position),
                                                           get_position):
        if position is None:
            # TODO: warn that we're skipping outdated information
            continue
        target_idx = base_target_idx + position
        composer.append_at(target_idx,
                           _make_thread(sorted(list(position_comments),
                                               key=lambda pc: pc.created_at)))


def _overlay_encoding(composer):
    composer.append_at(-1, u"# -*- coding: utf-8 -*-\n")


def _safe_get_commit(repo, sha):
    try:
        return repo.get_commit(sha)
    except GithubException, e:
        if e.status == 404:
            return None
        else:
            raise


def _overlay_commit_comments(composer, pull_request):
    for commit in pull_request.get_commits():
        # the commit comments seem generally to be in the head, but
        # let's make sure.
        for part in [pull_request.base, pull_request.head]:
            repo = part.repo
            repo_commit = _safe_get_commit(repo, commit.sha)
            if repo_commit:
                _overlay_comments(composer, repo_commit.get_comments())


def _export_to_diffscuss(gh, username, password, user_or_org, repo, pull_request, output_dir):
    diff_text = _get_diff_text(username, password, pull_request)
    composer = DiffscussComposer(diff_text)
    _overlay_encoding(composer)
    _overlay_pr_top_level(composer, gh, pull_request)
    _overlay_pr_comments(composer, pull_request)
    _overlay_commit_comments(composer, pull_request)

    dest_dir = os.path.join(output_dir, user_or_org.login, repo.name)
    _mkdir_p(dest_dir)
    dest_fname = os.path.join(dest_dir, u"%s.diffscuss" % pull_request.number)
    dest_fname_partial = u"%s.partial" % dest_fname

    with open(dest_fname_partial, 'wb') as dest_fil:
        for line in composer.render():
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
