#!/usr/bin/env python

import errno
import itertools
import logging
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
        logging.debug("Appending at index %s text %s",
                      index,
                      text)
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
    logging.debug("Finding all pull requests in repo %s",
                  repo.url)
    for state in ['open', 'closed']:
        logging.debug("Finding %s pull requests in repo %s",
                      state,
                      repo.url)
        for pull_request in repo.get_pulls(state=state):
            logging.debug("Found pull request %s",
                          pull_request.url)
            yield pull_request


def _user_or_org(gh, user_or_org_name):
    # note that gh will not return private users for an org, even if
    # the api user has access, unless you access the organization as
    # an organization, so first try to treat the name as an org, and
    # fall back to user.
    logging.debug("Finding user or org with name %s", user_or_org_name)
    try:
        logging.debug("Trying org %s", user_or_org_name)
        user_or_org = gh.get_organization(user_or_org_name)
        logging.debug("Found org %s", user_or_org_name)
    except GithubException, e:
        if e.status == 404:
            logging.debug("No org %s, trying user", user_or_org_name)
            user_or_org = gh.get_user(user_or_org_name)
        else:
            raise
    return user_or_org


def _pull_requests_from_spec(gh, spec):
    logging.info("Parsing spec %s", spec)
    if ':' in spec:
        repo_name, pr_id = spec.split(':')
        repo = gh.get_repo(repo_name)
        user_or_org = _user_or_org(gh, repo_name.split('/')[0])
        logging.info("Spec is for pr #%s in repo %s",
                     pr_id,
                     repo_name)
        yield user_or_org, repo, repo.get_pull(int(pr_id))
    elif '/' in spec:
        repo = gh.get_repo(spec)
        user_or_org = _user_or_org(gh, spec.split('/')[0])
        logging.info("Spec is for all prs in repo %s",
                     repo.url)
        for pull_request in _pull_requests_from_repo(repo):
            yield user_or_org, repo, pull_request
    else:
        user_or_org = _user_or_org(gh, spec)
        logging.info("Spec is for all prs in all repos for user/org %s",
                     spec)
        for repo in user_or_org.get_repos():
            for pull_request in _pull_requests_from_repo(repo):
                yield user_or_org, repo, pull_request


def _pull_requests_from_specs(gh, args):
    for spec in args:
        for user_or_org, repo, pull_request in _pull_requests_from_spec(gh, spec):
            logging.info("Found pull request %s",
                         pull_request.url)
            yield user_or_org, repo, pull_request


def _get_diff_text(username, password, pull_request):
    logging.info("Requesting diff from url %s",
                 pull_request.diff_url)
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
    logging.info("Overlaying top-level thread for pr %s",
                 pull_request.url)
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
    logging.debug("Init thread is %s", init_thread)
    composer.append_at(-1, init_thread)


def _overlay_pr_comments(composer, pull_request):
    """
    Get the inline comments into the diffscuss file (github makes
    these contextual comments available as "review comments" as
    opposed to "issue comments."
    """
    logging.info("Overlaying review comments for pr %s",
                 pull_request.url)
    _overlay_comments(composer,
                      pull_request.get_review_comments())


def _overlay_comments(composer, comments):
    comments = list(comments)
    logging.info("Overlaying %d total comments", len(comments))
    get_path = lambda rc: rc.path
    for (path, path_comments) in itertools.groupby(sorted(comments,
                                                          key=get_path),
                                                   get_path):
        if path:
            logging.info("Found path %s, will write path comments", path)
            _overlay_path_comments(composer, path, path_comments)
        else:
            logging.info("Could not find path, writing review level comments.")
            # if there's no path, make a new thread at the top of the
            # review.
            _overlay_review_level_comments(composer, path_comments)


def _overlay_review_level_comments(composer, comments):
    comments = list(comments)
    logging.info("Overlaying %d review level comments",
                 len(comments))
    thread = _make_thread(sorted(comments,
                                 key=lambda ic: ic.created_at))
    # note that we're assuming here that the pr thread has already
    # been created.
    logging.debug("Thread is %s", thread)
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
    logging.debug("Finding base target index for %s", path)
    looking_for_range_line = False

    for (i, tagged_line) in enumerate(walk(StringIO(orig_diff))):
        logging.debug("Checking at index %d tagged line %s",
                      i, tagged_line)
        assert(tagged_line[0] not in [COMMENT_HEADER, COMMENT_BODY])
        if looking_for_range_line and _is_range_line(tagged_line):
            logging.debug("Found range line at index %d", i)
            return i
        if _is_target_path(tagged_line, path):
            logging.debug("Found path %s at index %s, now looking for range line",
                          path, i)
            looking_for_range_line = True
    logging.info("Could not find path %s in diff", path)
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
    logging.info("Overlaying comments for path %s", path)
    base_target_idx = _find_base_target_idx(composer.orig_diff, path)
    logging.debug("Base target index for path %s is %s", path, base_target_idx)
    if base_target_idx is None:
        logging.warn("Couldn't find target for path %s (likely outdated diff)",
                     path)
        return

    get_position = lambda pc: pc.position
    for (position, position_comments) in itertools.groupby(sorted(list(path_comments),
                                                                  key=get_position),
                                                           get_position):
        position_comments = list(position_comments)
        if position is None:
            logging.info("Null position in path %s for %d comments (assuming outdated diff)",
                         path,
                         len(position_comments))
            continue

        logging.info("Writing %d comments for path %s at position %s.",
                     len(position_comments),
                     path, position)
        target_idx = base_target_idx + position
        composer.append_at(target_idx,
                           _make_thread(sorted(position_comments,
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
    logging.info("Overlaying all commit comments.")
    for commit in pull_request.get_commits():
        logging.info("Overlaying commit %s", commit.sha)
        # the commit comments seem generally to be in the head, but
        # let's make sure.
        for part in [pull_request.base, pull_request.head]:
            repo = part.repo
            logging.debug("Checking for commit in repo %s", repo.url)
            repo_commit = _safe_get_commit(repo, commit.sha)
            logging.debug("Found commit: %s", repo_commit)
            if repo_commit:
                logging.info("Will overlay commit %s from repo %s",
                             commit.sha,
                             repo.url)
                _overlay_comments(composer, repo_commit.get_comments())


def _import_to_diffscuss(gh, username, password, user_or_org, repo,
                         pull_request, output_dir):
    logging.info("Getting diff text for pull request %s", pull_request.url)
    diff_text = _get_diff_text(username, password, pull_request)
    composer = DiffscussComposer(diff_text)
    _overlay_encoding(composer)
    _overlay_pr_top_level(composer, gh, pull_request)
    _overlay_pr_comments(composer, pull_request)
    _overlay_commit_comments(composer, pull_request)

    dest_dir = os.path.join(output_dir, user_or_org.login, repo.name)
    logging.debug("Destination dir is %s", dest_dir)
    _mkdir_p(dest_dir)
    dest_fname = os.path.join(dest_dir, u"%s.diffscuss" % pull_request.number)
    logging.debug("Destination filename is %s", dest_fname)
    dest_fname_partial = u"%s.partial" % dest_fname
    logging.debug("Writing partial results to %s", dest_fname_partial)

    with open(dest_fname_partial, 'wb') as dest_fil:
        for line in composer.render():
            dest_fil.write(line.encode('utf-8'))

    logging.info("Moving final results to %s", dest_fname)
    shutil.move(dest_fname_partial, dest_fname)


def main(args):
    log_level = logging.INFO
    if args.debug:
        log_level = logging.DEBUG
    logging.basicConfig(filename=args.logfile, level=log_level,
                        format='[%(levelname)s %(asctime)s] %(message)s')

    logging.info("Starting run.")

    password = _password(args.passfile)
    gh = Github(args.username, password)

    for (user_or_org,
         repo,
         pull_request) in _pull_requests_from_specs(gh,
                                                    [args.pull_request_spec]):
        logging.info("Importing %s/%s:%s",
                     user_or_org.login,
                     repo.name,
                     pull_request.number)
        _import_to_diffscuss(gh, args.username, password,
                             user_or_org, repo, pull_request,
                             args.output_dir)
    sys.exit(0)


