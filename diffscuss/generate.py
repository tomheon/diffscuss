import subprocess
import sys
import time
from textwrap import dedent


def _check_output(*popenargs, **kwargs):
    """
    Run command with arguments and return its output as a byte string.

    Backported from Python 2.7 as it's implemented as pure python on
    stdlib.

    >>> check_output(['/usr/bin/python', '--version'])
    Python 2.6.2

    Copied from https://gist.github.com/edufelipe/1027906.
    """
    process = subprocess.Popen(stdout=subprocess.PIPE,
                               stderr=subprocess.PIPE,
                               *popenargs, **kwargs)
    output, err = process.communicate()
    retcode = process.poll()
    if retcode:
        cmd = kwargs.get("args")
        if cmd is None:
            cmd = popenargs[0]
        error = subprocess.CalledProcessError(retcode, cmd)
        error.output = '\n'.join([output, err])
        raise error
    return output


if 'check_output' not in dir(subprocess):
    check_output = _check_output
else:
    check_output = subprocess.check_output


def _git_config(config_name, git_exe):
    return check_output(_git_cmd(git_exe,
                                 ["config",
                                  "--get",
                                  config_name])).strip()


def _git_cmd(git_exe, cmd):
    if git_exe:
        git_cmd = [git_exe.split()]
    else:
        git_cmd = ["/usr/bin/env", "git"]
    return git_cmd + cmd


def _git_user_name(git_exe):
    return _git_config("user.name", git_exe)


def _git_user_email(git_exe):
    return _git_config("user.email", git_exe)


def _iso_time():
    return time.strftime("%Y-%m-%dT%H:%M:%S%z")


def _git_log(revision, git_exe):
    """
    Return a list of log lines.
    """
    return check_output(_git_cmd(git_exe,
                                 ["log", "--pretty=format:%B%n",
                                  "--reverse", revision])).split('\n')


def _write_diff(revision, lines_context, output_f, git_exe):
    output_f.write(check_output(_git_cmd(git_exe,
                                         ["diff",
                                          "--unified=%d" % lines_context,
                                          revision])))


def _write_diffscuss_header(output_f, author, email, git_exe):
    if author is None:
        author = _git_user_name(git_exe)
    if email is None:
        email = _git_user_email(git_exe)

    iso_time = _iso_time()

    header_lines = ['',
                    "author: %s" % author,
                    "email: %s" % email,
                    "date: %s" % iso_time,
                    '']
    header_lines = ['%%* %s' % s for s in header_lines]

    output_f.write('\n'.join(header_lines))
    output_f.write('\n')


def _write_diffscuss_body(output_f, revision, git_exe):
    log_lines = ['%%- %s' % s for s in _git_log(revision, git_exe)]
    output_f.write('\n'.join(log_lines))
    output_f.write('\n')


def _main(args):
    revision = args.git_revision_range
    lines_context = args.lines_context
    output_fname = args.output_file
    author = args.author
    email = args.email
    git_exe = args.git_exe

    if output_fname is None or output_fname == '-':
        output_f = sys.stdout
    else:
        output_f = open(output_fname, 'wb')

    _write_diffscuss_header(output_f, author, email, git_exe)
    _write_diffscuss_body(output_f, revision, git_exe)
    _write_diff(revision, lines_context, output_f, git_exe)

    if output_fname is not None and output_fname != '-':
        output_f.close()


def main(args):
    try:
        _main(args)
    except subprocess.CalledProcessError, e:
        print >> sys.stderr, e
        print >> sys.stderr, e.output
        sys.exit(e.retcode)
