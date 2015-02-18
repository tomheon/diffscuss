"""
Command line interface for use by the top-level 'diffscuss'
executable.
"""

import argparse
import sys
from textwrap import dedent

from diffscuss import find_local, generate, github_import, mailbox


def print_usage():
    usage = dedent("""\
                   This is a program.

                   Woot.
                   """)
    print >> sys.stderr, usage


def _add_gen_subparser(subparsers):
    epilog = dedent("""\
                    The git_revision_range will be passed as-is to
                    git diff and git log.  For example, you could use
                    HEAD~3..HEAD or some_branch..master.  Note that you
                    probably don't want just a branch name, as git log
                    will start the log from that point and run
                    backwards to the initial commit in the repo.  Use
                    something like master..branchname instead.
                    """)
    gen_subparser = subparsers.add_parser('generate',
                                          epilog=epilog)
    gen_subparser.add_argument("-a", "--author",
                               help="The author for the diffscuss review "
                               "(defaults to the result of 'git config "
                               "--get user.name')")
    gen_subparser.add_argument("-e", "--email",
                               help="The email address for the diffscuss "
                               "review (defaults to the result of 'git "
                               "config --get user.email')")
    gen_subparser.add_argument("-g", "--git-exe",
                               help="The path to the git executable"
                               " (defaults to '/usr/bin/env git')")
    gen_subparser.add_argument("-o", "--output-file",
                               help="File to write the output to"
                               " (defaults to stdout if not supplied or -)")
    gen_subparser.add_argument("-c", "--lines-context",
                               type=int,
                               help="Number of lines of context "
                               "to show in the diff (defaults to 20)",
                               default=20)
    gen_subparser.add_argument("git_revision_range",
                               help="The revisions to include in the "
                               "review, in a form recognizable to git log "
                               "or diff (e.g. HEAD~3..HEAD)")
    gen_subparser.add_argument("path", default=[],
                               help="Paths to include in review. Defaults to "
                               "every file changed in the revision. Works "
                               "exactly as 'git log <revision> -- <path>'",
                               nargs='*')


def _add_find_local_subparser(subparsers):
    epilog = dedent("""\
                    Accepts a diffscuss file (either through the
                    -i argument or stdin) and a line number within
                    that file, and outputs, on either the file
                    specified by -o or stdout, the path and line
                    number of the best-guess local source file.

                    The output format is "file_path line_number".

                    The line number is 1 based.

                    Exits with a non-0 return code if local source
                    cannot be found.
                    """)

    find_local_subparser = subparsers.add_parser('find-local',
                                                 epilog=epilog)
    find_local_subparser.add_argument('-i', '--input-file',
                                      default='-',
                                      help="Diffscuss file to read, "
                                      "stdin if not provided or -")
    find_local_subparser.add_argument('-o', '--output-file',
                                      default='-',
                                      help="File to write results to,"
                                      " stdout if not provided or -")
    find_local_subparser.add_argument('-d', '--directory',
                                      default='-',
                                      help="Directory in which to start"
                                      " searching for the local source, "
                                      "current dir if not provided")
    find_local_subparser.add_argument("line_number",
                                      type=int,
                                      help="The line number in the"
                                      " supplied diffscuss file for which"
                                      " to find the corresponding local"
                                      " source")


def _add_github_import_subparser(subparsers):
    gh_subparser = subparsers.add_parser('github-import')
    gh_subparser.add_argument("-p", "--passfile",
                              help="File containg github password "
                              "to use for api (required)",
                              required=True)
    gh_subparser.add_argument("-u", "--username",
                              help="Github user name to use for api (required)",
                              required=True)
    gh_subparser.add_argument("-o", "--output-dir",
                              help="Directory in which to put output"
                              " (defaults to 'gh-import')",
                              default="gh-import")
    gh_subparser.add_argument("-l", "--logfile",
                              help="File to use for logging"
                              " (defaults to gh-import.log)",
                              default="gh-import.log")
    gh_subparser.add_argument("-d", "--debug",
                              help="Turn on debug level logging.",
                              action="store_true", default=False)
    gh_subparser.add_argument("pull_request_spec",
                              help="""
                                   <user_or_org>[/repo_name[:pull_request_id]]
                                   specifies a single pull request
                                   (e.g. 'hut8labs/diffscuss:15'), all
                                   pull requests for a repo
                                   (e.g. 'tomheon/git_by_a_bus') or
                                   all pull requests for a user or
                                   organization (e.g. 'hut8labs')
                                   """)


def _add_check_mb_subparser(mb_subparser):
    check_subparser = mb_subparser.add_parser('check')
    check_subparser.add_argument("-g", "--git-exe",
                               help="The path to the git executable"
                               " (defaults to '/usr/bin/env git')")
    check_subparser.add_argument("-i", "--inbox",
                                 help="""Inbox name (if not supplied,
                                         will use the return of 'git config
                                         --get diffscuss-mb.inbox'""")
    check_subparser.add_argument("-e", "--emacs",
                                 action="store_true",
                                 default=False,
                                 help="Format for emacs compilation mode")
    check_subparser.add_argument("-s", "--short",
                                 action="store_true", default=False,
                                 help="List only reviews, no info about them")


def _add_set_inbox_subparser(mb_subparser):
    epilog = "Set default inbox name"
    inbox_subparser = mb_subparser.add_parser('set-default-inbox',
                                              epilog=epilog)
    inbox_subparser.add_argument("-g", "--git-exe",
                                 help="The path to the git executable"
                                 " (defaults to '/usr/bin/env git')")
    inbox_subparser.add_argument("inbox",
                                 help="The inbox to make the default")


def _add_make_inbox_subparser(mb_subparser):
    epilog = "Create an inbox"
    inbox_subparser = mb_subparser.add_parser('make-inbox',
                                               epilog=epilog)
    inbox_subparser.add_argument("-g", "--git-exe",
                                 help="The path to the git executable"
                                 " (defaults to '/usr/bin/env git')")
    inbox_subparser.add_argument("inbox",
                                 help="The name of the inbox to create")


def _add_init_subparser(mb_subparser):
    epilog = """Initialize a diffscuss mailbox directory (must
                be run within a git checkout)"""
    init_subparser = mb_subparser.add_parser('init',
                                             epilog=epilog)
    init_subparser.add_argument("-g", "--git-exe",
                                help="The path to the git executable"
                                " (defaults to '/usr/bin/env git')")
    init_subparser.add_argument("-d", "--directory",
                                default="diffscussions",
                                help="""The mailbox directory to create
                                        (defaults to 'diffscussions')""")


def _add_post_subparser(mb_subparser):
    epilog = """Post a review to one or more inboxes"""
    post_subparser = mb_subparser.add_parser('post',
                                             epilog=epilog)
    post_subparser.add_argument("-g", "--git-exe",
                                help="The path to the git executable"
                                " (defaults to '/usr/bin/env git')")
    post_subparser.add_argument("file",
                                help="Diffscuss file to post for review")
    post_subparser.add_argument("-p", "--print-review-path",
                                action="store_true",
                                default=False,
                                help="""Print the path of the posted review
                                        before exiting""")
    post_subparser.add_argument("inbox",
                                help="Inbox to post to")
    post_subparser.add_argument("inboxes",
                                nargs=argparse.REMAINDER,
                                help="Other inboxes to post to")


def _add_bounce_subparser(mb_subparser):
    epilog = """Bounce a review to one or more inboxes"""
    bounce_subparser = mb_subparser.add_parser('bounce',
                                               epilog=epilog)
    bounce_subparser.add_argument("-g", "--git-exe",
                                  help="The path to the git executable"
                                  " (defaults to '/usr/bin/env git')")
    bounce_subparser.add_argument("file",
                                  help="Diffscuss file to bounce for review")
    bounce_subparser.add_argument("-p", "--print-review-path",
                                  action="store_true",
                                  default=False,
                                  help="""Print the path of the bounced review
                                        before exiting""")
    bounce_subparser.add_argument("-f", "--from-inbox",
                                  help="""Inbox to remove file from
                                          (if not supplied, will use the return
                                          of 'git config --get
                                          diffscuss-mb.inbox'""")
    bounce_subparser.add_argument("inbox",
                                  help="Inbox to bounce to")
    bounce_subparser.add_argument("inboxes",
                                  nargs=argparse.REMAINDER,
                                  help="Other inboxes to bounce to")


def _add_done_subparser(mb_subparser):
    epilog = """Remove a review from an inbox"""
    done_subparser = mb_subparser.add_parser('done',
                                             epilog=epilog)
    done_subparser.add_argument("-g", "--git-exe",
                                help="The path to the git executable"
                                " (defaults to '/usr/bin/env git')")
    done_subparser.add_argument("-p", "--print-review-path",
                                action="store_true",
                                default=False,
                                help="""Print the path of the review
                                        before exiting""")
    done_subparser.add_argument("-f", "--from-inbox",
                                help="""Inbox to remove file from
                                          (if not supplied, will use the return
                                          of 'git config --get
                                          diffscuss-mb.inbox'""")
    done_subparser.add_argument("file",
                                help="Diffscuss file to done for review")



def _add_mailbox_subparser(subparsers):
    mb_parser = subparsers.add_parser('mailbox')
    mb_subparser = mb_parser.add_subparsers(dest="mailbox_subcommand_name")
    _add_check_mb_subparser(mb_subparser)
    _add_set_inbox_subparser(mb_subparser)
    _add_make_inbox_subparser(mb_subparser)
    _add_init_subparser(mb_subparser)
    _add_post_subparser(mb_subparser)
    _add_bounce_subparser(mb_subparser)
    _add_done_subparser(mb_subparser)


mod_map = {'find-local': find_local,
           'generate': generate,
           'github-import': github_import,
           'mailbox': mailbox}


if __name__ == '__main__':
    parser = argparse.ArgumentParser(prog='diffscuss')
    subparsers = parser.add_subparsers(dest="subcommand_name")

    _add_gen_subparser(subparsers)
    _add_find_local_subparser(subparsers)
    _add_github_import_subparser(subparsers)
    _add_mailbox_subparser(subparsers)

    args = parser.parse_args()
    mod = mod_map.get(args.subcommand_name)
    mod.main(args)
