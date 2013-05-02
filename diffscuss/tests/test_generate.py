import os
import subprocess
import tempfile

from nose.tools import ok_, eq_
from impermagit import fleeting_repo

from diffscuss import generate


def test_gen_diffscuss_basics():
    testy_mc = "Testy McTesterson <testy@example.com>"
    with fleeting_repo() as repo:
        repo.do_git(["config", "user.name", "Testy McTesterson"])
        repo.do_git(["config", "user.email", "testy@example.com"])
        repo.commit([('README.txt', 'dweezel')],
                    commit_msg="Initial commit")
        repo.commit([('test.txt', '\n'.join(['this is the first line',
                                             'this is the second line',
                                             '']))],
                    author=testy_mc,
                    commit_msg="First commit message.")
        repo.commit([('test.txt', '\n'.join(['this is the changed first line',
                                             'this is the second line',
                                             'this is the new third line',
                                             '']))],
                    author=testy_mc,
                    commit_msg="Second commit message.")

        diffscussion = _run_gen_diffscuss(cwd=repo.repo_root,
                                          revs="HEAD~2..HEAD")

        # do some cheesy tests to make sure strings we expect have /
        # haven't have made it into the diffscussion, before we do
        # line by line comparison, to make it easier to see what's
        # actually going wrong

        # we shouldn't have the initial commit msg
        ok_("Initial commit" not in diffscussion)
        # or the initial commit's diffs
        ok_('README.txt' not in diffscussion)
        ok_('dweezel' not in diffscussion)

        # both included commit logs msgs should be in there
        ok_("First commit message" in diffscussion)
        ok_("Second commit message" in diffscussion)
        # and they should be in chrono order
        ok_(diffscussion.find("First commit message") <
            diffscussion.find("Second commit message"))

        # make sure the diffs came through
        ok_("+this is the changed first line" in diffscussion)
        ok_("+this is the second line" in diffscussion)
        ok_("+this is the new third line" in diffscussion)

        # make sure the author was picked up
        ok_("author: Testy McTesterson" in diffscussion)
        # and the email
        ok_("email: testy@example.com" in diffscussion)

        # and some cheesy line by line structure
        lines = diffscussion.split("\n")

        eq_("%* ", lines[0])
        eq_("%* author: Testy McTesterson", lines[1])
        eq_("%* email: testy@example.com", lines[2])
        ok_(lines[3].startswith("%* date: "))
        eq_("%* ", lines[4])
        eq_("%- First commit message.", lines[5])
        eq_("%- ", lines[6])
        eq_("%- ", lines[7])
        eq_("%- Second commit message.", lines[8])
        eq_("%- ", lines[9])
        eq_("%- ", lines[10])
        ok_(lines[11].startswith("diff --git"))
        ok_(lines[12].startswith("new file mode"))
        ok_(lines[13].startswith("index"))
        ok_(lines[14].startswith("---"))
        ok_(lines[15].startswith("+++"))
        ok_(lines[16].startswith("@@"))
        eq_("+this is the changed first line", lines[17])
        eq_("+this is the second line", lines[18])
        eq_("+this is the new third line", lines[19])


class Args(object):

    def __init__(self, git_revision_range, output_file):
        self.git_revision_range = git_revision_range
        self.output_file = output_file
        self.lines_context = 20
        self.author = None
        self.email = None
        self.git_exe = None


def _run_gen_diffscuss(cwd, revs):
    old_dir = os.getcwd()
    try:
        os.chdir(cwd)
        with tempfile.NamedTemporaryFile() as fil:
            args = Args(revs, fil.name)
            generate._main(args)
            return fil.read()
    finally:
        os.chdir(old_dir)



def _gen_diffscuss_cmd(revs):
    this_dir = os.path.dirname(__file__)
    diffscuss_dir = os.path.join(this_dir, '..')
    abs_diffscuss_dir = os.path.abspath(diffscuss_dir)
    abs_gen_diffscuss = os.path.join(abs_diffscuss_dir,
                                     'gen-diffscuss.py')
    return [abs_gen_diffscuss, revs]
