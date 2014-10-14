        eq_("#* ", lines[0])
        eq_("#* author: Testy McTesterson", lines[1])
        eq_("#* email: testy@example.com", lines[2])
        ok_(lines[3].startswith("#* date: "))
        eq_("#* ", lines[4])
        eq_("#- First commit message.", lines[5])
        eq_("#- ", lines[6])
        eq_("#- ", lines[7])
        eq_("#- Second commit message.", lines[8])
        eq_("#- ", lines[9])
        eq_("#- ", lines[10])
def test_gen_diffscuss_with_path():
    testy_mc = "Testy McTesterson <testy@example.com>"
    with fleeting_repo() as repo:
        repo.do_git(["config", "user.name", "Testy McTesterson"])
        repo.do_git(["config", "user.email", "testy@example.com"])
        repo.commit([('README.txt', 'dweezel')],
                    commit_msg="Initial commit")
        repo.commit([('test.txt', 'test!'),
                     ('subdir/foo.txt', 'foo file')],
                    author=testy_mc,
                    commit_msg="First commit message.")
        repo.commit([('test.txt', 'teeest!!'),
                     ('subdir/bar.txt', 'bar file')],
                    author=testy_mc,
                    commit_msg="Second commit message.")

        diffscussion = _run_gen_diffscuss(cwd=repo.repo_root,
                                          revs="HEAD~2..HEAD",
                                          path=['subdir'])

        # Again, not the first commit
        ok_("Initial commit" not in diffscussion)

        # But the yes to the other two...
        ok_("First commit message" in diffscussion)
        ok_("Second commit message" in diffscussion)

        # Nothing about test.txt
        ok_("test.txt" not in diffscussion)
        ok_("+this is the changed first line" not in diffscussion)
        ok_("+this is the second line" not in diffscussion)
        ok_("+this is the new third line" not in diffscussion)

        # But everything else on 'subdir'
        ok_("subdir/foo.txt" in diffscussion)
        ok_("subdir/bar.txt" in diffscussion)
        ok_("+foo file" in diffscussion)
        ok_("+bar file" in diffscussion)

        # Everything else should be already proven in
        # `test_gen_diffscuss_basics`

    def __init__(self, git_revision_range, output_file, path=None):
        self.path = path or []
def _run_gen_diffscuss(cwd, revs, path=None):
            args = Args(revs, fil.name, path)