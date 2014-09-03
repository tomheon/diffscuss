#!/usr/bin/env bash

# Run all the tests.

# don't set -e so that all test invocations will run.

nosetests

NOSETEST_EXIT=$?

pushd diffscuss-mode/tests > /dev/null
./runtests.sh
EMACS_EXIT=$?
popd > /dev/null

# sum the exit codes of the test suites so all have to return 0
# (assumes that no one returns a negative, I know, but these return 1
# when they fail)

TO_EXIT=$((NOSETEST_EXIT + EMACS_EXIT))

exit $TO_EXIT
