#!/bin/bash

# Run emacs with nothing else but diffscuss-mode.el loaded (e.g. no
# user or site .emacs) to make it easier to see what's going on.

emacs -q -L . --no-site-file --no-splash --load diffscuss-mode.el "$@"
