# Diffscuss: Code Reviews.  In Plain Text.

## Why Should You Use Diffscuss?

* Create and read reviews in your editor.  Jump straight to the
  associated source to apply fixes etc.

* Keep your reviews in your repo, right next to your code, or email
  them back and forth if you're a small team and that's easier.

* Use grep and all the rest of that unix-y toolchain goodness on your
  reviews.

## Why Shouldn't You Use Diffscuss?

* You're a big team with a lot of process around code reviews
  (e.g. you have automated restrictions for review-then-commit
  workflows).

* You're not using git (for the moment, Diffscuss is tightly
  integrated with git).

* You're not using Emacs or vim (for the moment, those are the two
  editors with Diffscuss tooling support).

## What Does a Diffscuss Review Look Like?

<div style="font-family: Courier New; border: 2px solid; padding: 5px;">
<span style="color: blue;">%*</span><br />
<span style="color: blue;">%* author: Edmund Jorgensen</span><br />
<span style="color: blue;">%* email: edmund@example.com</span><br />
<span style="color: blue;">%* date: 2013-04-09T20:27:04-0400</span><br />
<span style="color: blue;">%*</span><br />
<span style="color: blue;">%-</span> Explode Comment in walker and just read line by line.<br />
<span style="color: blue;">%-</span><br />
<span style="color: blue;">%-</span> Later I'll add a record reader a la Matt Papi.<br />
<span style="color: blue;">%-</span><br />
<span style="color: blue;">%-</span><br />
<div style="color: gray;">diff --git a/diffscuss/tests/test_walker.py b/diffscuss/tests/test_walker.py<br />
index c3a04dd..3042bf8 100644<br />
--- a/diffscuss/tests/test_walker.py<br />
+++ b/diffscuss/tests/test_walker.py<br />
@@ -1,178 +1,179 @@
</div>
<div> # -*- coding: utf-8 -*-<br />
<br />
&nbsp;import itertools<br />
&nbsp;import os<br />
&nbsp;from StringIO import StringIO<br />
&nbsp;from textwrap import dedent<br />
<br />
&nbsp;from nose.tools import eq_, ok_<br />
<br />
&nbsp;from diffscuss.walker import walk, MissingAuthorException, \<br />
</div>
<div style="color: red;">-&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;EmptyCommentException, BadNestingException, Comment, \<br />
-&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;CommentInHeaderException
</div>
<div style="color: green;">+&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;EmptyCommentException, BadNestingException, \<br />
+&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;CommentInHeaderException, DIFF_HEADER, DIFF, \<br />
+&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;COMMENT_HEADER, COMMENT_BODY
</div>
<span style="color: blue;">%*</span><br />
<span style="color: blue;">%* author: Testy McTesterson</span><br />
<span style="color: blue;">%* email: testy@example.com</span><br />
<span style="color: blue;">%* date: 2013-04-09T20:47:19-0400</span><br />
<span style="color: blue;">%*</span><br />
<span style="color: blue;">%-</span> I don't think you ever use COMMENT_BODY, remove?<br />
<span style="color: blue;">%-</span><br />
<span style="color: purple;">%**</span><br />
<span style="color: purple;">%** author: Edmund Jorgensen</span><br />
<span style="color: purple;">%** email: edmund@example.com</span><br />
<span style="color: purple;">%** date: 2013-04-09T21:12:54-0400</span><br />
<span style="color: purple;">%**</span><br />
<span style="color: purple;">%--</span> Good call, fixed.<br />
<span style="color: purple;">%--</span><br />
<br />
<br />
&nbsp;def _test_fname(fname):<br />
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;return os.path.join(os.path.dirname(__file__),<br />
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;'testfiles',<br />
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;fname)<br />
<span>...</span>
</div>

## The Format in a Nutshell

Diffscuss adds a single beginning-of-line character to the unified
diff format: %, which marks the beginning of a comment line.  Lines
beginning with %* are comment headers, and lines beginning with %- are
comment bodies.  The number of * or - characters indicates reply
threading.

Comments apply to the line directly above them.  Comments at the top
of the file apply to the entire review in general.

Take a look at the "Format Definition" section for (much) more detail.

## The Emacs Mode

The Emacs mode is implemented in a single .el file, diffscuss-mode.el.
To install, either move the diffscuss-mode.el file to a directory in
your load path or else add the Diffscuss mode directory to your load
path in your .emacs file like so:

```
(setq load-path
      (append (list nil "/path/to/diffscuss/diffscuss-mode")
               load-path))
```

Once the file is in your load path, require the mode with:

```
(require 'diffscuss-mode)
```

To use the jump to local source feature, you also need to add the
following to your .emacs:

```
(setq diffscuss-dir "/path/to/diffscuss")
```

Where /path/to/diffscuss is the path to the top level of your
Diffscuss checkout (that is, where find-local-source.py is located).

The mode colorizes Diffscuss files to make for easier reading.
In addition it helps with:

### Inserting Comments

The main command you need to know is ```C-c C-c```, which generally
"does the right thing" based on the position of the cursor.  To wit:

* If the cursor is at the very top of the buffer, it will insert a new
  review-level comment (this function is available anywhere in the
  buffer with ```C-c C-f```).

* If the cursor is inside another comment, it will create a reply to
  that comment (this can also be invoked with ```C-c C-r```).

* If the cursor is inside the diff index information for a file /
  hunk, it will insert a comment after the "range line" (the line
  beginning with @@).

* If the cursor is on a diff line, it will create a comment directly
  below that line (this is also available with ```C-c C-i```).

### Jumping to Source

All these require that the Diffscuss file you are visiting is
somewhere under a git checkout of the repo against which the Diffscuss
file was generated, and that you have set the ```diffscuss-dir```
Emacs variable to root directory of your Diffscuss installation (where
find-local-source.py is located).

#### Local Source

If you position the cursor on one of the diff lines in a Diffscuss
file, then:

* ```C-c s``` will attempt to find the local source file / line in
  that file that's the best candidate to match up with the Diffscuss
  line the cursor is currently on.

#### Repository Versions of the Source

* ```C-c -``` will open up a temporary buffer containing the old
  version of the source (if it's available locally), with the cursor
  positioned on the same line.

* ```C-c +``` will open up a temporary buffer containing the new
  version of the source (if it's available locally), with the cursor
  positioned on the same line.

### Navigation

You can move around the Diffscussion quickly using:

* ```C-c f``` to move forward one comment.

* ```C-c b``` to move back one comment.

* ```C-c n``` to move to the next thread.

* ```C-c p``` to move to the previous thread.

* ```C-c a``` to move to the beginning of the current thread.

* ```C-c e``` to move to the end of the current thread.

## The Vim Plugin

Like the Emacs mode, the Vim plugin offers syntax highlight, comment insertion,
commands for jumping to source files, and motions for comments and threads.
The Vim plugin is implemented primarily in Python, so you'll need a version of
Vim compiled with Python support in order to use it.

### Configuration

The Vim plugin requires you to set `g:diffscuss_config` in your `.vimrc`,
which it uses to pre-fill comment headers and locate scripts.

```vim
let g:diffscuss_config = {
    \'author': 'Your Name',
    \'email': 'your.email@example.com',
    \'diffscuss_dir': '/path/to/diffscuss'
    \}
```

### Inserting Comments

* `<leader>dd`: insert a new comment contextually (Emacs `C-c C-c`)
* `<leader>df`: insert a new review-level comment (Emacs `C-c C-f`)
* `<leader>dr`: insert a new reply (Emacs `C-c C-r`)
* `<leader>di`: insert a new comment (Emacs `C-c C-i`)

### Showing the Source

* `<leader>do`: show the old source version (Emacs `C-c -`)
* `<leader>dn`: show the new source version (Emacs `C-c +`)
* `<leader>ds`: show the local source version (Emacs `C-c s`)

### Navigation

* `]d`: jump to the start of the next comment
* `[d`: jump to the start of the previous comment
* `]D`: jump to the end of the next comment
* `[D`: jump to the end of the previous comment
* `]t`: jump to the start of the next thread
* `[t`: jump to the start of the previous thread
* `]T`: jump to the end of the next thread
* `[T`: jump to the end of the previous thread

## gen-diffscuss.py

gen-diffscuss.py is a helper script for creating a new Diffscuss file
with diffs and log messages from a set of changes in git.

For example,

```gen-diffscuss.py HEAD~3..HEAD > some_code_review.diffscuss```

Makes a Diffscuss file containing the diffs from the last three
commits in a local git repo, introduced with a comment at the top
containing all the log messages for those three commits.

## Simple Mailbox Support

Diffscuss ships with diffscuss-mb, which provides simple mailbox
support for reviews.  In a nutshell, diffscuss-mb manages a single
directory (let's call it "diffscussions" in this example), with two
subdirs:

* reviews - where your .diffscuss files live

* users - which has one subdir per user in your system

When you post a diffscuss file for review, it's move into the
"reviews" directory, and for each user you request review from, a
symlink is created in their user directory.

There is support for posting reviews, marking them done (which just
translates to removing symlinks in user directories), and "bouncing"
reviews, which means marking your review done and asking someone else
to take a look (e.g., because you made comments that you want the
original poster to implement / respond to).

The Emacs and Vim modes have support for checking your mailbox,
posting, bouncing, and marking reviews done.

In Emacs:

* ```C-c m p``` prompts for recipients and posts the current Diffscuss
  file for their review

* ```C-c m b``` prompts for recipients and bounces the review to them,
  removing the review from your inbox.

* ```C-c m d``` marks the review as done, removing it from your inbox.

* ```M-x diffscuss-mb-check``` checks your inbox and lists all
  incoming reviews.  You may wish to bind it globally to ```C-c m c```
  with:

```
(global-set-key "\C-cmc"  'diffscuss-mb-check)
```

## What the Future Might Hold

* Side-by-side diff viewing in the modes

* Svn support

* Support for other Sublime

* Support for Eclipse

* Support for other editors

* Export from ReviewBoard / other code review systems

* Imports into various code review systems (Diffscuss as code review interchange)

If any of these appeal to you / scratch a personal itch, please let us
know--or even better, contribute!

## Format Definition

### Beginning of Line Marker

All Diffscuss lines begin with one '%'.

There's no limit to the length of a Diffscuss line, but keeping them
80 chars or less when possible is probably good citizenship, since
they're meant to be read and edited inside editors.

### Diffscuss Comment Format

A Diffscuss comment begins with an author line, and possibly other
header lines, followed by at least one body line.

### Header Lines

A header line one '%' followed by at least one '*', followed by a
space, followed by a header of the format 'field-name: value'.

Header lines may also be empty, containing no 'field-name: value', in
which case the space after the * is also optional.

A header line must always begin a comment or follow another header
line.

For example, all three of these are valid header lines:

```
%* author: Bob Jones
```

```
%**
```

```
%*** x-github-version: 1
```

The field name cannot contain whitespace.  The value cannot contain a
newline.

The current Diffscuss headers are:

* author

* email

* date

Non-standard headers should begin with an 'x-', to keep compatible
with future additions to the Diffscuss format.

#### The author Header

An author line is a standard header line with a field of 'author' and
a value indicating who authored the comment.  For example:

```
%* author: ejorgensen
```

Or

```
%** author: bsmith
```

Every comment must begin with an author line.  All other headers are
optional.

#### The date Header

Dates should be specified in the full ISO 8601 standard, including
time zone offset, e.g. 2013-11-22T23:11:21-0400.

### Body Lines

A body line is one '%' character followed by at least one -, followed
by a space, followed by arbitrary text.

Exception: for an empty body line, the space is optional.

For example:

```
%- This is a body line.
```

```
%-- And so is *this*.
```

```
%- and this next body line
%-
%- is empty, so doesn't need a space, but could have one.
```

A body line must always follow a header line or another body line.

### Threads

A thread is one or more adjacent comments, properly threaded.

For example, this is a thread:

```
%* author: ejorgensen
%- I'm a one comment thread.
```

And so is this:

```
%* author: ejorgensen
%- I'm a two comment thread.
%* author: bsmith
%- With no replies, just two top level comments.
```

And so is this:

```
%* author: ejorgensen
%- I'm a two comment thread.
%** author: bsmith
%-- With a reply.
```

To be explicit: the nesting / reply level of a thread is determined by
the number of '*' characters in each header line / '-' characters in
each body line, which should remain constant for a given comment.

A comment that is a reply to a previous comment should have one more
'*' at the beginning of each header line and one more '-' at the
beginning of each body line than its parent comment.

The parent comment for any reply can always be determined by finding
the closest previous comment with one less level of nesting.  For
example:

```
%* author: ejorgensen
%- I'm a top-level comment.
%** author: bsmith
%-- And I'm a reply.
%*** author: sjones
%--- And I'm a reply to the reply.
%** author: jkidd
%-- And I'm a second reply to the original top-level comment.
%* author: mdarks
%- And I'm another top-level comment.
%** author: lbutterman
%-- And I'm a reply to the second top-level comment.
```
#### Position / Target of Threads

Diffscuss threads are generally taken to apply to the line immediately
above them, so for example in this snippet:

```
+It's only just a test
%* author: ejorgensen
%- I have grave doubts about this change.  To me it appears foolhardy
%- and dangerous.
```

The comment applies to the line

```
+It's only just a test
```

A Diffscuss thread can also appear directly after the range
information in a hunk, in which case the target of the comment is
assumed to be the entire hunk, for example:

```
--- 1.txt	2013-03-07 20:18:10.000000000 -0500
+++ 2.txt	2013-03-07 20:18:35.000000000 -0500
@@ -1,5 +1,7 @@
%* author: ejorgensen
%- I love this hunk.
 This is a test.

-It's just a test
+It's only just a test
```

Finally, if a thread appears at the very top of the Diffscuss file,
the thread applies to the whole changeset (where it should generally
act as a thread discussing the review as a whole--for example,
introductory remarks about what the changes are attempting to achieve,
"ship it" remarks, etc.).

Every Diffscuss file must begin with such a changeset level thread
(optionally preceded by any number of "magic" comment lines, e.g. "#
-*- coding: utf-8 -*-").
