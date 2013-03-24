# Diffscuss: Code Reviews.  In Plain Text.

## What is Diffscuss?

* A format for embedding code reviews into unified diff files.

* An Emacs mode for creating / reading / responding to reviews in that
  format, with git integration.

* gen-diffscuss.py, a tool for generating code reviews from git repos.

There are also plans for some command line tools, and tools to allow diffscuss
files to serve as a code-review interchange format.

## Why Use Diffscuss?

If you like:

* Plain text formats that play nicely with the *nix toolchain

* Lightweight, flexible code-review processes

* The idea of being able to work with / keep your reviews close to
  your source, even in the same repository

Then you might want to give Diffscuss a look.

## The Format in a Nutshell

Diffscuss adds a single beginning-of-line character to the unified
diff format: %, which marks the beginning of a comment line.  Lines
beginning with %* are comment headers, and lines beginning with %- are
comment bodies.  The number of * or - characters indicates reply
threading.

Since one example is worth a lot of explaining, here's an example
diffscuss file:

```
%*
%* author: Bob Jones
%* email: bjones@example.com
%* date: 2013-03-15T19:00:21-0400
%*
%- This change set accomplishes two purposes:
%-
%- * Add a version of the world greeting for our Spanish-speaking users.
%-
%- * Indicate that it's only a test.
%-
%**
%** author: Edmund Jorgensen
%** email: edmund@example.com
%** date: 2013-03-16T19:00:45-0400
%**
%-- Looks good, one question about internationalization.
%--
diff --git a/example.txt b/example.txt
index f75ba05..01632d8 100644
--- a/example.txt
+++ b/example.txt
@@ -1 +1,7 @@
 Hello, world.
+
+Hola, mundo.
%*
%* author: Edmund Jorgensen
%* email: edmund@example.com
%* date: 2013-03-16T19:00:37-0400
%*
%- I'm not sure we want to go international just yet.
%-
%**
%** author: Bob Jones
%** email: bjones@example.com
%** date: 2013-03-17T18:12:49-0400
%**
%-- What are your specific concerns?
%--
+
+This is a test.
+
+It is only a test.
```

The comments at the top of the file apply to the entire review in
general.

The other two comments address the ```+Hola, mundo.``` line (that is,
the line directly above the first comment in the thread).

Take a look at the "Format Definition" section for much more detail.

## The Emacs Mode

The Emacs mode colorizes diffscuss files to make for easier reading.
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

### Showing the Source

If you position the cursor on one of the diff lines in a Diffscuss
file, then:

* ```C-c -``` will open up a temporary buffer containing the old
  version of the source (if it's available locally), with the cursor
  positioned on the same line.

* ```C-c +``` will open up a temporary buffer containing the new
  version of the source (if it's available locally), with the cursor
  positioned on the same line.

Both of these require that the Diffscuss file you're visiting is
located inside the git repo directory.

* ```C-c s``` will attempt to find the local source file / line in
  that file that's the best candidate to match up with the diffscuss
  line the cursor is currently on.

This requires that the diffscuss file you are visiting is somewhere
under a git checkout of the repo against which the diffscuss file was
generated, and that you have set the ```diffscuss-dir``` Emacs
variable to root directory of your diffscuss installation (where
find-local-source.py is located).

### Navigation

You can move around the diffscussion quickly using:

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

## What the Future Might Hold

### Providing for the Liberation of Code Reviews (Aspirational)

Right now, if you use Github (for example), you can stop using Github
at any moment without losing any of your code and history--it's all in
your git repo.

Github has done a fantastic job of making sure you can even take your
wiki information with you--that's just another git repo.

But the comments on your pull requests--all that amazing and rich
history of problems being hammered out and nits picked--is harder to
reconstruct.  Github has--in amazing non-lockin-y fashion, and mad
props to them for it--provided an API to get all the information about
your pull requests, but it's non-trivial to reconstruct it all,
requiring a bunch of calls and a bunch of reconstructed json.

Now imagine you could hit the pull request url with a .diffscuss
extension (as you can already do with a .diff extension) and pull in
the diff--**with** the contextualized comments, right inline, for
viewing in your editor or whatever.  That would be pretty cool, no?
Especially if you could then import into another code review system
with relatively little effort.

Or say that, just like you can with your Github wiki, you could clone
a git repo of all your project's pull requests, in a simple textual
format, for offline reading, and potentially--with some push
magic--even offline code reviews.

This is an aspirational in the extreme goal--after all, Github would
have to create the .diffscuss endpoint etc.--but a simple, standard
format helps pave the way.
