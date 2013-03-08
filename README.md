# Diffscuss: An Inter-Diff Code Review Format

Diffscuss is a simple textual standard for embedding code review
information--threads and all--inside unified diffs.

It is still very much in the early stages, and participation in both
defining the format and building tools around it are very welcome.

Diffscuss adds a single beginning-of-line character to the unified
diff format: %.  Since one example is worth a lot of explaining, take
/example.diffscuss/:

```
--- 1.txt	2013-03-07 20:18:10.000000000 -0500
+++ 2.txt	2013-03-07 20:18:35.000000000 -0500
@@ -1,5 +1,7 @@
 This is a test.

-It's just a test
+It's only just a test
%* author: ejorgensen
%- I have grave doubts about this change.  To me it appears foolhardy
%- and dangerous.
%** author: bjones
%-- Well I have grave doubts about your judgement.  So there.
%** author: ssmith
%-- Why do you say "foolhardy"?

 Only a test.
+
+Yup.
%* author: ejorgensen
%- On the other hand, this might be fine, but is worth discussion.
%** author: bjones
%-- Thanks...I guess.
%* author: ssmith
%- Should we maybe make this "yes"?  I'm on the fence.
%** author: ejorgensen
%-- That's a +1 from me.
%*** author: bjones
%--- Yeah I'm on board, done.
```

Take a look at the "Format Definition" section for more detail.

## Project Goals

Diffscuss has two main goals.

### Provide a Format and Tools for Offline, Versionable Code Reviews

Sometimes you just want to send a diff over email and mark it
up--maybe your company hasn't seen the light on code reviews yet, or
your review system is down, or only accessible from within the office.
It should still be easy to do a code review, even from the comfort of
your text editor.

And there's no reason that code reviews shouldn't be versionable
themselves, so they can go right into a repo too.

(Or maybe you're just a textual format zealot, like myself and some of
my friends, and you like to be able to use the standard unix text
processing tools on everything.)

### Provide for the Liberation of Code Reviews (Aspirational)

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

This is an aspirational goal--after all, Github would have to create
the .diffscuss endpoint etc.--but a simple, standard format helps
pave the way.

## Implementation Goals

### Work with Established Formats

Unified diffs are already used for informal email code reviews all the
time.  Diffscuss starts from there, and provides a format that can
always degrade back to a simple diff.

### Remain Sensibly Loose and Extensible

Code reviews already contain subtly different kinds of information in
each system.  If Diffscuss is ever going to serve as a kind of
interchange format for code reviews, it needs to leave room for extra
information in a sensible, non-ambiguous fashion.

Diffscuss attempts to do this through a simple, extensible header
format.

## Tools

### Diffscuss to Diff

diffscuss2diff.py strips out Diffscuss information, leaving a normal
diff file.

### Library (In Progress)

A reference Python library for parsing Diffscuss files.

### Emacs Mode (In Progress)

Navigate and create Diffscuss code reviews in Emacs.

### Vim Integration (Future)

Navigate and create Diffscuss code reviews in Vim.

### Github Pull Request Import / Export (Future)

Convert a pull request to a Diffscuss file, and vice-versa.

### Other Code Review Systems Export / Import (Future)

Import and export Diffscuss files to / from Reviewboard and other
open code review systems.

## Format Definition

### Beginning of Line Marker

All Diffscuss lines begin with one '%'.

There's no limit to the length of a Diffscuss line, but keeping them
80 chars or less when possible is probably good citizenship.

### Diffscuss Comment Format

A Diffscuss comment begins with an author line, and possibly other
header lines, followed by at least one body line.

### Header Lines

A header line one '%' followed by at least one '*', followed by a
space, followed by a header of the format 'field-name: value'.

A header line must always begin a comment or follow another header
line.

For example, both of these are valid header lines:

```
%* commit: 334
```

and

```
%*** github-version: 1
```

The field name cannot contain whitespace.  The value cannot contain a
newline.

#### Author Lines

An author line is a standard header line with a field of 'author' and
a value indicating who authored the comment.  For example:

```
%* author: ejorgensen
```

Or

```
%** author: bsmith
```

Every comment must begin with an author line.

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
%- is empty, so doesn't need a space.
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
%- With no replies.
```

And so is this:

```
%* author: ejorgensen
%- I'm a two comment thread.
%** author: bsmith
%-- With a reply.
```

#### Threading of Comments / Replies

The nesting / reply level of a thread is determined by the number of
'*' characters in each header line / '-' characters in each body line,
which should remain constant for a given comment.

A comment that is a reply to a previous comment should have one more
'*' at the beginning of each header line and one more '-' at the
beginning of each body line than its parent comment.  The parent
comment is always the first previous comment with one less level of
nesting.  For example:

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

#### Position / Target of Discourse Threads

Diffscuss threads are taken to apply to the line immediately above
them, so for example in this snippet:

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

A Diffscuss line should always appears after the range information of
a hunk in a unified diff (that is, after the @@ line).

So this is legal:

```
--- 1.txt	2013-03-07 20:18:10.000000000 -0500
+++ 2.txt	2013-03-07 20:18:35.000000000 -0500
@@ -1,5 +1,7 @@
 This is a test.

-It's just a test
+It's only just a test
%* author: ejorgensen
%- Hi there.
```

And this is not:

```
--- 1.txt	2013-03-07 20:18:10.000000000 -0500
+++ 2.txt	2013-03-07 20:18:35.000000000 -0500
%* author: ejorgensen
%- Hi there.
@@ -1,5 +1,7 @@
 This is a test.

-It's just a test
+It's only just a test
```

It is legal, however, to position a Diffscuss comment directly after
the range information in a hunk, in which case the target of the
comment is assumed to be the entire hunk, for example:

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
