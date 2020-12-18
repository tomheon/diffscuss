drop table if exists version;
drop table if exists diff_lines;
drop table if exists threads;
drop table if exists comments;
drop table if exists comment_headers;
drop table if exists comment_lines;

-- this tracks the version of this application schema
pragma user_version = 1;

create table diff_lines (
  -- the 1-indexed line number of this line in the original diff file, meaning
  -- stripped of all diffscuss content
  orig_line_no int primary key,
  -- the contents of that diff line
  contents text not null
);

create table comments (
  comment_id int primary key,
  -- 0 if no parent
  parent_comment_id int not null,
  comment_seq int not null,
  author text,
  made_at text
);

create table comment_headers (
  comment_id int not null references comments(comment_id),
  header text not null,
  contents text not null,
  primary key (comment_id, header)
);

create table comment_lines (
  comment_id int not null references comments(comment_id),
  -- the line number, 1 based, of the contents
  comment_line_no int not null,
  contents text not null
);

create table threads (
  thread_id int primary key,
  -- the thread starts before this line number in the original diff (so if
  -- precedes_line_no is 1, the thread starts at the top of the diff file,
  -- before any diff content
  precedes_line_no int not null references diff_lines(orig_line_no),
  -- tiebreaker for sequencing multiple threads that start before the same
  -- source line
  thread_seq int not null,
  -- the comment that kicks off the thread
  comment_id int not null references comments(comment_id)
);

