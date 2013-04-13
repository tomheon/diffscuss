"""
Test cases for the editor module.
"""
import editor

from nose.tools import eq_


TEST_LINE_PROPERTIES = [
    ('@@ -0,0 +1,2 @@',
     dict(is_diff_meta=True, is_diff_range=True,
          is_header=False, is_body=False, is_diffscuss=False, depth=0)),
    ('diff --git a/some/file',
     dict(is_diff_meta=True, is_diff_range=False,
          is_header=False, is_body=False, is_diffscuss=False, depth=0)),
    ('--- a/some/file',
     dict(is_diff_meta=True, is_diff_range=False,
          is_header=False, is_body=False, is_diffscuss=False, depth=0)),
    ('+++ a/some/file',
     dict(is_diff_meta=True, is_diff_range=False,
          is_header=False, is_body=False, is_diffscuss=False, depth=0)),
    ('index rev1...rev2 100644',
     dict(is_diff_meta=True, is_diff_range=False,
          is_header=False, is_body=False, is_diffscuss=False, depth=0)),
    ('-diff line',
     dict(is_diff_meta=False, is_diff_range=False,
          is_header=False, is_body=False, is_diffscuss=False, depth=0)),
    ('+diff line',
     dict(is_diff_meta=False, is_diff_range=False,
          is_header=False, is_body=False, is_diffscuss=False, depth=0)),
    (' diff line',
     dict(is_diff_meta=False, is_diff_range=False,
          is_header=False, is_body=False, is_diffscuss=False, depth=0)),
    ('%* Header',
     dict(is_diff_meta=False, is_diff_range=False,
          is_header=True, is_body=False, is_diffscuss=True, depth=1)),
    ('%- Body',
     dict(is_diff_meta=False, is_diff_range=False,
          is_header=False, is_body=True, is_diffscuss=True, depth=1)),
    ('%***** Deep header',
     dict(is_diff_meta=False, is_diff_range=False,
          is_header=True, is_body=False, is_diffscuss=True, depth=5)),
    ('%----- Deep body',
     dict(is_diff_meta=False, is_diff_range=False,
          is_header=False, is_body=True, is_diffscuss=True, depth=5)),
    ('%*- Strange header',
     dict(is_diff_meta=False, is_diff_range=False,
          is_header=True, is_body=False, is_diffscuss=True, depth=1)),
    ('%-* Strange body',
     dict(is_diff_meta=False, is_diff_range=False,
          is_header=False, is_body=True, is_diffscuss=True, depth=1))
]

def test_line_properties():
    for line, expected_attrs in TEST_LINE_PROPERTIES:
        yield _check_line_properties, line, expected_attrs


def _check_line_properties(line, expected_attrs):
    props = editor.LineProperties(line)
    for attr, value in expected_attrs.iteritems():
        eq_(value, getattr(props, attr))


TEST_BUFFER_NONE = """
diff --git a/some/file b/some/file
index rev1..rev2 100644
--- a/some/file
+++ b/some/file
@@ -1,1 +1,2 @@
+diff1
 diff2
 diff3
""".strip().split('\n')

TEST_BUFFER_FILE = """
%*
%* author: Test
%* email: test@example.com
%* date: 2013-01-01T00:00:00-0500
%*
%- This is a test comment.
%-
%**
%** author: Test
%** email: test@example.com
%** date: 2013-01-01T00:01:00-0500
%**
%-- This is a test reply.
%--
diff --git a/some/file b/some/file
index rev1..rev2 100644
--- a/some/file
+++ b/some/file
@@ -1,1 +1,2 @@
+diff1
 diff2
 diff3
""".strip().split('\n')

TEST_BUFFER_BODY = """
diff --git a/some/file b/some/file
index rev1..rev2 100644
--- a/some/file
+++ b/some/file
@@ -1,1 +1,2 @@
+diff1
%*
%* author: Test
%* email: test@example.com
%* date: 2013-01-01T00:00:00-0500
%*
%- This is a test comment.
%-
%**
%** author: Test
%** email: test@example.com
%** date: 2013-01-01T00:01:00-0500
%**
%-- This is a test reply.
%--
 diff2
 diff3
""".strip().split('\n')


def test_find_header_start():
    for i in range(1, len(TEST_BUFFER_NONE) + 1):
        result = editor.find_header_start(TEST_BUFFER_NONE, (i , i))
        yield eq_, (i, i), result

    for i in range(1, len(TEST_BUFFER_FILE) + 1):
        result = editor.find_header_start(TEST_BUFFER_FILE, (i, 1))
        if i <= TEST_BUFFER_FILE.index('%**'):
            yield eq_, (1, 1), result
        elif i <= TEST_BUFFER_FILE.index('diff --git a/some/file b/some/file'):
            yield eq_, (7, 1), result
        else:
            yield eq_, (i, 1), result

    for i in range(1, len(TEST_BUFFER_BODY) + 1):
        result = editor.find_header_start(TEST_BUFFER_BODY, (i, 1))
        if i <= TEST_BUFFER_BODY.index('%*'):
            yield eq_, (i, 1), result
        elif i <= TEST_BUFFER_BODY.index('%**'):
            yield eq_, (6, 1), result
        elif i <= TEST_BUFFER_BODY.index(' diff2'):
            yield eq_, (13, 1), result
        else:
            yield eq_, (i, 1), result
