"""
Test cases for the editor module.
"""
from functools import wraps

from nose.tools import eq_

from diffscuss.support import editor


class patch(object):
    """
    Quick and dirty patching. Use it as a decorator or as a context manager.
    """
    PATCH_REMOVE = object()

    def __init__(self, obj, attr, new):
        self.obj = obj
        self.attr = attr
        self.new = new
        self.patch_attr = '_patched_' + attr

    def _patch(self):
        """
        Sets `obj.attr` to `new`, saving the original value of `obj.attr` (if
        there was one) for later patching.
        """
        if not hasattr(self.obj, self.patch_attr):
            setattr(self.obj, self.patch_attr, [])
        saved = getattr(self.obj, self.attr, self.PATCH_REMOVE)
        getattr(self.obj, self.patch_attr).append(saved)
        setattr(self.obj, self.attr, self.new)

    def _unpatch(self):
        """
        Unsets `obj.attr`, restoring its original value if there was one.
        """
        assert hasattr(self.obj, self.patch_attr)
        restore_list = getattr(self.obj, self.patch_attr)
        to_restore = restore_list.pop()
        if to_restore is self.PATCH_REMOVE:
            delattr(self.obj, self.attr)
        else:
            setattr(self.obj, self.attr, to_restore)
        if not restore_list:
            delattr(self.obj, self.patch_attr)

    def __call__(self, func):
        """
        A decorator that patches `obj.attr` to `new` within the decorated
        function.
        """
        @wraps(func)
        def _wrapped(*args, **kwargs):
            with self:
                return func(*args, **kwargs)
        return _wrapped

    def __enter__(self):
        self._patch()

    def __exit__(self, exc_type, value, traceback):
        self._unpatch()


config_patch = patch(editor, 'config',
                     lambda: dict(author='Test', email='test@example.com'))


def setup_module():
    # Patch in a config() function with test defaults.
    config_patch._patch()


def teardown_module():
    config_patch._unpatch()


class BufferWrapper(list):
    """
    Adapts a Python list to the Vim buffer interface.
    """
    def append(self, obj, index=None):
        if not hasattr(obj, '__iter__'):
            obj = [obj]
        if index is None:
            for item in obj:
                list.append(self, obj)
        else:
            for item in obj:
                list.insert(self, index, item)
                index += 1


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
    ('#* Header',
     dict(is_diff_meta=False, is_diff_range=False,
          is_header=True, is_body=False, is_diffscuss=True, depth=1)),
    ('#- Body',
     dict(is_diff_meta=False, is_diff_range=False,
          is_header=False, is_body=True, is_diffscuss=True, depth=1)),
    ('#***** Deep header',
     dict(is_diff_meta=False, is_diff_range=False,
          is_header=True, is_body=False, is_diffscuss=True, depth=5)),
    ('#----- Deep body',
     dict(is_diff_meta=False, is_diff_range=False,
          is_header=False, is_body=True, is_diffscuss=True, depth=5)),
    ('#*- Strange header',
     dict(is_diff_meta=False, is_diff_range=False,
          is_header=True, is_body=False, is_diffscuss=True, depth=1)),
    ('#-* Strange body',
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
#*
#* author: Test
#* email: test@example.com
#* date: 2013-01-01T00:00:00-0500
#*
#- This is a test comment.
#-
#**
#** author: Test
#** email: test@example.com
#** date: 2013-01-01T00:01:00-0500
#**
#-- This is a test reply.
#--
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
#*
#* author: Test
#* email: test@example.com
#* date: 2013-01-01T00:00:00-0500
#*
#- This is a test comment.
#-
#**
#** author: Test
#** email: test@example.com
#** date: 2013-01-01T00:01:00-0500
#**
#-- This is a test reply.
#--
 diff2
 diff3
""".strip().split('\n')

TEST_BUFFER_END = """
diff --git a/some/file b/some/file
index rev1..rev2 100644
--- a/some/file
+++ b/some/file
@@ -1,1 +1,2 @@
+diff1
 diff2
 diff3
#*
#* author: Test
#* email: test@example.com
#* date: 2013-01-01T00:00:00-0500
#*
#- This is a test comment.
#-
""".strip().split('\n')


def test_find_header_start():
    for i in range(1, len(TEST_BUFFER_NONE) + 1):
        result = editor.find_header_start(TEST_BUFFER_NONE, (i, i))
        yield eq_, (i, i), result

    for i in range(1, len(TEST_BUFFER_FILE) + 1):
        result = editor.find_header_start(TEST_BUFFER_FILE, (i, 1))
        if i <= TEST_BUFFER_FILE.index('#**'):
            yield eq_, (1, 1), result
        elif i <= TEST_BUFFER_FILE.index('diff --git a/some/file b/some/file'):
            yield eq_, (7, 1), result
        else:
            yield eq_, (i, 1), result

    for i in range(1, len(TEST_BUFFER_BODY) + 1):
        result = editor.find_header_start(TEST_BUFFER_BODY, (i, 1))
        if i <= TEST_BUFFER_BODY.index('#*'):
            yield eq_, (i, 1), result
        elif i <= TEST_BUFFER_BODY.index('#**'):
            yield eq_, (6, 1), result
        elif i <= TEST_BUFFER_BODY.index(' diff2'):
            yield eq_, (13, 1), result
        else:
            yield eq_, (i, 1), result


def test_find_body_end():
    for i in range(1, len(TEST_BUFFER_NONE) + 1):
        result = editor.find_body_end(TEST_BUFFER_NONE, (i, i))
        yield eq_, (i, i), result

    for i in range(1, len(TEST_BUFFER_END) + 1):
        result = editor.find_body_end(TEST_BUFFER_END, (i, i))
        yield eq_, (i, i), result

    for i in range(1, len(TEST_BUFFER_FILE) + 1):
        result = editor.find_body_end(TEST_BUFFER_FILE, (i, 1))
        if i <= TEST_BUFFER_FILE.index('#**'):
            yield eq_, (7, 1), result
        elif i <= TEST_BUFFER_FILE.index('diff --git a/some/file b/some/file'):
            yield eq_, (14, 1), result
        else:
            yield eq_, (i, 1), result

    for i in range(1, len(TEST_BUFFER_BODY) + 1):
        result = editor.find_body_end(TEST_BUFFER_BODY, (i, 1))
        if i <= TEST_BUFFER_BODY.index('#*'):
            yield eq_, (i, 1), result
        elif i <= TEST_BUFFER_BODY.index('#**'):
            yield eq_, (13, 1), result
        elif i <= TEST_BUFFER_BODY.index(' diff2'):
            yield eq_, (20, 1), result
        else:
            yield eq_, (i, 1), result


def test_find_subthread_end():
    for i in range(1, len(TEST_BUFFER_NONE) + 1):
        result = editor.find_subthread_end(TEST_BUFFER_NONE, (i, i))
        yield eq_, (i, i), result

    for i in range(1, len(TEST_BUFFER_END) + 1):
        result = editor.find_subthread_end(TEST_BUFFER_END, (i, i))
        yield eq_, (i, i), result

    for i in range(1, len(TEST_BUFFER_FILE) + 1):
        result = editor.find_subthread_end(TEST_BUFFER_FILE, (i, 1))
        if i <= TEST_BUFFER_FILE.index('#**'):
            yield eq_, (14, 1), result
        elif i <= TEST_BUFFER_FILE.index('diff --git a/some/file b/some/file'):
            yield eq_, (14, 1), result
        else:
            yield eq_, (i, 1), result

    for i in range(1, len(TEST_BUFFER_BODY) + 1):
        result = editor.find_subthread_end(TEST_BUFFER_BODY, (i, 1))
        if i <= TEST_BUFFER_BODY.index('#*'):
            yield eq_, (i, 1), result
        elif i <= TEST_BUFFER_BODY.index('#**'):
            yield eq_, (20, 1), result
        elif i <= TEST_BUFFER_BODY.index(' diff2'):
            yield eq_, (20, 1), result
        else:
            yield eq_, (i, 1), result


def test_find_thread_end():
    for i in range(1, len(TEST_BUFFER_NONE) + 1):
        result = editor.find_thread_end(TEST_BUFFER_NONE, (i, i))
        yield eq_, (i, i), result

    for i in range(1, len(TEST_BUFFER_FILE) + 1):
        result = editor.find_thread_end(TEST_BUFFER_FILE, (i, 1))
        if i <= TEST_BUFFER_FILE.index('#**'):
            yield eq_, (14, 1), result
        elif i <= TEST_BUFFER_FILE.index('diff --git a/some/file b/some/file'):
            yield eq_, (14, 1), result
        else:
            yield eq_, (i, 1), result

    for i in range(1, len(TEST_BUFFER_BODY) + 1):
        result = editor.find_thread_end(TEST_BUFFER_BODY, (i, 1))
        if i <= TEST_BUFFER_BODY.index('#*'):
            yield eq_, (i, 1), result
        elif i <= TEST_BUFFER_BODY.index('#**'):
            yield eq_, (20, 1), result
        elif i <= TEST_BUFFER_BODY.index(' diff2'):
            yield eq_, (20, 1), result
        else:
            yield eq_, (i, 1), result


def test_find_range():
    for buf in [TEST_BUFFER_NONE, TEST_BUFFER_FILE, TEST_BUFFER_BODY]:
        for i in range(1, len(buf) + 1):
            result = editor.find_range(buf, (i, i))
            if i <= buf.index('@@ -1,1 +1,2 @@'):
                yield eq_, (buf.index('@@ -1,1 +1,2 @@') + 1, i), result
            else:
                yield eq_, (i, i), result


@patch(editor.time, 'strftime', lambda arg: '2013-01-01T01:01:01-0500')
def test_make_comment():
    eq_(['#*', '#* author: Test', '#* email: test@example.com',
         '#* date: 2013-01-01T01:01:01-0500', '#*', '#- ', '#-'],
        editor.make_comment(depth=1))
    eq_(['#**', '#** author: Test', '#** email: test@example.com',
         '#** date: 2013-01-01T01:01:01-0500', '#**', '#-- ', '#--'],
        editor.make_comment(depth=2))
    eq_(['#*', '#* author: Test', '#* email: test@example.com',
         '#* date: 2013-01-01T01:01:01-0500', '#*', '#- ', '#-'],
        editor.make_comment(depth=0))

    with patch(editor, 'config', lambda: dict()):
        eq_(['#*', '#* author: Unknown', '#* email: Unknown',
             '#* date: 2013-01-01T01:01:01-0500', '#*', '#- ', '#-'],
            editor.make_comment(depth=1))


@patch(editor.time, 'strftime', lambda arg: '2013-01-01T00:00:00-0500')
def test_inject_comment():
    new_buf = BufferWrapper(list(TEST_BUFFER_NONE))
    result = editor.inject_comment(new_buf, (6, 1))
    eq_((12, 3), result)
    eq_(['diff --git a/some/file b/some/file',
         'index rev1..rev2 100644',
         '--- a/some/file',
         '+++ b/some/file',
         '@@ -1,1 +1,2 @@',
         '+diff1',
         '#*',
         '#* author: Test',
         '#* email: test@example.com',
         '#* date: 2013-01-01T00:00:00-0500',
         '#*',
         '#- ',
         '#-',
         ' diff2',
         ' diff3'], new_buf)


@patch(editor.time, 'strftime', lambda arg: '2013-01-01T00:00:00-0500')
def test_insert_comment():
    new_buf = BufferWrapper(list(TEST_BUFFER_BODY))
    result = editor.insert_comment(new_buf, (7, 1))
    eq_((26, 3), result)
    eq_(['diff --git a/some/file b/some/file',
         'index rev1..rev2 100644',
         '--- a/some/file',
         '+++ b/some/file',
         '@@ -1,1 +1,2 @@',
         '+diff1',
         '#*',
         '#* author: Test',
         '#* email: test@example.com',
         '#* date: 2013-01-01T00:00:00-0500',
         '#*',
         '#- This is a test comment.',
         '#-',
         '#**',
         '#** author: Test',
         '#** email: test@example.com',
         '#** date: 2013-01-01T00:01:00-0500',
         '#**',
         '#-- This is a test reply.',
         '#--',
         '#*',
         '#* author: Test',
         '#* email: test@example.com',
         '#* date: 2013-01-01T00:00:00-0500',
         '#*',
         '#- ',
         '#-',
         ' diff2',
         ' diff3'], new_buf)
