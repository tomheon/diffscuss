"""
Functions for adapting the generic buffer/cursor manipulation code in editor.py
to Vim.
"""
import vim


def config():
    """
    Returns the current configuration for diffscuss.vim from Vim as a dict.
    """
    return dict(vim.eval('g:diffscuss_config'))


def return_expr(func):
    """
    Calls `func` with the current buffer and cursor as arguments, and returns
    the resulting expression as a string.
    """
    win = vim.current.window
    return vim.command('return "%s"' % func(win.buffer, win.cursor))


def update_buffer(*funcs):
    """
    For each function argument, updates the cursor to the result of calling
    the function on the current buffer and cursor.
    """
    win = vim.current.window
    for func in funcs:
        win.cursor = func(win.buffer, win.cursor)


def run_mailbox(func):
    """
    Calls the function, passing it the name of the current buffer and a
    function with which it can prompt the user.

    If the function returns non-None, the result will be displayed to the user.
    If it raises an exception, the exception will be displayed to the user.
    """
    buffer_name = vim.current.buffer.name
    prompt_func = lambda prompt: vim.eval('input("%s")' % prompt)
    try:
        message = func(buffer_name, prompt_func)
        vim.command("silent! normal! :bd %s\n" % buffer_name)
        if message:
            vim.command('echom "%s"' % message)
    except Exception, e:
        vim.command('echom "%s"' % e)


def open_preview(func):
    """
    Calls the function, passing it the current buffer and cursor and the path
    to a tempfile (managed by Vim, removed on Vim close).

    If the function does not raise an exception, the contents of the tempfile
    will be opened in the preview window. If the function returns non-None,
    that value will be used as the line number to jump to in the preview
    window.

    If the function raises an exception, the exception will be displayed to the
    user.
    """
    tempfile = vim.eval('tempname()')
    win = vim.current.window
    try:
        lineno = func(win.buffer, win.cursor, tempfile)
        if lineno is not None:
            vim.command("normal! :pedit +%d %s\n" % (lineno, tempfile))
        else:
            vim.command("normal! :pedit %s\n" % tempfile)
    except Exception, e:
        vim.command('echom "%s"' % e)


def open_file(func):
    """
    Calls the function, passing it the current buffer and cursor.

    If the function does not raise an exception, the return value of the
    function will be opened in the current window.

    If the function raises an exception, the exception will be displayed to the
    user.
    """
    win = vim.current.window
    try:
        filename = func(win.buffer, win.cursor)
        vim.command("normal! :e %s\n" % filename)
    except Exception, e:
        vim.command('echom "%s"' % e)
