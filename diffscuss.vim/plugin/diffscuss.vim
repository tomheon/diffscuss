" === Set up configuration defaults if necessary.

function! s:Run(command)
    let s:output = system(a:command)
    return substitute(s:output, '\n\\*', '', 'g')
endfunction

if !exists("g:diffscuss_config")
    let g:diffscuss_config = {}
endif
if !has_key(g:diffscuss_config, "author")
    " Try name from git config, then real name from /etc/passwd, then $USER.
    let g:diffscuss_config["author"] = <SID>Run("
        \ git config --get user.name || echo $USER")
endif
if !has_key(g:diffscuss_config, "email")
    " Try email from git config, then $USER@host.
    let g:diffscuss_config["email"] = <SID>Run("
        \ git config --get user.email || echo $USER@$(hostname)")
endif
if !has_key(g:diffscuss_config, "diffscuss_dir")
    " Determine the path starting from this script file.
    let g:diffscuss_config["diffscuss_dir"] = expand("<sfile>:p:h:h:h")
endif


" === Load Python functions

let s:diffscuss_dir = g:diffscuss_config["diffscuss_dir"]
execute printf("pyfile %s/support/vimhelper.py", s:diffscuss_dir)
execute printf("pyfile %s/support/editor.py", s:diffscuss_dir)


" === Global mappings

nnoremap <buffer> <silent> <leader>mc :<c-u>call DiffscussMailboxCheck()<cr>

function! DiffscussMailboxCheck()
    python open_preview(mailbox_check)
endfunction
