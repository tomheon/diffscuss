if !exists("g:diffscuss_config")
    echoerr "g:diffscuss_config is not set!"
    finish
endif

" Load Python functions
let s:diffscuss_dir = g:diffscuss_config['diffscuss_dir']
execute printf("pyfile %s/support/vimhelper.py", s:diffscuss_dir)
execute printf("pyfile %s/support/editor.py", s:diffscuss_dir)

nnoremap <buffer> <silent> <leader>mc :<c-u>call DiffscussMailboxCheck()<cr>

function! DiffscussMailboxCheck()
    python open_preview(mailbox_check)
endfunction
