if !exists("g:diffscuss_config")
    echoerr "g:diffscuss_config is not set!"
    finish
endif

" === Mappings and setup

" Comment insertion
nnoremap <buffer> <leader>di :<c-u>call <SID>InsertComment()<cr>
nnoremap <buffer> <leader>df :<c-u>call <SID>InsertFileComment()<cr>
nnoremap <buffer> <leader>dr :<c-u>call <SID>ReplyToComment()<cr>
nnoremap <buffer> <leader>dd :<c-u>call <SID>InsertContextualComment()<cr>

" Showing source
nnoremap <buffer> <silent> <leader>dn :<c-u>call <SID>ShowNewSource()<cr>
nnoremap <buffer> <silent> <leader>do :<c-u>call <SID>ShowOldSource()<cr>
nnoremap <buffer> <silent> <leader>ds :<c-u>call <SID>ShowLocalSource()<cr>

" Mailboxes
noremap <buffer> <leader>mp :<c-u>call <SID>MailboxPost()<cr>
nnoremap <buffer> <leader>mb :<c-u>call <SID>MailboxBounce()<cr>
nnoremap <buffer> <leader>md :<c-u>call <SID>MailboxDone()<cr>
nnoremap <buffer> <leader>mc :<c-u>call <SID>MailboxCheck()<cr>

" Navigation
nnoremap <buffer> <silent> ]d :<c-u>call <SID>NextComment()<cr>
nnoremap <buffer> <silent> [d :<c-u>call <SID>PrevComment()<cr>
nnoremap <buffer> <silent> ]D :<c-u>call <SID>NextCommentEnd()<cr>
nnoremap <buffer> <silent> [D :<c-u>call <SID>PrevCommentEnd()<cr>
nnoremap <buffer> <silent> ]t :<c-u>call <SID>NextThread()<cr>
nnoremap <buffer> <silent> [t :<c-u>call <SID>PrevThread()<cr>
nnoremap <buffer> <silent> ]T :<c-u>call <SID>NextThreadEnd()<cr>
nnoremap <buffer> <silent> [T :<c-u>call <SID>PrevThreadEnd()<cr>

" Auto-formatting
set comments=nb:%-,nb:%*,nb:%--,nb:%**,nb:%---,nb:%***,nb:%----,nb:%****,nb:%-----,nb:%******,nb:%------,nb:%*******,nb:%-------,nb:%********
set formatoptions=tcqron

" Folding
" set foldmethod=expr
set foldexpr=DiffscussFold(v:lnum)

function! DiffscussFold(lnum)
    let match = matchlist(getline(a:lnum), "^%[*-]\\+")
    if match != []
        return string(len(match[0]) - 1)
    else
        return '0'
    endif
endfunction

" Load Python functions
let s:diffscuss_dir = g:diffscuss_config['diffscuss_dir']
execute printf("pyfile %s/support/vimhelper.py", s:diffscuss_dir)
execute printf("pyfile %s/support/editor.py", s:diffscuss_dir)


" === Comment insertion

function! s:InsertComment()
    python update_buffer(insert_comment)
    start!
endfunction

function! s:InsertFileComment()
    python update_buffer(insert_file_comment)
    start!
endfunction

function! s:ReplyToComment()
    python update_buffer(reply_to_comment)
    start!
endfunction

function! s:InsertContextualComment()
    python update_buffer(insert_contextual_comment)
    start!
endfunction

" === Showing source

function! s:ShowOldSource()
    python open_preview(show_old_source)
endfunction

function! s:ShowNewSource()
    python open_preview(show_new_source)
endfunction

function! s:ShowLocalSource()
    python open_file(show_local_source)
endfunction

" === Mailboxes

function! s:MailboxPost()
    python run_mailbox(mailbox_post)
endfunction

function! s:MailboxBounce()
    python run_mailbox(mailbox_bounce)
endfunction

function! s:MailboxDone()
    python run_mailbox(mailbox_done)
endfunction

function! s:MailboxCheck()
    python open_preview(mailbox_check)
endfunction

" === Navigation

function! s:NextComment()
    python update_buffer(find_next_comment)
endfunction

function! s:PrevComment()
    python update_buffer(find_prev_comment)
endfunction

function! s:NextThread()
    python update_buffer(find_next_thread)
endfunction

function! s:PrevThread()
    python update_buffer(find_prev_thread)
endfunction

function! s:NextCommentEnd()
    python update_buffer(find_next_comment_end)
endfunction

function! s:PrevCommentEnd()
    python update_buffer(find_prev_comment_end)
endfunction

function! s:NextThreadEnd()
    python update_buffer(find_next_thread_end)
endfunction

function! s:PrevThreadEnd()
    python update_buffer(find_prev_thread_end)
endfunction
