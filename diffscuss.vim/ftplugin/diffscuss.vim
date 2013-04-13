" === Mappings and setup

" Comment insertion
nnoremap <buffer> <silent> <leader>di :<c-u>call DiffscussInsertComment()<cr>
nnoremap <buffer> <silent> <leader>df :<c-u>call DiffscussInsertFileComment()<cr>
nnoremap <buffer> <silent> <leader>dr :<c-u>call DiffscussReplyToComment()<cr>
nnoremap <buffer> <silent> <leader>dd :<c-u>call DiffscussInsertContextualComment()<cr>

" Showing source
nnoremap <buffer> <silent> <leader>dn :<c-u>call DiffscussShowNewSource()<cr>
nnoremap <buffer> <silent> <leader>do :<c-u>call DiffscussShowOldSource()<cr>
nnoremap <buffer> <silent> <leader>ds :<c-u>call DiffscussShowLocalSource()<cr>

" Mailboxes
nnoremap <buffer> <silent> <leader>mp :<c-u>call DiffscussMailboxPost()<cr>
nnoremap <buffer> <silent> <leader>mb :<c-u>call DiffscussMailboxBounce()<cr>
nnoremap <buffer> <silent> <leader>md :<c-u>call DiffscussMailboxDone()<cr>

" Navigation
nnoremap <buffer> <silent> ]d :<c-u>call DiffscussNextComment()<cr>
nnoremap <buffer> <silent> [d :<c-u>call DiffscussPrevComment()<cr>
nnoremap <buffer> <silent> ]D :<c-u>call DiffscussNextCommentEnd()<cr>
nnoremap <buffer> <silent> [D :<c-u>call DiffscussPrevCommentEnd()<cr>
nnoremap <buffer> <silent> ]t :<c-u>call DiffscussNextThread()<cr>
nnoremap <buffer> <silent> [t :<c-u>call DiffscussPrevThread()<cr>
nnoremap <buffer> <silent> ]T :<c-u>call DiffscussNextThreadEnd()<cr>
nnoremap <buffer> <silent> [T :<c-u>call DiffscussPrevThreadEnd()<cr>

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


" === Comment insertion

function! DiffscussInsertComment()
    python update_buffer(insert_comment)
    start!
endfunction

function! DiffscussInsertFileComment()
    python update_buffer(insert_file_comment)
    start!
endfunction

function! DiffscussReplyToComment()
    python update_buffer(reply_to_comment)
    start!
endfunction

function! DiffscussInsertContextualComment()
    python update_buffer(insert_contextual_comment)
    start!
endfunction

" === Showing source

function! DiffscussShowOldSource()
    python open_preview(show_old_source)
endfunction

function! DiffscussShowNewSource()
    python open_preview(show_new_source)
endfunction

function! DiffscussShowLocalSource()
    python open_file(show_local_source)
endfunction

" === Mailboxes

function! DiffscussMailboxPost()
    python run_mailbox(mailbox_post)
endfunction

function! DiffscussMailboxBounce()
    python run_mailbox(mailbox_bounce)
endfunction

function! DiffscussMailboxDone()
    python run_mailbox(mailbox_done)
endfunction

" === Navigation

function! DiffscussNextComment()
    python update_buffer(find_next_comment)
endfunction

function! DiffscussPrevComment()
    python update_buffer(find_prev_comment)
endfunction

function! DiffscussNextThread()
    python update_buffer(find_next_thread)
endfunction

function! DiffscussPrevThread()
    python update_buffer(find_prev_thread)
endfunction

function! DiffscussNextCommentEnd()
    python update_buffer(find_next_comment_end)
endfunction

function! DiffscussPrevCommentEnd()
    python update_buffer(find_prev_comment_end)
endfunction

function! DiffscussNextThreadEnd()
    python update_buffer(find_next_thread_end)
endfunction

function! DiffscussPrevThreadEnd()
    python update_buffer(find_prev_thread_end)
endfunction
