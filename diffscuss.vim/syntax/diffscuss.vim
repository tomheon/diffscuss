if exists("b:current_syntax")
  finish
endif

runtime! syntax/diff.vim
unlet b:current_syntax

syn keyword diffscussHeaderField author email date contained
syn match diffscussHeader "^%\*\+.*$" contains=diffscussHeaderField
syn match diffscussBody "^%-\+.*$"
hi link diffscussHeaderField Type
hi link diffscussHeader Todo
hi link diffscussBody Comment

let b:current_syntax = "diffscuss"
