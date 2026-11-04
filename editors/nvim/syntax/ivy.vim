" Vim syntax file for the Ivy programming language.

if exists("b:current_syntax")
  finish
endif

" Keywords
syn keyword ivyKeyword     let mut type newtype trait impl module import pub where do in from as for
syn keyword ivyConditional if then else match with end
syn keyword ivyBoolean     true false
syn keyword ivyOperatorKw  and or
syn keyword ivyFn          fn nextgroup=ivyFunction skipwhite

" Identifiers
" capitalized names are types / constructors
syn match ivyType     "\<[A-Z][A-Za-z0-9_]*\>"
" name immediately after `fn` (see nextgroup above)
syn match ivyFunction "[a-z_][A-Za-z0-9_]*[?!]\?" contained
" `__`-prefixed names are low-level intrinsics
syn match ivyBuiltin  "\<__[A-Za-z0-9_]\+\>"

" Literals
syn match  ivyNumber "\<\d\+\>"
syn match  ivyFloat  "\<\d\+\.\d\+\>"
syn match  ivyChar   "'\\\?.'"
syn region ivyString start=+"+ skip=+\\"+ end=+"+ contains=ivyStringEscape
syn match  ivyStringEscape "\\." contained

" Operators
syn match ivyOperator "|>\|::\|++\|->\|=>\|==\|!=\|<=\|>=\|[-+*/%<>=!]"

" Comments
" line comments `--` and nested block comments `{- ... -}`
syn match  ivyComment "--.*$" contains=@Spell
syn region ivyBlockComment start="{-" end="-}" contains=ivyBlockComment,@Spell

" Highlight
hi ivyKeyword     guifg=#FF66FF ctermfg=207 gui=bold cterm=bold
hi ivyFn          guifg=#FF66FF ctermfg=207 gui=bold cterm=bold
hi ivyConditional guifg=#FF66FF ctermfg=207 gui=bold cterm=bold
hi ivyBoolean     guifg=#FF66FF ctermfg=207
hi ivyOperator    guifg=#FF66FF ctermfg=207
hi ivyOperatorKw  guifg=#FF66FF ctermfg=207
hi ivyBuiltin     guifg=#FF66FF ctermfg=207
hi ivyType        guifg=#FFC636 ctermfg=221 gui=bold cterm=bold

hi def link ivyFunction     Function
hi def link ivyNumber       Constant
hi def link ivyFloat        Constant
hi def link ivyChar         Constant
hi def link ivyString       String
hi def link ivyStringEscape Special
hi def link ivyComment      Comment
hi def link ivyBlockComment Comment

let b:current_syntax = "ivy"
