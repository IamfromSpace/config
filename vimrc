" If pathogen is not installed, run:
" mkdir -p ~/.vim/autoload ~/.vim/bundle && \
" curl -LSso ~/.vim/autoload/pathogen.vim https://tpo.pe/pathogen.vim

" start pathogen:
execute pathogen#infect()

" use 'hh' as an alternate to the Escape key
imap hh <Esc>

" show line numbers and the line/column and the bottom right
set nu
set ruler

" Use syntax highlighting and show whitespace characters as symbols
syntax on
set list

" Force the use of spaces instead of tabs
" indents always equal 2, and line breaks indent to previous indentation value
set expandtab
set sw=2
set tabstop=2
set autoindent

" Fix strange backspace settings in 7.4 for Mac
set backspace=indent,eol,start

" Recommended settings for Syntastic
" See (github.com/scrooloose/syntastic)
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

" My settings for Syntastic, sane colorings and number of lines
let g:syntastic_loc_list_height = 2
highlight Search ctermbg=0

" Default Synastic Checkers
let g:syntastic_javascript_checkers = ['eslint']
