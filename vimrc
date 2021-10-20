" If pathogen is not installed, run:
" mkdir -p ~/.vim/autoload ~/.vim/bundle && \
" curl -LSso ~/.vim/autoload/pathogen.vim https://tpo.pe/pathogen.vim

" elm-vim is dependent on plug beginning, but is loaded via pathogen
call plug#begin()
Plug 'sbdchd/neoformat'
call plug#end()

" start pathogen:
execute pathogen#infect()

noremap t <Up>
noremap n <Down>
noremap s <Right>

noremap T <PageUp>
noremap N <PageDown>

noremap f n
noremap F N

map <Left> <nop>
map <Right> <nop>
map <Up> <nop>
map <Down> <nop>

" Remap direction commands in the Explorer (netrw)
augroup netrw_mapping
    autocmd!
    autocmd filetype netrw call NetrwMapping()
augroup END

function! NetrwMapping()
    noremap <buffer> h <Left>
    noremap <buffer> t <Up>
    noremap <buffer> n <Down>
    noremap <buffer> s <Right>
endfunction


" show line numbers and the line/column and the bottom right
set nu
set ruler

" Use syntax highlighting and show whitespace characters as symbols
syntax on
set list

" Force the use of spaces instead of tabs
set expandtab

" Set line breaks indent to previous indentation value
set autoindent

" For js, indents always equal 2
autocmd Filetype javascript setlocal ts=2 sw=2
autocmd Filetype openscad setlocal ts=2 sw=2

" For elm, we match elm-format
autocmd Filetype elm setlocal ts=4 sw=4 sts=0

" Fix strange backspace settings in 7.4 for Mac
set backspace=indent,eol,start

" Always show the tab bar
set showtabline=2

" Autoformat JS on save
autocmd BufWritePre *.js Neoformat
autocmd BufWritePre *.hs Neoformat hindent

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

" elm-vim specific settings
let g:elm_syntastic_show_warnings = 1
let g:elm_format_autosave = 1
let g:elm_make_output_file = "index.html"

" Default Synastic Checkers
let g:syntastic_javascript_checkers = ['eslint']
let g:syntastic_elm_checkers = ['elm_make']

" Vim diff colors
hi DiffChange term=bold ctermbg=7
hi DiffText term=bold ctermbg=8
hi DiffAdd term=bold ctermbg=17
hi DiffDelete term=bold ctermbg=1

