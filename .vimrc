" don't bother trying to be compatible with vi
set nocompatible

" disable bells
set visualbell t_vb=
" no sound when at end of line
set noerrorbells

" https://vi.stackexchange.com/questions/24925/usage-of-timeoutlen-and-ttimeoutlen
set timeoutlen=1000 ttimeoutlen=0

" show possible matches in command-line completion
set wildmenu

" allow moving through buffers withouth writing each one every time
set hidden

" when there is a previous search pattern, do not highlight all its matches
set nohlsearch

" let backspace over indent, eol (join two lines) and start of insert
set backspace=indent,eol,start

" more risky, but cleaner
set nobackup
set noswapfile
set nowritebackup

" never show status line
set laststatus=0

" cursor always as a block
set guicursor=

" show current line number
set number

" enable mouse support
set mouse=nicr

" show command while is typed
set showcmd

" set encoding
set encoding=utf-8

" highlights while you are typing in / search mode
set incsearch

" set clipboard to + (system X clipboard)
set clipboard=unnamedplus

" tab size
let indent=4
let &tabstop=indent
let &softtabstop=indent
let &shiftwidth=indent

" tells vim to apply the indentation of the current line to the next
set autoindent

" substitute tabs with spaces
set expandtab

" suppress netrw banner
" let g:netrw_banner=0
" netrw tree style 0=thin; 1=long; 2=wide; 3=tree
let g:netrw_liststyle=1

" faster scrolling
set ttyfast

" set every files as Unix (LF) as Windows might set CRLF wich is not working for unix
set fileformat=unix

" syntax highlighting
syntax on
" set t_Co=0

" https://vi.stackexchange.com/questions/10124/what-is-the-difference-between-filetype-plugin-indent-on-and-filetype-indent
filetype plugin indent on

set omnifunc=syntaxcomplete#Complete

" options for code completion in insert mode
set completeopt=longest,menuone,menu

" ctags
command! MakeTags !ctags -R .

" remap resize splits
nnoremap <silent> <C-Up> :resize -2<CR>
nnoremap <silent> <C-Down> :resize +2<CR>
nnoremap <silent> <C-Left> :vertical resize -2<CR>
nnoremap <silent> <C-Right> :vertical resize +2<CR>

" make new splits position more intuitive
set splitbelow splitright
