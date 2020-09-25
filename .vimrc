" plugins

if filereadable(expand("~/.vim/autoload/plug.vim"))
    call plug#begin()
    Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
    Plug 'junegunn/fzf.vim'
    call plug#end()

    nnoremap <C-p> :Files<Cr>
    nnoremap <C-g> :Rg<Cr>
    nnoremap <C-u> :Buffers<Cr>
endif

" normal config

" disable bells
set visualbell t_vb=
" no sound when at end of line
set noerrorbells

" https://vi.stackexchange.com/questions/24925/usage-of-timeoutlen-and-ttimeoutlen
set timeoutlen=1000 ttimeoutlen=0

" search recursively
set path+=**

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

" use relative line numbers
set relativenumber

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
let g:netrw_banner=0
" netrw tree style
let g:netrw_liststyle=3

" faster scrolling
set ttyfast

"set every files as Unix (LF) as Windows might set CRLF wich is not working for unix
set fileformat=unix

" syntax highlighting
syntax on
colorscheme elflord

" https://vi.stackexchange.com/questions/10124/what-is-the-difference-between-filetype-plugin-indent-on-and-filetype-indent
filetype plugin indent on

" options for code completion in insert mode
set completeopt=longest,menuone,menu

" ctags
command! MakeTags !ctags -R .

" remap resize splits
nnoremap <silent> <C-Up> :resize -2<CR>
nnoremap <silent> <C-Down> :resize +2<CR>
nnoremap <silent> <C-Left> :vertical resize -2<CR>
nnoremap <silent> <C-Right> :vertical resize +2<CR>

" move to the previous buffer
nnoremap gp :bp<CR>
" move to the next buffer
nnoremap gn :bn<CR>
" list all possible buffers
nnoremap gl :ls<CR>
" list all possible buffers and accept a new buffer argument
nnoremap gb :ls<CR>:b 
" close current buffer
nnoremap gc :bd<CR>

" move lines
nnoremap <silent> <C-j> :m .+1<CR>==
nnoremap <silent> <C-k> :m .-2<CR>==
inoremap <silent> <C-j> <Esc>:m .+1<CR>==gi
inoremap <silent> <C-k> <Esc>:m .-2<CR>==gi
vnoremap <silent> <C-j> :m '>+1<CR>gv=gv
vnoremap <silent> <C-k> :m '<-2<CR>gv=gv

" undo break points
inoremap , ,<c-g>u
inoremap . .<c-g>u
inoremap / /<c-g>u
inoremap ? ?<c-g>u
inoremap : :<c-g>u
inoremap ; ;<c-g>u
inoremap <space> <space><c-g>u

