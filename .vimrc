" do not make vim compatible with vi
set nocompatible

" disable bells
set noerrorbells
set novisualbell
set t_vb=

" https://vi.stackexchange.com/questions/24925/usage-of-timeoutlen-and-ttimeoutlen
set timeoutlen=1000 ttimeoutlen=0

" set to auto read when a file is changed from the outside
set autoread

" show possible matches in command-line completion
set wildmenu
set wildmode=full:lastused
set wildignore=*.o,*~,*.pyc
if has("win16") || has("win32")
    set wildignore+=.git\*,.hg\*,.svn\*
else
    set wildignore+=*/.git/*,*/.hg/*,*/.svn/*,*/.DS_Store
endif
set wildignore+=tags
set wildignorecase

" allow moving through buffers withouth writing each one every time
set hidden

" do not highlight all matches of a search pattern
set nohlsearch

" let backspace over indent, eol (join two lines) and start of insert
set backspace=indent,eol,start

" disable backup and swap files
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

if has("clipboard")
    " copy to the system clipboard
    set clipboard=unnamed
    " X11 support
    if has("unnamedplus")
        set clipboard=unnamedplus
    endif
endif

" tab size
let indent=4
let &tabstop=indent
let &softtabstop=indent
let &shiftwidth=indent

" tells vim to apply the indentation of the current line to the next
set autoindent

" smart indent
set smartindent

" substitute tabs with spaces
set expandtab

" suppress netrw banner
" let g:netrw_banner=0
" netrw tree style 0=thin; 1=long; 2=wide; 3=tree
let g:netrw_liststyle=1

" faster scrolling
set ttyfast

" set every files as Unix (LF line ending) as Windows might set CRLF
set fileformat=unix

" syntax highlighting
if has("syntax")
    syntax off
    highlight LineNr NONE
    " set t_Co=0
endif


" https://vi.stackexchange.com/questions/10124/what-is-the-difference-between-filetype-plugin-indent-on-and-filetype-indent
filetype plugin indent on

set omnifunc=syntaxcomplete#Complete

" options for code completion in insert mode
set completeopt=longest,menuone

" map resize splits
nnoremap <silent> <C-Up> :resize -2<CR>
nnoremap <silent> <C-Down> :resize +2<CR>
nnoremap <silent> <C-Left> :vertical resize -2<CR>
nnoremap <silent> <C-Right> :vertical resize +2<CR>

" map some buffer commands
nnoremap gp :bp<CR>
nnoremap gn :bn<CR>
nnoremap gl :ls<CR>
nnoremap gb :ls<CR>:b 
nnoremap gc :bd<CR>

" make new splits position more intuitive
set splitbelow splitright
