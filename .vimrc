" do not make vim compatible with vi
set nocompatible

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

" cursor always as a block
set guicursor=

" show current line number
set number

" enable mouse support
set mouse=a

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

" substitute tabs with spaces
set expandtab

" suppress netrw banner
" let g:netrw_banner=0
" netrw tree style 0=thin; 1=long; 2=wide; 3=tree
let g:netrw_liststyle=1

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
