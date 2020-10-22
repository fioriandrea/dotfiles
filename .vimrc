" syntax highlighting
syntax enable

" status line

set laststatus=2 "always show status line
set statusline=%F%m%r%h%w\ [dec=%3.3b]\ [hex=%02.2B]\ [pos=%04l:%04v][%p%%\ of\ %L]

" highlight

" hi CursorLine   cterm=NONE ctermbg=9 " ctermfg=white  
" hi CursorColumn cterm=NONE ctermbg=9 " ctermfg=white  
" set cursorline 
" set cursorcolumn

" enable mouse support

set mouse=nicr

" show command while is typed
set showcmd

" no sound when at end of line
set noerrorbells

" set encoding
set encoding=utf-8

" tells vim to apply the indentation of the current line to the next 
set autoindent

" it's gonna try to indent for you
set smartindent

filetype plugin indent on

" highlights while you are typing in \ search mode
set incsearch

" set clipboard to + (system X clipboard)
set clipboard=unnamedplus

" tab size
set tabstop=4
" >> and << size
set shiftwidth=4
" substitute tabs with spaces
" set expandtab

" enable omnicompletion
filetype plugin on
set completeopt=longest,menuone
set omnifunc=syntaxcomplete#Complete

" completion "

inoremap <expr> <CR> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"

function! Smart_TabComplete()
  let line = getline('.')                         " current line

  let substr = strpart(line, -1, col('.'))      " from the start of the current
                                                " to where the cursor is
  let substr = matchstr(substr, "[^ \t]*$")       " word till cursor (remove leading blanks)
  if (strlen(substr)==0)                          " nothing to match on empty string
    return "\<tab>"
  endif
  let has_slash = match(substr, '\/') != -1       " position of slash, if any
  if (has_slash)
    return "\<C-X>\<C-F>"                         " file matching
  else
    return "\<C-N>" 
  endif
endfunction
inoremap <expr> <tab> Smart_TabComplete()

" Shortcutting split navigation, saving a keypress
nnoremap <C-h> <C-W>h
nnoremap <C-j> <C-W>j
nnoremap <C-k> <C-W>k
nnoremap <C-l> <C-W>l

" remap resize splits
nmap <Up> :resize +3<CR> 
nmap <Down> :resize -3<CR>
nmap <Left> :vertical resize +3<CR>
nmap <Right> :vertical resize -3<CR>

" splits vert to horiz and viceversa
noremap <leader>h <C-W>H
noremap <leader>k <C-W>K

" remap go normal mode terminal splits
tnoremap <Esc><Esc> <C-W>N

" Show current line number
set number
" Show relative line numbers
set relativenumber

" map space to leader
nnoremap <Space> <Nop>
map <Space> <leader>
