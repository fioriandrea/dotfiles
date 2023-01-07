if filereadable(expand("~/.vim/autoload/plug.vim"))
    call plug#begin()
    Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
    Plug 'junegunn/fzf.vim'
    call plug#end()

    nnoremap <C-p> :Files<Cr>
    nnoremap <C-g> :Rg<Cr>
    nnoremap <C-u> :Buffers<Cr>
endif
