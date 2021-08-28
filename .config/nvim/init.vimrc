call plug#begin('~/.vim/plugged')
    Plug 'itchyny/lightline.vim'                                  " Lightline statusbar
    Plug 'preservim/nerdtree'                                     " Nerdtree
    Plug 'tiagofumo/vim-nerdtree-syntax-highlight'                " Highlighting Nerdtree
    Plug 'ryanoasis/vim-devicons'                                 " Icons for Nerdtree
    Plug 'tpope/vim-surround'                                     " Change surrounding marks
    Plug 'arcticicestudio/nord-vim'                               " Nord theme
    Plug 'raimondi/delimitmate'                                   " Autocomplete closing delimiters 
    " Plug 'cespare/vim-toml'                                       " toml syntax highlighting
    Plug 'nvim-treesitter/nvim-treesitter', { 'do': ':TSUpdate' } " Treesitter support
call plug#end()
