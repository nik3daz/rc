set nocompatible

filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" let Vundle manage Vundle, required
Plugin 'gmarik/Vundle.vim'

Plugin 'tpope/vim-fugitive'
"Plugin 'Valloric/YouCompleteMe'
"Plugin 'L9'
Plugin 'FuzzyFinder'
Plugin 'chriskempson/base16-vim'
Plugin 'powerline/powerline', {'rtp': 'powerline/bindings/vim/'}
Plugin 'airblade/vim-gitgutter'

call vundle#end()            " required

" source /usr/share/vim/google/google.vim

syntax on

set ai
set si

set hid

set hlsearch
set incsearch

set noerrorbells
set expandtab
set smarttab
set shiftwidth=2
set tabstop=2

" Sroll offset, causes file lines to be visible around current line while scrolling.
set so=5

" Because I'm not a masochist.
set mouse=a
set nu

set wildmenu
set wildmode=longest:full,full
set ruler

" Map 0 to first non-blank character
map 0 ^
noremap <S-K> :ClangFormat<CR>

map gr gT
nmap <silent> <PageDown> :set scroll=0<CR>:set scroll^=2<CR>:set scroll-=1<CR><C-D>:set scroll=0<CR>
nmap <silent> <PageUp> :set scroll=0<CR>:set scroll^=2<CR>:set scroll-=1<CR><C-U>:set scroll=0<CR>
map <S-Up> <Up>
map <S-Down> <Down>

" Highlight tabs and trailing spaces
autocmd BufNewFile,BufRead * set list listchars=tab:>.,trail:-
" Highlight lines that are too long
autocmd BufNewFile,BufRead * match Error /\%>80v.\+/

map <C-p> :FufBuffer<CR>

let g:ycm_global_ycm_extra_conf = '~/Projects/chromium/src/tools/vim/chromium.ycm_extra_conf.py'

set t_Co=256
set background=dark
set termguicolors
let g:jellybeans_overrides = {
      \    'background': { 'guibg': '022527' },
      \}
colorscheme jellybeans
" colorscheme base16-solarized

filetype plugin indent on    " required
