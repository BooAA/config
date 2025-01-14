set nocompatible

set ignorecase
set smartcase
set incsearch
set hlsearch

set mouse=a

syntax enable
colorscheme zenburn
hi Normal ctermbg=none
set lazyredraw

set autoindent
filetype plugin on
filetype indent on

set nobackup
set nowritebackup
set noswapfile
set autoread

set noerrorbells
set novisualbell

set wildmenu
set wildmode=list:longest,full

set history=1000
set undolevels=1000
set backspace=indent,eol,start

set cursorline
hi clear CursorLine
hi CursorLine cterm=underline

set noexpandtab
set tabstop=8
set shiftwidth=8
