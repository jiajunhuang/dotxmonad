call plug#begin('~/.config/nvim/plugged')

" basic plugins
Plug 'syntastic'
Plug 'The-NERD-Commenter'
Plug 'mileszs/ack.vim'
Plug 'klen/python-mode'
Plug 'bling/vim-airline'
Plug 'ervandew/supertab'
Plug 'tpope/vim-fugitive'
Plug 'luochen1990/rainbow'
Plug 'jiangmiao/auto-pairs'
Plug 'kshenoy/vim-signature'
Plug 'vim-scripts/matchit.zip'
Plug 'neovimhaskell/haskell-vim'
Plug 'terryma/vim-multiple-cursors'
Plug 'bronson/vim-trailing-whitespace'

call plug#end()

" basic settings
"syntax
syntax on
syntax enable
" display
set ignorecase
set number
set ruler
set cursorline cursorcolumn
"the-80-line is red!
set cc=80
"display pairs
set showmatch
"history
set history=1000
"indent
set expandtab
set shiftwidth=4
set tabstop=4
set softtabstop=4
set cindent
set shiftround
"Tab
set smarttab
"keywords
set iskeyword+=_,$,@,%,#,-
"
"plug-in settings

" Rainbow
let g:rainbow_active = 1

" Paste
nnoremap <F2> :set invpaste paste?<CR>
imap <F2> <C-O>:set invpaste paste?<CR>
set pastetoggle=<F2>

" vim undodir
set undofile
set undodir=~/.cache/nvim/undodir

" emacs key bindings for vim insert mode
inoremap <C-d> <Del>
inoremap <C-a> <Home>
inoremap <C-e> <End>
inoremap <C-f> <Right>
inoremap <C-b> <Left>
inoremap <C-n> <Down>
inoremap <C-p> <Up>
inoremap <C-v> <PageDown>
inoremap <M-v> <PageUp>

" syntastic
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*
let g:syntastic_python_checkers=['flake8']
let g:syntastic_python_flake8_args='--ignore=E501,E225'

" color
set background=dark

" newly vimrc
set showcmd
set ruler
set showmode
" disable mouse
set mouse=
" enable .nvimrc in current directory
set exrc

" terminal
if exists(':tnoremap')
    tnoremap <Esc> <C-\><C-n>
endif

" pymode
let g:pymode_lint = 0 " shutdown pylint check, I use syntastic
let g:pymode_rope = 0 " shutdown rope_project

" use space to toggle fold
nnoremap <silent> <Space> @=(foldlevel('.')?'za':"\<Space>")<CR>
vnoremap <Space> zf
