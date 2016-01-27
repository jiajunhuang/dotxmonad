call plug#begin('~/.config/nvim/plugged')

" basic plugins
Plug 'kien/ctrlp.vim'
Plug 'mattn/emmet-vim'
Plug 'tpope/vim-repeat'
Plug 'bling/vim-airline'
Plug 'ervandew/supertab'
Plug 'mitsuhiko/vim-rst'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-fugitive'
Plug 'sickill/vim-monokai'
Plug 'scrooloose/nerdtree'
Plug 'luochen1990/rainbow'
Plug 'jiangmiao/auto-pairs'
Plug 'tshirtman/vim-cython'
Plug 'scrooloose/syntastic'
Plug 'kshenoy/vim-signature'
Plug 'wlangstroth/vim-racket'
Plug 'pangloss/vim-javascript'
Plug 'vim-scripts/matchit.zip'
Plug 'scrooloose/nerdcommenter'
Plug 'neovimhaskell/haskell-vim'
Plug 'gorodinskiy/vim-coloresque'
Plug 'hynek/vim-python-pep8-indent'
Plug 'ntpeters/vim-better-whitespace'
Plug '~/Code/scheme/vimplug/'

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
set wrap
set textwidth=0
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
colorscheme monokai

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

" import pdb
nnoremap <Leader>b Oimport pdb; pdb.set_trace()  # TODO remove it<Esc>:w<ENTER>

" open terminal
nnoremap <C-s> :vs term://bash<CR>A

" repeat.vim
silent! call repeat#set("\<Plug>MyWonderfulMap", v:count)

" autoremove whitespace(not only show them)
autocmd BufWritePre * StripWhitespace

" NERDTree
nnoremap <F3> :NERDTreeToggle<CR>

" scheme
aug Scheme
  au!
  au Filetype scheme setl cindent& lispwords=define
aug END
