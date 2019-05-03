filetype plugin on

set omnifunc=syntaxcomplete#Complete
set ignorecase
set completeopt+=noinsert,menuone
set shortmess+=c  
set laststatus=2
set shiftwidth=2
set tabstop=2
set number
set softtabstop=2
set foldmethod=syntax
let javaScript_fold=1  
set foldlevel=20


let $FZF_DEFAULT_COMMAND='rg --files --hidden --smart-case --glob "!.git/*"'
let g:ale_fixers = {'typescript': ['tslint', 'prettier'], 'haskell': ['brittany']}
let g:ale_linters = {'haskell': ['ghc']}
let g:ale_lint_on_save = 1
let g:ale_javascript_prettier_use_local_config = 1

nnoremap <F12> :YcmCompleter GoToDeclaration<CR>
nnoremap <C-p> :Files<CR>
nnoremap <F2>  :YcmCompleter RefactorRename 
nnoremap <F8>  :lopen<CR> :lnext<CR>
nnoremap <F9>  :YcmCompleter FixIt<CR>
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>
tnoremap <Esc> <C-\><C-n>


call plug#begin('~/.vim/plugged')

Plug 'ajh17/Spacegray.vim'
Plug 'lifepillar/vim-mucomplete'
Plug 'vim-airline/vim-airline'
Plug 'HerringtonDarkholme/yats.vim'
Plug 'maxmellon/vim-jsx-pretty'
Plug 'Valloric/YouCompleteMe', { 'do': './install.py --ts-completer' }
Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
Plug 'rdnetto/YCM-Generator', { 'branch': 'stable' }
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'tpope/vim-fugitive'
Plug 'styled-components/vim-styled-components', { 'branch': 'main' }
Plug 'w0rp/ale'
Plug 'tpope/vim-sleuth'
Plug 'junegunn/fzf.vim'
Plug 'yonchu/accelerated-smooth-scroll'
Plug 'mhinz/vim-signify'
Plug 'terryma/vim-multiple-cursors'

" Initialize plugin system
call plug#end()

colorscheme spacegray

