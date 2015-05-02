syntax enable
set encoding=utf-8

" Note: Skip initialization for vim-tiny or vim-small.
if !1 | finish | endif

if has('vim_starting')
    if &compatible
        set nocompatible               " Be iMproved
    endif

    " Required:
    set runtimepath+=~/.vim/bundles/neobundle.vim/
endif

" Required:
call neobundle#begin(expand('~/.vim/bundle/'))

" Let NeoBundle manage NeoBundle
" Required:
NeoBundleFetch 'Shougo/neobundle.vim'

" My Bundles here:
" Refer to |:NeoBundle-examples|.
" Note: You don't set neobundle setting in .gvimrc!
call neobundle#end()

" Required:
filetype plugin indent on

" If there are uninstalled bundles found on startup,
" this will conveniently prompt you to install them.
NeoBundleCheck

if filereadable(expand('~/.vim/.vimrc.bundles'))
    source ~/.vim/.vimrc.bundles
endif

if filereadable(expand('~/.vim/.vimrc.local'))
    source ~/.vim/.vimrc.local
endif

if filereadable(expand('~/.vim/.vimrc.keymap'))
    source ~/.vim/.vimrc.keymap
endif


if filereadable(expand('~/.vim/.vimrc.ctrlp'))
    source ~/.vim/.vimrc.ctrlp
endif

if filereadable(expand('~/.vim/.vimrc.python'))
    source ~/.vim/.vimrc.python
endif


if filereadable(expand('~/.vim/.vimrc.snippet'))
    source ~/.vim/.vimrc.snippet
endif




if has('gui_running')
    set background=light
    colorscheme monokai
else
    set background=dark

    colorscheme solarized
endif
