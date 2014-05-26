if has('vim_starting')
  set nocompatible               " Be iMproved
  " Required:
  set runtimepath+=~/.vim/bundle/neobundle.vim/
endif


" Required:
call neobundle#begin(expand('~/.vim/bundle/'))

" install NeoBundle bundles
if filereadable(expand("~/.vimrc.bundles"))
  source ~/.vimrc.bundles
  source ~/.vimrc.bundles.local
  source ~/.vimrc.local
endif

call neobundle#end()

" Required:
filetype plugin indent on

" If there are uninstalled bundles found on startup,
" this will conveniently prompt you to install them.
NeoBundleCheck


if filereadable(expand("~/.vimrc.keymap"))
  source ~/.vimrc.keymap
endif

set nocursorline " don't highlight current line

"#####表示設定#####
set title "編集中のファイル名を表示
syntax on "コードの色分け
set tabstop=4 "インデントをスペース4つ分に設定
set smartindent autoindent "オートインデント
set backspace=indent,eol,start
set wildmode=longest,list,full

"""#####検索設定#####
set ignorecase "大文字/小文字の区別なく検索する
set smartcase "検索文字列に大文字が含まれている場合は区別して検索する
set wrapscan "検索時に最後まで行ったら最初に戻る

"--------------------
"""" 基本的な設定
"--------------------
"""新しい行のインデントを現在行と同じにする
set autoindent

" TABでなく空白文字を利用
set expandtab

""変更中のファイルでも、保存しないで他のファイルを表示する
set hidden

"インクリメンタルサーチを行う
"set incsearch
""
""行番号を表示する
set number

"閉括弧が入力された時、対応する括弧を強調する
"set showmatch
""
""新しい行を作った時に高度な自動インデントを行う
set smarttab

set nobackup
set undofile
set undodir=$HOME/.vim/.vimundo

set wildmenu                            " コマンド補完を強化
set wildmode=list:full                  " リスト表示，最長マッチ

set showcmd                             " 入力中のコマンドを表示
set showmode
set cursorline                          " カーソル行に下線
set completeopt-=preview                " 自動プレビューを無効(主にPython用)
set ruler

" 不可視文字
set list                                " 不可視文字を表示
set listchars=tab:>\                    " 不可視文字の表示方法
hi ZenkakuSpace guibg=DarkBlue gui=underline ctermfg=LightBlue
match ZenkakuSpace /　/                 " 全角文字

set laststatus=2                        " ステータスラインを2行に


" Show full path  ----------------------
augroup EchoFilePath
  autocmd WinEnter * execute "normal! 1\<C-g>"
  augroup END


  " Charset, Line ending -----------------
  set termencoding=utf-8
  set encoding=utf-8
  set fileencodings=utf-8,iso-2022-jp,euc-jp,cp932
  set ffs=unix,dos,mac
  if exists('&ambiwidth')
    set ambiwidth=double
    endif

    " 保存時に空白削除
    autocmd BufWritePre * :%s/\s\+$//e

    " 保存時にtabをスペースに変換する
    autocmd BufWritePre * :%s/\t/  /ge

    " Enable basic mouse behavior such as resizing buffers.
    set mouse=a
    if exists('$TMUX')  " Support resizing in tmux
      set ttymouse=xterm2
        endif

" keyboard shortcuts
inoremap jj <ESC>

" highlight search
"set hlsearch
"nmap <leader>hl :let @/ = ""<CR>

" gui settings
if (&t_Co == 256 || has('gui_running'))
  if ($TERM_PROGRAM == 'iTerm.app')
    colorscheme solarized
  else
    colorscheme desert
  endif
endif

" Disambiguate ,a & ,t from the Align plugin, making them fast again.
"
" This section is here to prevent AlignMaps from adding a bunch of mappings
" that interfere with the very-common ,a and ,t mappings. This will get run
" at every startup to remove the AlignMaps for the *next* vim startup.
"
" If you do want the AlignMaps mappings, remove this section, remove
" ~/.vim/bundle/Align, and re-run rake in maximum-awesome.
function! s:RemoveConflictingAlignMaps()
  if exists("g:loaded_AlignMapsPlugin")
    AlignMapsClean
  endif
endfunction
command! -nargs=0 RemoveConflictingAlignMaps call s:RemoveConflictingAlignMaps()
silent! autocmd VimEnter * RemoveConflictingAlignMaps

let g:syntastic_python_checkers=['flake8', 'pep8']
let g:syntastic_python_checker_args='--ignore=E501,E225'
