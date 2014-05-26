if has('vim_starting')
  set nocompatible               " Be iMproved

  " Required:
  set runtimepath+=~/.vim/bundle/neobundle.vim/
endif

" Required:
call neobundle#begin(expand('~/.vim/bundle/'))

" Let NeoBundle manage NeoBundle
" Required:
NeoBundleFetch 'Shougo/neobundle.vim'

" My Bundles here:
NeoBundle 'Shougo/neosnippet.vim'
NeoBundle 'Shougo/neosnippet-snippets'
NeoBundle 'tpope/vim-fugitive'
NeoBundle 'kien/ctrlp.vim'
NeoBundle 'flazz/vim-colorschemes'
NeoBundle 'Shougo/neocomplete.vim'
NeoBundle 'davidhalter/jedi-vim'
NeoBundle 'thinca/vim-quickrun'
NeoBundle 'Shougo/neocomplcache'
NeoBundle 'nathanaelkane/vim-indent-guides'
NeoBundle 'airblade/vim-gitgutter'
NeoBundle 'rking/ag.vim'

" You can specify revision/branch/tag.
NeoBundle 'Shougo/vimshell', { 'rev' : '3787e5' }

call neobundle#end()

" Required:
filetype plugin indent on

" If there are uninstalled bundles found on startup,
" this will conveniently prompt you to install them.
NeoBundleCheck



"#####表示設定#####
set title "編集中のファイル名を表示
syntax on "コードの色分け
set tabstop=4 "インデントをスペース4つ分に設定
set smartindent autoindent "オートインデント
set backspace=indent,eol,start
set wildmode=longest,list,full

""#####検索設定#####
set ignorecase "大文字/小文字の区別なく検索する
set smartcase "検索文字列に大文字が含まれている場合は区別して検索する
set wrapscan "検索時に最後まで行ったら最初に戻る

"--------------------
""" 基本的な設定
"--------------------
""新しい行のインデントを現在行と同じにする
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
set statusline=%<%F\ #%n%m%r%h%w%{'['.(&fenc!=''?&fenc:&enc).']['.&ff.']'}%y%=%l,%c%V%8P


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

" Keymapping
nnoremap j gj
nnoremap k gk
nnoremap gj j
nnoremap gk k
