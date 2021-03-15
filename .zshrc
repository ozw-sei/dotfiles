setopt AUTO_CD
setopt AUTO_PUSHD
setopt correct
setopt list_packed
setopt nolistbeep
setopt EXTENDED_HISTORY
setopt HIST_EXPAND
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_IGNORE_SPACE
setopt transient_rprompt
source   ~/.zshenv

bindkey -e

autoload -Uz compinit
compinit

typeset -g ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE='20'

autoload history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end
bindkey "^P" history-beginning-search-backward-end
bindkey "^N" history-beginning-search-forward-end

HISTFILE=~/.zsh_history
HISTSIZE=1000000
SAVEHIST=1000000

autoload -U compinit
compinit

## history
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end
zstyle ':completion:*' completer _expand _complete _ignored _correct _approximate
zstyle ':completion:*' max-errors 50
zstyle ':completion:*' list-colors ''
zstyle :compinstall filename "$HOME/.zshrc"

alias ll="ls -l"
alias la="ls -a"
alias h="hub"
alias gi="git init"
alias gd="git diff"
alias gss="git status --short"
alias gg="git grep"
alias t="git grep"
alias reload="source ~/.zshrc"
alias zshrc="vim ~/.zshrc"
alias dotfiles="cd ~/dotfiles"
alias e='emacsclient -nw -a ""'
alias ekill='emacsclient -e "(kill-emacs)'

# Env
eval "$(nodenv init -)"
eval "$(rbenv init -)"
eval "$(pyenv init -)"

# load zshrc
[ -f ~/.zshrc.local ] && source ~/.zshrc.local
if [ -n "$TMUX" ]; then
     alias pbcopy="reattach-to-user-namespace pbcopy"
fi

function ghq-fzf() {
local src=$(ghq list | fzf --preview "bat --color=always --style=header,grid --line-range :80 $(ghq root)/{}/README.*")
if [ -n "$src" ]; then
	BUFFER="cd $(ghq root)/$src"
	zle accept-line
fi
zle -R -c
}
zle -N ghq-fzf
bindkey '^;' ghq-fzf

function gh-fzf() {
local src=$(curl 'https://api.github.com/users/ozw-sei/repos?per_page=1000&page=1' | jq --stream -r 'select(.[0][1] == "full_name") | .[1]' | fzf)
if [ -n "$src" ]; then
	ghq get github.com/$src
fi
}
zle -N gh-fzf
bindkey "^'" gh-fzf

eval "$(direnv hook zsh)"


# terminal color


eval "$(nodenv init -)"


# prompt

git_prompt_sh=/usr/local/etc/bash_completion.d/git-prompt.sh
if [ -e $git_prompt_sh ]; then
    source $git_prompt_sh
    precmd () { __git_ps1 "%F{cyan}%c%f" " $ " " (%s)" }
else
    PS1='%F{cyan}%c%f \$ '
fi

[[ -z "$TMUX" && ! -z "$PS1" ]] && tmux source ~/.tmux.conf

## https://qiita.com/nishina555/items/f4f1ddc6ed7b0b296825
# ここはプロンプトの設定なので今回の設定とは関係ありません
if [ $UID -eq 0 ];then
# ルートユーザーの場合
PROMPT="%F{red}%n:%f%F{green}%d%f [%m] %%
"
else
# ルートユーザー以外の場合
PROMPT="%F{cyan}%n:%f%F{green}%d%f [%m] %%
"
fi

# ブランチ名を色付きで表示させるメソッド
function rprompt-git-current-branch {
  local branch_name st branch_status

  if [ ! -e  ".git" ]; then
    # gitで管理されていないディレクトリは何も返さない
    return
  fi
  branch_name=`git rev-parse --abbrev-ref HEAD 2> /dev/null`
  st=`git status 2> /dev/null`
  if [[ -n `echo "$st" | grep "^nothing to"` ]]; then
    # 全てcommitされてクリーンな状態
    branch_status="%F{green}"
  elif [[ -n `echo "$st" | grep "^Untracked files"` ]]; then
    # gitに管理されていないファイルがある状態
    branch_status="%F{red}?"
  elif [[ -n `echo "$st" | grep "^Changes not staged for commit"` ]]; then
    # git addされていないファイルがある状態
    branch_status="%F{red}+"
  elif [[ -n `echo "$st" | grep "^Changes to be committed"` ]]; then
    # git commitされていないファイルがある状態
    branch_status="%F{yellow}!"
  elif [[ -n `echo "$st" | grep "^rebase in progress"` ]]; then
    # コンフリクトが起こった状態
    echo "%F{red}!(no branch)"
    return
  else
    # 上記以外の状態の場合は青色で表示させる
    branch_status="%F{blue}"
  fi
  # ブランチ名を色付きで表示する
  echo "${branch_status}[$branch_name]"
}

# プロンプトが表示されるたびにプロンプト文字列を評価、置換する
setopt prompt_subst

# プロンプトの右側(RPROMPT)にメソッドの結果を表示させる
RPROMPT='`rprompt-git-current-branch`'

source ~/dotfiles/antigen.zsh
antigen init $HOME/.antigenrc

antigen apply

# Added by serverless binary installer
export PATH="$HOME/.serverless/bin:$PATH"
alias pbcopy='xsel --clipboard --input'

fpath=(~/dotfiles/completion $fpath)

autoload -U compinit
compinit -u


# fbr - checkout git branch
fbr() {
  local branches branch
  branches=$(git branch -vv) &&
  branch=$(echo "$branches" | fzf +m) &&
  git checkout $(echo "$branch" | awk '{print $1}' | sed "s/.* //")
}

if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
    alias clear='vterm_printf "51;Evterm-clear-scrollback";tput clear'
fi

eval "$(gh completion -s zsh)"
