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

bindkey -e

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
alias emacs="emacs -nw"
alias e='emacsclient -nw -a ""'
alias ekill='emacsclient -e "(kill-emacs)'

# Env
export PATH="$HOME/.nodenv/bin:$PATH"
eval "$(nodenv init -)"

export PATH="$HOME/.rbenv/bin:$PATH"
eval "$(rbenv init -)"

export PATH="$HOME/.pyenv/bin:$PATH"
eval "$(pyenv init -)"

# load zshrc
[ -f ~/.zshrc.local ] && source ~/.zshrc.local
if [ -n "$TMUX" ]; then
     alias pbcopy="reattach-to-user-namespace pbcopy"
fi

export PATH="/usr/local/sbin:$PATH"

export HOMEBREW_GITHUB_API_TOKEN=2a94d1a1cd5efabd02d0a9a12559e1d002714311
export DICPATH=$HOME/.emacs.d/dict
export DICTIONARY="en_US"

export PATH=$PATH:/usr/bin
export PATH=$PATH:/usr/local/mysql/bin
export PATH=$PATH:/usr/local/opt/go/libexec/bin
export PATH=$PATH:/usr/local/bin
export PATH=$PATH:~/bin
export PATH=$PATH:$GOPATH/bin
export PATH=$PATH:$HOME/.rbenv/bin
export PATH=$PATH:$HOME/.local/bin
export PATH=$PATH:$HOME/.cask/bin
export PATH=$PATH:$HOME/.cargo/bin
export PATH=$PATH:/Library/Frameworks/Mono.framework/Versions/Current/Commands

export PGDATA=/usr/local/var/postgres

zle -N peco-src
function peco-src () {
  local selected_dir=$(ghq list -p | peco --query "$LBUFFER")
  if [ -n "$selected_dir" ]; then
    BUFFER="cd ${selected_dir}"
    zle accept-line
  fi
  zle clear-screen
}
bindkey '^\' peco-src

eval "$(direnv hook zsh)"

export LANG=ja_JP.UTF-8

# terminal color
export CLICOLOR=1

eval "$(nodenv init -)"


# prompt
export GIT_PS1_SHOWCOLORHINTS=1
git_prompt_sh=/usr/local/etc/bash_completion.d/git-prompt.sh
if [ -e $git_prompt_sh ]; then
    source $git_prompt_sh
    precmd () { __git_ps1 "%F{cyan}%c%f" " $ " " (%s)" }
else
    PS1='%F{cyan}%c%f \$ '
fi

[[ -z "$TMUX" && ! -z "$PS1" ]] && tmux && tmux source ~/.tmux.conf

export LANG=ja_JP.UTF-8

source ~/dotfiles/antigen.zsh
antigen bundle zsh-users/zsh-syntax-highlighting
antigen bundle git
antigen bundle heroku
antigen bundle pip
antigen bundle lein
antigen bundle command-not-found
antigen bundle zsh-z

antigen apply
