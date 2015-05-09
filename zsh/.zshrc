## setopt
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
HISTSIZE=100000
SAVEHIST=100000

autoload -U compinit
compinit

PROMPT="%{${fg[yellow]}%}$%{${reset_color}%} "

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
alias rm="trash"
alias reload="source ~/.zshrc;source ~/.zshenv"

# Env
export PATH="$HOME/.rbenv/bin:$PATH"
eval "$(rbenv init -)"


# load zshrc
[ -f ~/.zshrc.local ] && source ~/.zshrc.local
[[ -z "$TMUX" && ! -z "$PS1" ]] && tmux

if [ -n "$TMUX" ]; then
    alias pbcopy="reattach-to-user-namespace pbcopy"
fi

source "~/dotfiles/zsh/plugins/zsh-vcs-prompt/zshrc.sh"
