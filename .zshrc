export ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="robbyrussell"

source $ZSH/oh-my-zsh.sh

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
alias reload="source ~/.zshrc;source ~/.zshenv"
alias zshrc="vim ~/.zshrc"
alias zshenv="vim ~/.zshenv"
alias dotfiles="cd ~/dotfiles"

# Env
export PATH="$HOME/.rbenv/bin:$PATH"
eval "$(rbenv init -)"


# load zshrc
[ -f ~/.zshrc.local ] && source ~/.zshrc.local
if [ -n "$TMUX" ]; then
     alias pbcopy="reattach-to-user-namespace pbcopy"
fi
[[ -z "$TMUX" && ! -z "$PS1" ]] && tmux
alias ohmyzsh="mate ~/.oh-my-zsh"

export PATH="/usr/local/sbin:$PATH"

function workspace {
  cd "$( ls -1d $HOME/work/* | peco )"
}

function agvim {
  vim $(ag $@ | peco --query  "$LBUFFER" | awk -F : '{print "-c " $2 " " $1}')
}

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"



[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
