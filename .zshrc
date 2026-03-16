# --- Options ---
setopt AUTO_CD
setopt AUTO_PUSHD
setopt correct
setopt list_packed
setopt nolistbeep
setopt prompt_subst

# --- History ---
setopt EXTENDED_HISTORY
setopt HIST_EXPAND
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_IGNORE_SPACE
setopt transient_rprompt

HISTFILE=~/.zsh_history
HISTSIZE=1000000
SAVEHIST=1000000

# --- Completion ---
autoload -Uz compinit
compinit -u

zstyle ':completion:*' completer _expand _complete _ignored _correct _approximate
zstyle ':completion:*' max-errors 50
zstyle ':completion:*' list-colors ''
zstyle :compinstall filename "$HOME/.zshrc"

# --- Key bindings ---
bindkey -e

autoload history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end
bindkey "^P" history-beginning-search-backward-end
bindkey "^N" history-beginning-search-forward-end

# --- Aliases ---
alias ll="ls -l"
alias la="ls -a"
alias gd="git diff"
alias gss="git status --short"
alias gg="git grep"
alias reload="source ~/.zshrc"
alias dotfiles="cd ~/dotfiles"

# clipboard (platform-aware)
if [[ "$OSTYPE" == "linux-gnu"* ]]; then
    alias pbcopy='xsel --clipboard --input'
    alias pbpaste='xsel --clipboard --output'
fi

# --- Tool init ---
source ~/.zshenv

# direnv
command -v direnv &>/dev/null && eval "$(direnv hook zsh)"

# fzf
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# gh
command -v gh &>/dev/null && eval "$(gh completion -s zsh)"

# Claude Code
command -v claude &>/dev/null && eval "$(claude completions zsh)"

# --- ghq + fzf ---
if command -v ghq &>/dev/null && command -v fzf &>/dev/null; then
    function ghq-fzf() {
        local src=$(ghq list | fzf --preview "ls -la $(ghq root)/{}")
        if [ -n "$src" ]; then
            BUFFER="cd $(ghq root)/$src"
            zle accept-line
        fi
        zle -R -c
    }
    zle -N ghq-fzf
    bindkey '^]' ghq-fzf

    # fbr - checkout git branch with fzf
    fbr() {
        local branches branch
        branches=$(git branch -vv) &&
        branch=$(echo "$branches" | fzf +m) &&
        git checkout $(echo "$branch" | awk '{print $1}' | sed "s/.* //")
    }
fi

# --- Prompt ---
function rprompt-git-current-branch {
    local branch_name st branch_status

    if [ ! -e ".git" ]; then
        return
    fi
    branch_name=$(git rev-parse --abbrev-ref HEAD 2>/dev/null)
    st=$(git status 2>/dev/null)
    if [[ -n $(echo "$st" | grep "^nothing to") ]]; then
        branch_status="%F{green}"
    elif [[ -n $(echo "$st" | grep "^Untracked files") ]]; then
        branch_status="%F{red}?"
    elif [[ -n $(echo "$st" | grep "^Changes not staged for commit") ]]; then
        branch_status="%F{red}+"
    elif [[ -n $(echo "$st" | grep "^Changes to be committed") ]]; then
        branch_status="%F{yellow}!"
    elif [[ -n $(echo "$st" | grep "^rebase in progress") ]]; then
        echo "%F{red}!(no branch)"
        return
    else
        branch_status="%F{blue}"
    fi
    echo "${branch_status}[$branch_name]"
}

PROMPT="%F{cyan}%n:%f%F{green}%~%f %%
"
RPROMPT='$(rprompt-git-current-branch)'

# --- WSL2 ---
if [[ "$(uname -r)" == *microsoft* ]]; then
    export SSH_AUTH_SOCK=$HOME/.ssh/agent.sock
    ALREADY_RUNNING=$(ps -auxww | grep -q "[n]piperelay.exe -ei -s //./pipe/openssh-ssh-agent"; echo $?)
    if [[ $ALREADY_RUNNING != "0" ]]; then
        if [[ -S $SSH_AUTH_SOCK ]]; then
            rm $SSH_AUTH_SOCK
        fi
        (setsid socat UNIX-LISTEN:$SSH_AUTH_SOCK,fork EXEC:"npiperelay.exe -ei -s //./pipe/openssh-ssh-agent",nofork &) >/dev/null 2>&1
    fi
fi

# --- tmux auto-start ---
[[ -z "$TMUX" && -n "$PS1" ]] && command -v tmux &>/dev/null && tmux

# Load local overrides
[ -f ~/.zshrc.local ] && source ~/.zshrc.local
