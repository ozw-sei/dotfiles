export LANG=ja_JP.UTF-8
export CLICOLOR=1
export GIT_PS1_SHOWCOLORHINTS=1
export PIPENV_VENV_IN_PROJECT=1
export GOPATH=$HOME/go
export TERM=xterm-256color

# Homebrew (platform-aware)
if [[ "$(uname -m)" == "arm64" ]]; then
    eval "$(/opt/homebrew/bin/brew shellenv)"
elif [[ -d /home/linuxbrew/.linuxbrew ]]; then
    eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"
fi

# PATH
export PATH="$HOME/.local/bin:$PATH"
export PATH="$HOME/.cargo/bin:$PATH"
export PATH="$GOPATH/bin:$PATH"
export PATH="$HOME/bin:$PATH"

# Load local overrides
[ -f ~/.zshenv.local ] && source ~/.zshenv.local
