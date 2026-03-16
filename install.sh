#!/bin/bash
set -e

echo "=== dotfiles install ==="

# --- Homebrew ---
if ! command -v brew &>/dev/null; then
    echo "Installing Homebrew..."
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

    # Activate brew for this session
    if [[ "$(uname -m)" == "arm64" ]]; then
        eval "$(/opt/homebrew/bin/brew shellenv)"
    elif [[ -d /home/linuxbrew/.linuxbrew ]]; then
        eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"
    fi
fi

echo "Homebrew: $(brew --version | head -1)"

# --- Brew packages ---
packages=(
    fzf
    ghq
    direnv
    gh
    tmux
    tig
    vim
    peco
    jq
)

# Linux (WSL2) extras
if [[ "$(uname)" == "Linux" ]]; then
    packages+=(xsel socat)
fi

echo ""
echo "Installing packages..."
for pkg in "${packages[@]}"; do
    if brew list "$pkg" &>/dev/null; then
        echo "  $pkg (installed)"
    else
        echo "  $pkg (installing...)"
        brew install "$pkg"
    fi
done

# --- Claude Code ---
if ! command -v claude &>/dev/null; then
    echo ""
    echo "Installing Claude Code..."
    curl -fsSL https://claude.ai/install.sh | bash
fi

# fzf key bindings and completion
if [ ! -f ~/.fzf.zsh ]; then
    echo ""
    echo "Setting up fzf key bindings..."
    "$(brew --prefix)/opt/fzf/install" --key-bindings --completion --no-update-rc --no-bash --no-fish
fi

# --- Symlinks ---
echo ""
DOTFILES_DIR="$(cd "$(dirname "$0")" && pwd)"
echo "Setting up symlinks..."
bash "$DOTFILES_DIR/set_symlink.sh"

echo ""
echo "=== Done! ==="
echo "Restart your shell or run: source ~/.zshrc"
