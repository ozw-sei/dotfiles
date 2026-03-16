#!/bin/sh
set -e

DOTFILES_DIR="$(cd "$(dirname "$0")" && pwd)"

# Files/dirs to symlink to $HOME
targets=(
    .zshrc
    .zshenv
    .gitconfig
    .editorconfig
    .tmux.conf
    .tmux
    .tigrc
    .ignore
    .peco
)

for item in "${targets[@]}"; do
    src="$DOTFILES_DIR/$item"
    dst="$HOME/$item"

    if [ -e "$dst" ] && [ ! -L "$dst" ]; then
        echo "Backing up $dst -> ${dst}.bak"
        mv "$dst" "${dst}.bak"
    fi

    ln -sfn "$src" "$dst"
    echo "Linked $item"
done

echo "Done!"
