#/bin/sh

for item in ".zshrc" ".agignore" ".zshenv" ".gitconfig" ".tmux.conf" ".tmuxinator" ".vim"
do
    if [ -L "$HOME/$item" ]; then
        echo "$item is exist"
      :
    else
        ln -s "`pwd`/$item" "$HOME"
        echo 'set $item'
  fi
done
