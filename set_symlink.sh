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

if [ -L "/usr/local/tools" ];then
    echo '/usr/local/toolsが既に存在します'
else
    ln -s ~/dotfiles/tools /usr/local
fi


