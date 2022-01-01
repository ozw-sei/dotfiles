#/bin/sh

for item in `ls -al | awk '{print $9}' | grep '^\...' | grep -v '.git$' | grep -v '\#' | grep -v '\.swp$' | grep -v '^.env'`
do
    if [ -L "$HOME/$item" ]; then
        mv "${HOME}/${item}" "${HOME}/${item}.org"
    fi

    ln -s "`pwd`/$item" "$HOME"
    echo "set ${item}"
done

ln -sf $HOME/dotfiles/powerline.conf $HOME
