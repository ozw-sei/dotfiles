#/bin/sh

for item in `ls -al | awk '{print $9}' | grep '^\...' | grep -v '.git$' | grep -v '\#' | grep -v '\.swp$'`
do
    if [ -L "$HOME/$item" ]; then
        mv "${HOME}/${item}" "${HOME}/${item}.org"
    fi

    ln -s "`pwd`/$item" "$HOME"
    echo "set ${item}"
done



