#!/bin/sh

# homebrew がインストールされているかチェック
# インストールされていない場合はインストールする
ISINSTALLED=`which brew`

if [ -f $ISINSTALLED ]; then
   echo 'brew is installed'
else
   echo 'brew is not installed'
   /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
fi

# ほしいパッケージをいろいろインストールする
for package in "git" "emacs" "flyway" hub go mysql nvm python python3 tmux zsh go heroku ctags reattach-to-user-namespace the_silver_searcher git-flow vim redis sbt tig tree wget typesafe-activator scala ruby-build rbenv sqlite trash pgcli phantomjs jq openssl ansible bash-completion fish

do
    _installed=`which $package`

    if [ -f "$_installed" -a -n "$_installed" ]; then
        echo "$_installed is exist"
        continue
    else
        echo "$_installed is not installed"
    fi

    brew install $package
done
