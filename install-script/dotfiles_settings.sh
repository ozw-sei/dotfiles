git submodule init
git submodule update
cd zsh/maximum-awesome
rake

mv ~/.zshrc ~/.zshrc.org
ln -s ~/dotfiles/zsh/zshrc ~/
mv ~/zshrc ~/.zshrc

mv ~/.zshenv ~/.zshenv.org
ln -s ~/dotfiles/zsh/zshenv ~/
mv ~/zshenv ~/.zshenv

mv ~/.gitconfig ~/.gitconfig.org
ln -s ~/dotfiles/gitconfig
mv ~/gitconfig ~/.gitconfig

mv ~/.tmux.conf.org
ln -s ~/dotfiles/tmux.conf
mv ~/tmux.conf ~/.tmux.conf

