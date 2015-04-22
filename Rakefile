task :install do
	sh "ruby -e \"$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)\""
	sh "brew bundle"
end

task :update do
	sh "git pull origin master"
	sh "git submodule init && git submodule update"
end

task :symlink do
	sh "ln -s ~/dotfiles/.vim/ ~/"
	sh "ln -s ~/dotfiles/fish/config.fish ~/.config/fish/"
	sh "ln -s ~/dotfiles/.gitconfig ~/"
	sh "ln -s ~/dotfiles/.tmux.conf ~/"
end

