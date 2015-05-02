task :install do
  sh "ruby -e \"$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)\""
  sh "brew bundle"
  sh "sudo brew bundle"
end

task :update do
    sh "git pull origin master"
    sh "git submodule init && git submodule update"
end

task :symlink do
  until File.exist?("~/.vim") do
      sh "ln -s ~/dotfiles/.vim ~/"
  end
  until File.exist?("~/.zshrc") do
      sh "ln -s ~/dotfiles/zsh/.zshrc ~/"
  end
  until File.exist?("~/.zshenv") do
      sh "ln -s ~/dotfiles/zsh/.zshenv ~/"
  end
  until File.exist?("~/.gitconfig") do
      sh "ln -s ~/dotfiles/.gitconfig ~/"
  end
  until File.exist?("~/.tmux.conf") do
      sh "ln -s ~/dotfiles/.tmux.conf ~/"
  end
  until File.exist?("~/.vimrc") do
      sh "ln -s ~/dotfiles/.vim/.vimrc ~/"
  end
end
