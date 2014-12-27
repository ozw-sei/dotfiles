# Path to your oh-my-fish.
set fish_path $HOME/.oh-my-fish

# Theme
set fish_theme robbyrussell

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-fish/plugins/*)
# Custom plugins may be added to ~/.oh-my-fish/custom/plugins/
# Example format: set fish_plugins autojump bundler

# Path to your custom folder (default path is $FISH/custom)
#set fish_custom $HOME/dotfiles/oh-my-fish

# Load oh-my-fish configuration.
. $fish_path/oh-my-fish.fish
set fish_greeting

set DEVELOP $HOME/Dropbox/Develop
set PROJECT $DEVELOP/Project
set SANDBOX $DEVELOP/Sandbox
set ANDROID_HOME $DEVELOP/Lib/android-sdk/22.2.1/
set ANDROID_NDK_HOME $DEVELOP/Lib/android-ndk/android-ndk/9/
set VIRTUALENV_USE_DITRIBUTE true
set HOMEBREW_CASK_OPTS "--appdir=/Applications"
set ANYENV_ROOT $HOME/.anyenv
set NODEBREW_ROOT $HOME/.nodebrew
set PYTHON_PATH /usr/local/Cellar/python/2.7.6/bin
set PGDATA /usr/local/var/postgres
set LANG en_US.UTF-8
set LC_ALL en_US.UTF-8
set _JAVA_OPTIONS "-Dfile.encoding=UTF-8"
set -x SECRET_KEY_BASE "9f53bb1a4a533c22f12b15c0e9bfa15a960cc2eb4226f55cadaeee9efcd04f1f5919e7e6ab75db04c2201255add79de054e6d31551bb93e5aaf364b0d2497ce0"
set -x IDEA_HOME "/Applications/IntelliJ\ IDEA\ 14.app"

set PATH /bin
set -x PATH $PATH /usr/local/bin
set -x PATH $PATH /usr/local/sbin
set -x PATH $PATH $HOME/.gem/ruby/*/bin
set -x PATH $PATH /var/lib/gems/*/bin

set -x PATH $PATH /usr/bin
set -x PATH $PATH /sbin
set -x PATH $PATH /usr/sbin


#######################
##  paths and aliases  #
########################
#
if begin; status --is-login; and test -z $TMUX; end
    set -x EDITOR vim
	set -x VISUAL vim

	set -x CDPATH . "$HOME" $CDPATH
end

if test -e ~/.config/fish/config_thismachine.fish
	source ~/.config/fish/config_thismachine.fish
end




##########
##  misc  #
###########

set __fish_git_prompt_showuntrackedfiles 'yes'
set __fish_git_prompt_showdirtystate 'yes'
set __fish_git_prompt_color_branch magenta
set __fish_git_prompt_char_dirtystate '!'
set __fish_git_prompt_char_untrackedfiles '?'
set __fish_git_prompt_char_stagedstate '→'
set __fish_git_prompt_char_stashstate '↩'
set __fish_git_prompt_char_upstream_ahead '↑'
set __fish_git_prompt_char_upstream_behind '↓'

function j
    cd (command autojump $argv)
end

if type -f dircolors >/dev/null
	# eval (dircolors -c ~/.dir_colors.light)
	eval (dircolors -c ~/.dir_colors.dark)
end

set -x GREP_COLOR '37;45'

alias reload "source ~/.config/fish/config.fish"

set -x PATH $HOME/.rbenv/bin $PATH
set -x PATH $HOME/.rbenv/shims $PATH
rbenv rehash >/dev/null ^&1

