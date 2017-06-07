export GTK_IM_MODULE='uim'
export QT_IM_MODULE='uim'
uim-xim &
export XMODIFIERS='@im=uim'

setxkbmap -layout jp
albert & 
exec startkde

[[ -f ~/.Xresources ]] && xrdb -merge -I$HOME ~/.Xresources

