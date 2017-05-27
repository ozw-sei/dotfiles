export GTK_IM_MODULE=ibus
export QT_IM_MODULE=ibus
export XMODIFIERS=@im=ibus
setxkbmap -layout jp
albert & 
exec startxfce4

[[ -f ~/.Xresources ]] && xrdb -merge -I$HOME ~/.Xresources

