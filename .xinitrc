export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx
export XMODIFIERS=@im=fcitx
xsetroot -cursor_name left_ptr
setxkbmap -layout jp
albert & 
exec startxfce4

[[ -f ~/.Xresources ]] && xrdb -merge -I$HOME ~/.Xresources

