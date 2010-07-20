#! /bin/sh
setxkbmap -model evdev -layout us -option compose:ralt,caps:escape
xrdb -merge .Xresources

urxvtd -q -f -o & 

[ -e ~/bin_local/xinit] && ~/.bin_local/xinit

export GTK_IM_MODULE=xim
export QT_IM_MODULE=xim
export GDK_NATIVE_WINDOWS=true

exec startkde
