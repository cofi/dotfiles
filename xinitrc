keychain --quiet ~/.ssh/id_rsa

setxkbmap -model evdev -layout us -option compose:ralt,caps:escape
xmodmap ~/.Xmodmap
xrdb -merge .Xresources

urxvtcd -e screen -R
