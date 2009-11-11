keychain --quiet ~/.ssh/id_rsa

setxkbmap -model evdev -layout us -option compose:menu,caps:escape
xmodmap ~/.Xmodmap
xrdb -merge .Xresources

urxvtcd -pe tabbed -e screen -R
