#! /bin/zsh -f
# Rename file, but keep suffix

OLD="$1"
NEW="$2"

SUFFIX=${OLD#*.}

DIR=$(dirname $OLD)

mv ${OLD} $DIR/${NEW}.${SUFFIX}
