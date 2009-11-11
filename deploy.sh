for file in `ls`; do
    case $file in
        "vimenv_setup")
            echo "foo" $file
            ln --symbolic --force -T `pwd`/$file /tmp/.$file
            ;;
        *)
            echo "bar" $file
            ln --symbolic --force -T `pwd`/$file /tmp/.vimrc
            ;;
        esac
done
