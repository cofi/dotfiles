for file in `ls`; do
    case $file in
        "vimenv_setup")
            echo "foo" $file
            ln --symbolic --force -T `pwd`/$file ~/.$file
            ;;
        *)
            echo "bar" $file
            ln --symbolic --force -T `pwd`/$file ~/.vimrc
            ;;
        esac
done
