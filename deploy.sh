for file in `ls`; do
    case $file in
        "vimenv_setup")
            ln --symbolic --force -T `pwd`/$file ~/.vimrc
            ;;
        *)
            ln --symbolic --force -T `pwd`/$file ~/.$file
            ;;
        esac
done
