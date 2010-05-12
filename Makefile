.PHONY: all emacs-compile elisp-compile deploy

PWD := `pwd`
LINK_CMD := ln --symbolic --force -T
NORMAL_FILES := `ls -I newsbeuterconfig -I vimenv_setup -I Makefile`

refresh:
	 emacs --batch --no-site-file --eval '(byte-recompile-directory "emacs.d/")'
	 emacs --batch --no-site-file --eval '(byte-recompile-directory "elisp/")'

all: compile deploy

compile: 
	 emacs --batch --no-site-file --eval '(byte-recompile-directory "emacs.d/" 0 t)'
	 emacs --batch --no-site-file --eval '(byte-recompile-directory "elisp/" 0 t)'

deploy:
	for file in $(NORMAL_FILES); do $(LINK_CMD) $(PWD)/$$file ~/.$$file; done
	$(LINK_CMD) $(PWD)/vimenv_setup ~/.vimrc
	$(LINK_CMD) $(PWD)/newsbeuterconfig ~/.newsbeuter/config
