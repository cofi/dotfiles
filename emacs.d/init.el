(load (setq custom-file "~/.emacs.d/custom.el"))
(defun cofi/fortune2scratch ()
  "Return a comment-padded fortune cookie."
  (let ((cookie (shell-command-to-string "fortune -a")))
    (concat
     (replace-regexp-in-string " *$" ""
        (replace-regexp-in-string "^" ";; " cookie))
     "\n")))
(setq initial-scratch-message (cofi/fortune2scratch))

(defun add-to-loadpath (&rest dirs)
  (dolist (dir dirs load-path)
    (add-to-list 'load-path (expand-file-name dir) nil #'string=)))

(add-to-loadpath "~/.elisp"
                 "~/.emacs.d/config"
                 "~/.elisp/vendor/sackspace"
                 "~/.elisp/vendor/keychain-environment"
                 "~/.elisp/vendor/pylookup")

(require 'cofi-util)
(require-and-exec 'package
  (pour-lists package-archives '(("marmalade" . "http://marmalade-repo.org/packages/")))
  (package-initialize)
  (dolist (package '(
                     all
                     ace-jump-mode
                     auctex
                     auto-complete
                     auto-dictionary
                     bookmark+
                     boxquote
                     c-eldoc
                     cdlatex
                     dedicated
                     diminish
                     eldoc-eval
                     elein
                     epc
                     evil-numbers
                     flymake-cursor
                     full-ack
                     gist
                     gitconfig-mode
                     git-commit-mode
                     gitignore-mode
                     highlight-parentheses
                     highlight-80+
                     htmlize
                     keyfreq
                     keywiz
                     lacarte
                     markdown-mode
                     multi-term
                     offlineimap
                     paredit
                     pp-c-l
                     rainbow-mode
                     scratch
                     sml-modeline
                     twittering-mode
                     undo-tree
                     workgroups
                     yaml-mode
                     zencoding-mode
                     ))
    (unless (package-installed-p package)
      (package-install package))))

(defvar cofi/hostname (car (split-string system-name "\\." t)))
(defvar on-mobile? (find cofi/hostname '("hitchhiker") :test #'string=))

(load "private" 'noerror)
(require 'cofi-evil)

(defvar cofi/standard-settings '(cofi-color
                                 cofi-autoloads
                                 cofi-helm
                                 cofi-buffer
                                 cofi-calendar
                                 cofi-desktop
                                 cofi-ediff
                                 cofi-elisp
                                 cofi-erc
                                 cofi-files
                                 cofi-func
                                 cofi-keys
                                 cofi-mail
                                 cofi-project
                                 cofi-snippets
                                 cofi-ui
                                 cofi-org
                                 cofi-vcs
                                 cofi-shell
                                 cofi-smartparens
                                 cofi-workgroups
                                 cofi-write
                                 cofi-completion
                                 cofi-modeline))
(defvar cofi/full-settings '(cofi-programming
                             ))

(defvar cofi/full-emacs t "Load all settings not just minimal.")
(defvar cofi/mail-instance nil "This is an email instance.")

(mapc #'require cofi/standard-settings)

(defun cofi/dont-mess-with-history ()
  (setq kill-emacs-hook nil
        cofi/before-kill-hook nil))

(add-to-list 'command-switch-alist
             '("gnus" . (lambda (&rest ignore)
                        (setq cofi/mail-instance t)
                        (setq cofi/full-emacs nil)
                        (add-hook 'emacs-startup-hook 'gnus 'append)
                        (run-with-timer 10 600 'offlineimap)
                        ;; Exit Emacs after quitting gnus
                        (add-hook 'gnus-after-exiting-gnus-hook 'save-buffers-kill-emacs)
                        (add-hook 'emacs-startup-hook #'cofi/dont-mess-with-history 'append))))

(add-to-list 'command-switch-alist
             '("test" . (lambda (&rest ignore)
                          (add-hook 'emacs-startup-hook #'cofi/dont-mess-with-history 'append))))

(add-hook 'emacs-startup-hook (lambda ()
                                (on-mail-instance
                                  (global-linum-mode -1))
                                (on-full-instance
                                  (mapc #'require cofi/full-settings)
                                  (workgroups-mode 1))))
(cofi-next-file-assoc)
(add-hook 'emacs-startup-hook (lambda ()
                                (message "Time needed to load: %s seconds."
                                         (emacs-uptime "%s")))
          'append)

(add-hook 'after-save-hook 'byte-compile-config-on-save)
