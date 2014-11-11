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
)

(require 'package)
(package-initialize)
(require 'cofi-util)
(pour-lists package-archives '(
                               ("marmalade" . "http://marmalade-repo.org/packages/")
                               ("melpa"     . "http://melpa.org/packages/")
                               ))
(dolist (package '(
                   all
                   ace-jump-mode
                   ack-and-a-half
                   auctex
                   auto-complete
                   ac-slime
                   auto-dictionary
                   bookmark+
                   boxquote
                   cljdoc
                   clojure-mode
                   clojure-test-mode
                   c-eldoc
                   cdlatex
                   dedicated
                   diminish
                   elein
                   elfeed
                   elpy
                   epc
                   ercn
                   ess
                   evil-indent-textobject
                   evil-leader
                   evil-numbers
                   flymake-cursor
                   ghc
                   ggtags
                   gist
                   haskell-mode
                   highlight-parentheses
                   highlight-symbol
                   htmlize
                   jinja2-mode
                   json-mode
                   keyfreq
                   keywiz
                   lacarte
                   markdown-mode
                   multi-term
                   offlineimap
                   org-journal
                   paredit
                   pp-c-l
                   rainbow-mode
                   scratch
                   sml-modeline
                   twittering-mode
                   sackspace
                   undo-tree
                   queue
                   workgroups
                   yaml-mode
                   yasnippet
                   zencoding-mode
                   ))
  (unless (package-installed-p package)
    (package-install package)))

(defvar cofi/hostname (car (split-string system-name "\\." t)))
(defvar on-mobile? (cl-find cofi/hostname '("hitchhiker") :test #'string=))

(load "private" 'noerror)
(require 'cofi-evil)

(defvar cofi/standard-settings '(
                                 cofi-color
                                 cofi-autoloads
                                 cofi-helm
                                 cofi-buffer
                                 cofi-calendar
                                 cofi-desktop
                                 cofi-ediff
                                 cofi-elisp
                                 cofi-files
                                 cofi-func
                                 cofi-keys
                                 cofi-mail
                                 cofi-snippets
                                 cofi-ui
                                 cofi-org
                                 cofi-smartparens
                                 cofi-write
                                 cofi-completion
                                 cofi-workgroups
                                 cofi-modeline
                                 ))

(defvar cofi/full-settings '(
                             cofi-programming
                             cofi-project
                             cofi-vcs
                             cofi-shell
                             ))

(defvar cofi/full-emacs t "Load all settings not just minimal.")
(defvar cofi/comm-instance nil "This is an comm instance.")

(mapc #'require cofi/standard-settings)

(defun cofi/dont-mess-with-history ()
  (setq kill-emacs-hook nil
        cofi/before-kill-hook nil))

(add-to-list 'command-switch-alist
             '("comm" . (lambda (&rest ignore)
                          (setq cofi/comm-instance t
                                cofi/full-emacs nil)
                          (require 'cofi-erc)
                          (require 'cofi-news)
                          (add-hook 'emacs-startup-hook 'gnus 'append)
                          (run-with-timer 10 600 'offlineimap)
                          ;; Exit Emacs after quitting gnus
                          (add-hook 'gnus-after-exiting-gnus-hook #'save-buffers-kill-emacs)
                          (add-hook 'gnus-after-exiting-gnus-hook #'erc-log-save-all-buffers)
                          (add-hook 'gnus-after-exiting-gnus-hook (lambda () (erc-cmd-GQUIT ""))
                          (add-hook 'emacs-startup-hook #'cofi/dont-mess-with-history 'append)))))

(add-to-list 'command-switch-alist
             '("test" . (lambda (&rest ignore)
                          (add-hook 'emacs-startup-hook #'cofi/dont-mess-with-history 'append))))

(add-hook 'emacs-startup-hook (lambda ()
                                (on-comm-instance
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
