(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
(defun cofi/fortune2scratch ()
  "Return a comment-padded fortune cookie."
  (let ((cookie (shell-command-to-string "fortune -a")))
    (concat
     (replace-regexp-in-string " *$" ""
        (replace-regexp-in-string "^" ";; " cookie))
     "\n")))
(setq initial-scratch-message (cofi/fortune2scratch))

(setq custom-safe-themes '(;; cofi-dark
                           "e77e80399143cc5fe28744824f774ea0505d3ddb"
                           ;; cofi-light
                           "8193834d90b9929336ca61bac4d3dac243c41759"
                           ))
(load-theme 'cofi-dark)

(defun add-to-loadpath (&rest dirs)
  (dolist (dir dirs load-path)
    (add-to-list 'load-path (expand-file-name dir) nil #'string=)))

(add-to-loadpath "~/.elisp"
                 "~/.emacs.d"
                 "~/.elisp/vendor/sackspace"
                 "~/.elisp/vendor/keychain-environment"
                 "~/.elisp/vendor/pylookup")

(require 'cofi-util)
(when (or (and (version< emacs-version "24")
               (load (expand-file-name "~/.emacs.d/elpa/package.el")))
          (require 'package))
  (pour-lists package-archives '(("marmalade" . "http://marmalade-repo.org/packages/")
                                 ("elpa" . "http://tromey.com/elpa/")))
  (package-initialize)
  (dolist (package '(
                     all
                     ace-jump-mode
                     auctex
                     auto-complete
                     auto-dictionary
                     autopair
                     bookmark+
                     boxquote
                     c-eldoc
                     cdlatex
                     dedicated
                     diminish
                     elein
                     evil-numbers
                     flymake-cursor
                     full-ack
                     gist
                     highlight-parentheses
                     highlight-80+
                     htmlize
                     keyfreq
                     keywiz
                     lacarte
                     magit
                     magithub
                     markdown-mode
                     multi-term
                     offlineimap
                     paredit
                     pp-c-l
                     rainbow-mode
                     scratch
                     sml-modeline
                     undo-tree
                     workgroups
                     yaml-mode
                     ))
    (unless (package-installed-p package)
      (package-install package))))

(defvar cofi/hostname (car (split-string system-name "\\." t)))
(defvar on-mobile? (find cofi/hostname '("hitchhiker") :test #'string=))

(load "private" 'noerror)
(require 'cofi-evil)

(defvar cofi/standard-settings '("cofi-autoloads"
                                 "cofi-anything"
                                 "cofi-buffer"
                                 "cofi-calendar"
                                 "cofi-desktop"
                                 "cofi-ediff"
                                 "cofi-elisp"
                                 "cofi-files"
                                 "cofi-func"
                                 "cofi-keys"
                                 "cofi-mail"
                                 "cofi-markup"
                                 "cofi-project"
                                 "cofi-snippets"
                                 "cofi-ui"
                                 "cofi-org"
                                 "cofi-vcs"
                                 "cofi-shell"
                                 "cofi-workgroups"
                                 "cofi-completion"))
(defvar cofi/full-settings '("cofi-programming"
                             "nxhtml/autostart.el"))

(defvar cofi/full-emacs t "Load all settings not just minimal.")
(defvar cofi/mail-instance nil "This is an email instance.")

(mapc #'load cofi/standard-settings)

(add-to-list 'command-switch-alist
             '("gnus" . (lambda (&rest ignore)
                        (setq cofi/mail-instance t)
                        (setq cofi/full-emacs nil)
                        (add-hook 'emacs-startup-hook 'gnus 'append)
                        ;; Exit Emacs after quitting gnus
                        (add-hook 'gnus-after-exiting-gnus-hook 'save-buffers-kill-emacs))))

(add-hook 'emacs-startup-hook (lambda ()
                                (on-mail-instance
                                  (global-linum-mode -1))
                                (on-full-instance
                                  (mapc #'load cofi/full-settings)
                                  (workgroups-mode 1))))
(cofi-next-file-assoc)
(cofi/next-colorscheme)
(add-hook 'emacs-startup-hook (lambda ()
                                (message "Time needed to load: %s seconds."
                                         (emacs-uptime "%s")))
          'append)

(add-hook 'after-save-hook 'byte-compile-config-on-save)
