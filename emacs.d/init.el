(mapc (lambda (dir)
             (add-to-list 'load-path dir)
             )
      '("~/.elisp"
        "~/.elisp/auto-complete"
        "~/.elisp/haskell-mode"
        "~/.elisp/magit"
        "~/.emacs.d" ))

(defvar hostname (substring (shell-command-to-string "hostname") 0 -1))

(require 'cofi-util)
(require 'cofi-vim)

(mapc #'load '(
            "cofi-ack"
            "cofi-bindings"
            "cofi-buffer"
            "cofi-color"
            "cofi-column"
            "cofi-desktop"
            "cofi-eldoc"
            "cofi-flymake"
            "cofi-haskell"
            "cofi-lisp"
            "cofi-markers"
            "cofi-python"
            "cofi-recentf"
            "cofi-snippets"
            "cofi-tab"
            "cofi-term"
            "cofi-tex"
            "cofi-ui"
            "cofi-vc"
            "cofi-windowing"
            "private"
            ))

;; Ensure that the rest (e.g. yasnippet) has been initialized
(load "cofi-autocompletion")

(mapc #'require '(
               auto-dictionary
               magit
               redo
               w3m-load
           ))

(setq-default woman-use-own-frame nil
              woman-use-topic-at-point t)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(coding-category-utf-8 nil t)
 '(column-number-mode t)
 '(comint-prompt-read-only t)
 '(comment-style (quote align))
 '(inhibit-startup-screen t))

;; Use UTF-8 dammit
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

;; Spelling
(setq-default ispell-program-name "aspell")
(setq-default ispell-default-dictionary "en_US")

;; tab settings
(setq-default indent-tabs-mode nil)     ; no nasty tabs i say!
(setq-default tab-width 4)

(setq-default abbrev-mode t)
(setq abbrev-file-name "~/.emacs.d/abbrevs")
(setq save-abbrevs t)
(quietly-read-abbrev-file)

(setq dabbrev-case-replace nil)

(setq backup-directory-alist '(("" . "~/.emacs-backups")))
(setq auto-save-default nil)

(setq dired-dwim-target t)

(require-and-exec 'keychain-environment
                  (add-hook 'after-make-frame-functions
                            (lambda (frame)
                              (funcall 'refresh-keychain-environment))))

(require-and-exec 'autopair
                  (autopair-global-mode t))
(transient-mark-mode t)
