(setq initial-scratch-message
      ";; Have a lot of fun!\n")

(defconst startup-time
  (current-time)
  "Time Emacs started.")

(mapc (lambda (dir)
             (add-to-list 'load-path dir)
             )
      '(
        "~/.elisp"
        "~/.elisp/auto-complete"
        "~/.elisp/haskell-mode"
        "~/.elisp/magit"
        "~/.emacs.d"
        ))

(defvar hostname (substring (shell-command-to-string "hostname") 0 -1))

(require 'cofi-util)
(require 'cofi-vim)
(load "private" 'noerror)

(mapc #'load '(
            "cofi-artist"
            "cofi-buffer"
            "cofi-calendar"
            "cofi-color"
            "cofi-column"
            "cofi-desktop"
            "cofi-eldoc"
            "cofi-flymake"
            "cofi-haskell"
            "cofi-lisp"
            "cofi-mail"
            "cofi-markers"
            "cofi-project"
            "cofi-python"
            "cofi-org"
            "cofi-snippets"
            "cofi-tex"
            "cofi-ui"
            "cofi-vc"
            "cofi-windowing"
            "cofi-autocompletion"
            "cofi-tab"
            ))

(mapc #'require '(
               auto-dictionary
               redo
           ))

(setq-default woman-use-own-frame nil
              woman-use-topic-at-point t)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(TeX-view-program-selection (quote (((output-dvi style-pstricks) "xdg-open") (output-pdf "xdg-open") (output-dvi "xdg-open") (output-html "xdg-open"))))
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
(setq-default ispell-dictionary "de_DE")

;; tab settings
(setq-default indent-tabs-mode nil)     ; no nasty tabs i say!
(setq-default tab-width 4)

(setq dabbrev-case-replace nil)

(setq backup-directory-alist '(("" . "~/.emacs-backups")))
(setq auto-save-default nil)

(setq dired-dwim-target t)
(add-hook 'dired-mode-hook 'auto-revert-mode)

(setq default-major-mode (lambda ()
                           (text-mode)
                           (set-viper-state-in-major-mode)))

(require-and-exec 'keychain-environment
                  (add-hook 'after-make-frame-functions
                            (lambda (frame)
                              (funcall 'refresh-keychain-environment)))
                  (add-hook 'after-init-hook 'refresh-keychain-environment))

(require-and-exec 'autopair
                  (autopair-global-mode t))
(transient-mark-mode t)

(color-theme-cofi)

(message "Time needed to load: %d seconds."
         (time-to-seconds (time-since startup-time)))
