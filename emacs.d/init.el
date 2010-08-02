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
        "~/.emacs.d"
        "~/.elisp/vendor/magit"
        "~/.elisp/vendor/sackspace"
        "/usr/local/share/emacs/site-lisp/semi/"
        "/usr/local/share/emacs/site-lisp/flim/"
        "/usr/local/share/emacs/site-lisp/apel/"
        "/usr/local/share/emacs/23.2/site-lisp/emu/"
        "/usr/local/share/emacs/site-lisp/wl/"
        ))

(defvar hostname (substring (shell-command-to-string "hostname") 0 -1))

(require 'cofi-util)
(require 'cofi-vim)
(load "private" 'noerror)

(mapc #'load '(
            "cofi-artist"
            "cofi-buffer"
            "cofi-bbdb"
            "cofi-calendar"
            "cofi-color"
            "cofi-column"
            "cofi-desktop"
            "cofi-flymake"
            "cofi-haskell"
            "cofi-lisp"
            "cofi-mail"
            "cofi-markers"
            "cofi-project"
            "cofi-python"
            "cofi-snippets"
            "cofi-tex"
            "cofi-ui"
            "cofi-vc"
            "cofi-org"
            "cofi-windowing"
            "cofi-completion"
            ))

(require 'redo)

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
 '(framepop-enable-keybinding "<f11>")
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

;; Autoloads  ------------------------------
(autoload 'ack "full-ack" "Run ack." t)
(autoload 'ack-same "full-ack" "Run ack only in buffers matching the current major mode." t)

(autoload 'adict-change-dictionary "auto-dictionary"
  "Set buffer language to LANG and stop detecting it automatically." t)
(autoload 'adict-guess-dictionary "auto-dictionary"
  "Automatically change ispell dictionary based on buffer language." t)
(autoload 'auto-dictionary-mode "auto-dictionary"
  "A minor mode that automatically sets `ispell-dictionary`." t)

(autoload 'boxquote-defun "boxquote" "Boxquote the current defun" t)
(autoload 'boxquote-insert-buffer "boxquote" "Insert & boxquote a buffer" t)
(autoload 'boxquote-insert-file "boxquote" "Insert & boxquote a file" t)
(autoload 'boxquote-paragraph "boxquote" "Boxquote the current paragraph" t)
(autoload 'boxquote-region "boxquote" "Boxquote the current region" t)
(autoload 'boxquote-shell-command "boxquote"
  "Insert & boxquote output of shell command" t)
(autoload 'boxquote-unbox "boxquote" "Remove boxquote that surrounds point" t)

(autoload 'dedicated-mode "dedicated" "Dedicate currect buffer." t)

(autoload 'dired-jump "dired" "Jump to current buffer's file in dired" t)

(autoload 'home-end-end "home-end" "Go to end of line/window/buffer." t)
(autoload 'home-end-home "home-end" "Go to beginning of line/window/buffer." t)

(autoload 'magit-status "magit" nil t)

(autoload 'markdown-mode "markdown" "Mode for markdown files" t)

(autoload 'rainbow-mode "rainbow-mode" "Highlight color names in buffer" t)

(autoload 'trivial-cite "tc"
  "A simple citation function for use in news/mailreaders." t)
  
(autoload 'wl "wl" "Wanderlust" t)
(autoload 'wl-draft "wl-draft" "Write draft with Wanderlust." t)
(autoload 'wl-other-frame "wl" "Wanderlust on new frame." t)
(autoload 'wl-user-agent-compose "wl-draft" "Compose mail with Wanderlust" t)
;; -----------------------------------------

(color-theme-cofi)

(message "Time needed to load: %d seconds."
         (time-to-seconds (time-since startup-time)))
