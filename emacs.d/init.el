;; Clean GUI
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(defun add-to-path* (paths)
  "Expand every path and adds it and its subdirs to load-path."
  (mapc (lambda (x)
        (progn (add-to-list 'load-path x)
               (cd x)
               (normal-top-level-add-subdirs-to-load-path)))
       paths))

(add-to-path* '(
                "~/.elisp"
                "~/.emacs.d"
                ))

(defun load-all (libs)
  (mapc #'load libs))

(load-all '(
            "cofi-ack"
            "cofi-bindings"
            "cofi-buffer"
            "cofi-color"
            "cofi-column"
            "cofi-eldoc"
            "cofi-haskell"
            "cofi-ido"
            "cofi-lisp"
            "cofi-markers"
            "cofi-python"
            "cofi-recentf"
            "cofi-smex"
            "cofi-snippets"
            "cofi-tab"
            "cofi-vim"
            "cofi-windowing"
            "private"
            ))

;; Ensure that the rest (e.g. yasnippet) has been initialized
(load "cofi-autocompletion")

(defun require-all (packages)
    "Require all items in list."
    (mapc #'require packages))

(require-all '(
               autopair
               magit
               redo
               sml-modeline
               uniquify
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

(when (require 'auto-dictionary nil 'noerror)
  (add-hook 'flyspell-mode (lambda ()
                             (auto-dictionary-mode t))))

;; tab settings
(setq-default indent-tabs-mode nil)     ; no nasty tabs i say!
(setq-default tab-width 4)

;; Auto minor modes
(mapc (lambda (mode)
        (funcall mode t))
      '(
        autopair-global-mode            ; Yeah, gimme teh matching pairs
        global-font-lock-mode
        global-hl-line-mode
        global-linum-mode               ; show line numbers
        show-paren-mode                 ; show matching paren
        sml-modeline-mode
        transient-mark-mode
        winner-mode
        ))

(fringe-mode 'left-only)
(setq display-time-24hr-format t)
(display-time)
(display-battery-mode)

(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special

(setq-default abbrev-mode t)
(setq abbrev-file-name "~/.emacs.d/abbrevs")
(setq save-abbrevs t)
(quietly-read-abbrev-file)

(setq dabbrev-case-replace nil)

(setq backup-directory-alist '(("" . "~/.emacs-backups")))
(setq auto-save-default nil)

(defalias 'yes-or-no-p 'y-or-n-p)       ; don't make me type
(defalias 'at 'ansi-term)
(defalias 'eb 'eval-buffer)
(defalias 'er 'eval-region)

(defalias 'bl 'bookmark-bmenu-list)
(defalias 'bj 'bookmark-jump)
(defalias 'bs 'bookmark-set)
(defalias 'sb 'bookmark-save)

(defalias 'sl 'sort-lines)
(defalias 'dtw 'delete-trailing-whitespace)

(put 'narrow-to-defun 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)

(setq next-line-add-newlines t)

(mouse-avoidance-mode 'cat-and-mouse)
