;; Clean UI ========================================
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'tooltip-mode) (tooltip-mode -1))
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'blink-cursor-mode) (blink-cursor-mode -1))
;; ==================================================

;; IDO ========================================
(add-hook 'ido-setup-hook
          (lambda ()
            ;; first won't work because of viper binding
            (define-key ido-completion-map (kbd "C-h") 'ido-prev-match)
            (define-key ido-completion-map (kbd "C-l") 'ido-next-match)))

(setq ido-enable-regexp t
      ido-enable-dot-prefix t
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-use-url-at-point nil
      ido-use-filename-at-point nil
      ido-ignore-extensions t)

(eval-after-load "ido"
  '(progn
     (setq ido-ignore-buffers (append
                               ido-ignore-buffers
                               '(
                                 "\\` "
                                 "\\`\\*.*\\*"
                                 "_region_"
                                 )))
     (setq ido-ignore-directories (append
                                   ido-ignore-directories
                                   '(
                                     "^auto/$"
                                     "\\.prv/"
                                     "_region_"
                                     )))
     (setq ido-ignore-files (append
                             ido-ignore-files
                             '(
                               "^auto/$"
                               "_region_"
                               )))
     (fset 'ido-directory-too-big-p #'ignore)))

(ido-mode 'files)
(ido-everywhere 1)
;; ==================================================

;; other packages ========================================
(require-and-exec 'diminish
                  (eval-after-load "yasnippet"
                    '(diminish 'yas/minor-mode " Y"))
                  (eval-after-load "autopair"
                    '(diminish 'autopair-mode " p"))
                  (eval-after-load "eldoc"
                    '(diminish 'eldoc-mode " ED"))
                  (eval-after-load "highlight-parentheses"
                    '(diminish 'highlight-parentheses-mode))
                  (eval-after-load "undo-tree"
                    '(diminish 'undo-tree-mode)))

(require-and-exec 'ansi-color
                  (ansi-color-for-comint-mode-on))

(setq recentf-auto-cleanup 'never)
(setq recentf-max-saved-items 500)
(require-and-exec 'recentf
                  (recentf-mode 1)
                  (setq recentf-exclude '(
                                          "\\.recentf"
                                          "\\.ido\\.last"
                                          "Documents/bbdb"
                                          "Documents/diary"
                                          "\\.keychain/.*?-sh\\(-gpg\\)?"
                                          ))
                  )

(require-and-exec 'sml-modeline
                  (setq sml-modeline-len  8)
                  (sml-modeline-mode t))

(require-and-exec 'highlight-parentheses
                  (add-hook 'find-file-hook 'highlight-parentheses-mode))

(require-and-exec 'uniquify
                  (setq uniquify-buffer-name-style 'reverse
                        uniquify-separator "/"
                        uniquify-after-kill-buffer-p t
                        uniquify-ignore-buffers-re "^\\*"))

(require-and-exec 'multi-term
                  (global-set-key (kbd "<f11>") 'multi-term-dedicated-open)
                  (global-set-key (kbd "C-<f11>") 'multi-term-next))

(require-and-exec 'shell-command
                  (shell-command-completion-mode))

(require-and-exec 'dired+)

(require-and-exec 'point-stack
  (global-set-key (kbd "<f1>") 'point-stack-push)
  (global-set-key (kbd "<f2>") 'point-stack-pop)
  (global-set-key (kbd "C-<f2>") 'point-stack-forward-stack-pop))

(require-and-exec 'eldoc
  (add-hook 'python-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
  (eldoc-add-command 'autopair-insert-opening))

(require-and-exec 'keychain-environment
                  (add-hook 'after-make-frame-functions
                            (lambda (frame)
                              (funcall 'keychain/refresh)))
                  (add-hook 'after-init-hook 'keychain/refresh))
;; ==================================================

;; Settings ========================================
(mapc (lambda (ext)
        (push ext completion-ignored-extensions))
      '(
        ".dvi" ".djvu" ".ps"
        ".mov" ".mp4" ".ogv"
        ".mp3" ".ogg"
        ".doc" ".ods" ".odt" ".pps" ".ppt"
        ))

(setq display-time-24hr-format t
      display-time-string-forms '(day "." month " " 24-hours ":" minutes)
      display-time-mail-file 'none
      display-time-default-load-average nil)

(setq frame-title-format "emacs %b - <%f>"
      icon-title-format "emacs %b")

(setq global-mode-string '("" display-time-string
                           battery-mode-line-string
                           appt-mode-string))

(setq-default mode-line-format '("%e" mode-line-mule-info
                                 mode-line-client mode-line-modified
                                 mode-line-remote mode-line-frame-identification
                                 mode-line-buffer-identification
                                 mode-line-position
                                 (vc-mode vc-mode)
                                 (global-mode-string
                                  ("" global-mode-string
                                   #("--" 0 2 (help-echo "mouse-1: Select (drag to resize)\nmouse-2: Make current window occupy the whole frame\nmouse-3: Remove current window from display"))))
                                 mode-line-modes
                                 (which-func-mode
                                  ("" which-func-format
                                   #("--" 0 2 (help-echo "mouse-1: Select (drag to resize)\nmouse-2: Make current window occupy the whole frame\nmouse-3: Remove current window from display"))))
                                 viper-mode-string
                                 #("-%-" 0 3
                                   (help-echo "mouse-1: Select (drag to resize)\nmouse-2: Make current window occupy the whole frame\nmouse-3: Remove current window from display")))
              )

(setq battery-mode-line-format " [%L %p%%]")

(setq dired-recursive-copies 'always)
(setq ack-prompt-for-directory 'unless-guessed)

(mouse-avoidance-mode 'cat-and-mouse)

(setq vc-handled-backends '(SVN Hg)
      vc-follow-symlinks t)

(setq comint-prompt-read-only t)

(setq comment-style 'align)

(setq inhibit-startup-screen t)

(setq ediff-split-window-function 'split-window-horizontally)

;; Use UTF-8 dammit
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Spelling
(setq speck-engine 'Hunspell
      speck-hunspell-library-directory "/usr/share/hunspell/")

(setq speck-hunspell-dictionary-alist '( ("de" . "de_DE")
                                         ("en" . "en_US")))
;; Let speck highlight doublets
(setq speck-doublet t)

;; tab settings

(setq-default tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68
                              72 76 80 84 88 92 96 100 104 108 112 116 120)
              indent-tabs-mode nil     ; no nasty tabs i say!
              tab-width 4)

(setq dabbrev-case-replace nil)

(setq backup-directory-alist '(("" . "~/.emacs-backups")))
(setq auto-save-default nil
      auto-save-list-file-prefix "~/var/emacs/auto-save-list/save-")

(setq-default woman-use-own-frame nil
              woman-use-topic-at-point t
              woman-imenu t
              woman-cache-filename "~/.wmncach.el")

(eval-after-load "info"
  '(add-to-list 'Info-directory-list
         "~/doc/info"))

;; mixedCase to small_words_with_underscores (visually)
(setq glasses-separate-parentheses-p nil
      glasses-uncapitalize-p t
      glasses-uncapitalize-regexp "[a-zA-Z]")

(setq-default major-mode 'text-mode)
(setq dired-dwim-target t)
(add-hook 'dired-mode-hook 'auto-revert-mode)
(setq-default cursor-type 'bar)

(setq autopair-autowrap t)

(setq compilation-scroll-output 'first-error)
;; ==================================================

;; Default modes ========================================
(require-and-exec 'autopair
                  (autopair-global-mode 1))
(require-and-exec 'saveplace
                  (setq-default save-place t))
(transient-mark-mode t)
(display-time-mode t)
(global-font-lock-mode t)
(global-hl-line-mode t)
(column-number-mode 1)
(add-hook 'find-file-hook (lambda () (linum-mode 1))) ;start linum only for real files
(defadvice normal-mode (after re-line activate) (linum-mode 1)) ; fix normal-mode disabling
(show-paren-mode t)
(when (string= hostname "hitchhiker")
    (display-battery-mode t))

(setq winner-dont-bind-my-keys t)
(require-and-exec 'winner
                  (winner-mode 1))

(setq kill-buffer-query-functions (remq 'process-kill-buffer-query-function
                                        kill-buffer-query-functions))
;; ==================================================

;; Alias ========================================
(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'eb 'eval-buffer)
(defalias 'er 'eval-region)

(defalias 'cofi/file 'ido-find-file)
(defalias 'cofi/buffer-alternate 'ido-switch-buffer)

(if (fboundp 'anything-buffers+)
    (progn
      (defalias 'cofi/buffer 'cofi/anything-buffers)
      (defalias 'cofi/file-alternate 'cofi/anything-files))
  (defalias 'cofi/buffer 'ido-switch-buffer)
  (defalias 'cofi/file-alternate 'ido-find-file))
;; ==================================================

;; KeyBindings ========================================
(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "C-M-h") 'backward-kill-word)

(if (fboundp 'anything-imenu)
    (global-set-key (kbd "C-c i") 'anything-imenu)
  (global-set-key (kbd "C-c i") 'idomenu))

(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
(global-set-key (kbd "C-x M-b") 'ido-switch-buffer-other-window)
(global-set-key (kbd "C-x C-d") 'ido-display-buffer)
(global-set-key (kbd "C-x M-d") 'dired-other-window)

(global-set-key (kbd "C-c g") 'magit-status)

(add-hook 'diff-mode-hook '(lambda ()
                            (local-set-key (kbd "q") 'kill-this-buffer)))

(global-set-key (kbd "C-c b") 'revert-buffer)

(global-set-key (kbd "C-x f") 'cofi/file)
(global-set-key (kbd "C-x C-f") 'cofi/file-alternate)
(global-set-key (kbd "C-x b") 'cofi/buffer)
(global-set-key (kbd "C-x B") 'cofi/buffer-alternate)

(require-and-exec 'cofi-func
  (global-set-key (kbd "C-;") 'comment-or-uncomment-current-line-or-region)
  (global-set-key (kbd "<f4>") 'cofi/macro-dwim)
  (global-set-key (kbd "S-<f4>") 'cofi/reset-macro)

  (add-hook 'artist-mode-init-hook
            (lambda ()
              (define-key artist-mode-map (kbd "C-c C-a C-o")
                'artist-ido-select-operation)
              (define-key artist-mode-map (kbd "C-c C-a C-c")
                'artist-ido-select-settings)))
  )

(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(define-key isearch-mode-map (kbd "C-h") 'backward-delete-char)

(global-set-key (kbd "M-r") 'query-replace-regexp)
(global-set-key (kbd "M-R") 'query-replace)

(when (locate-library "scratch")
  (autoload 'scratch "scratch" nil t)
  (global-set-key (kbd "<f12>") 'scratch))

(when (locate-library "home-end")
  (global-set-key (kbd "<home>") 'home-end-home)
  (global-set-key (kbd "<end>") 'home-end-end))

;;; mode keymap
(let ((map (make-sparse-keymap)))
  (define-key map (kbd "f") 'auto-fill-mode)
  (define-key map (kbd "o") 'orgstruct++-mode)
  (define-key map (kbd "p") 'pretty-mode)
  (define-key map (kbd "r") 'auto-revert-mode)
  (define-key map (kbd "s") 'speck-mode)
  (define-key map (kbd "t") 'orgtbl-mode)
  (define-key map (kbd "w") 'whitespace-mode)
  (global-set-key (kbd "C-x m") map))
;;; insert keymap
(let ((map (make-sparse-keymap)))
  (define-key map (kbd "b") 'insert-buffer)
  (define-key map (kbd "c") 'clipper-insert)
  (define-key map (kbd "f") 'insert-file)
  (define-key map (kbd "s") 'yas/insert-snippet)
  (global-set-key (kbd "C-x i") map))

;; ==================================================

;; Colums ========================================
(require-and-exec 'column-marker
    (defun highlight-80 ()
      (interactive)
      (column-marker-1 80))

    (add-hook 'python-mode-hook 'highlight-80))

(setq-default fill-column 80)
;; ==================================================

;; enable functions ========================================
(put 'narrow-to-defun 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'set-goal-column 'disabled nil)
;; ==================================================

;; prettyfying ==============================
(add-to-list 'load-path "~/.elisp/vendor/pretty-mode/")
(require-and-exec 'pretty-mode
                  (global-pretty-mode 1)
                  (mapc (lambda (mode)
                          (pretty-add-keywords mode '(
                                                      ("=" . "←")
                                                      ("==" . "=")
                                                      )))
                        '(python-mode c-mode java-mode cpp-mode)))
;; ========================================

(provide 'cofi-ui)
