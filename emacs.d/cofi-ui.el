;; Clean UI ========================================
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
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
      ido-use-url-at-point nil
      ido-use-filename-at-point nil
      ido-everywhere t
      ido-ignore-extensions t
      )

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
                               )))))
(ido-mode t)
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
                  )

(require-and-exec 'second-sel
                  (setq secondary-selection-ring-max 1000)
                  )
(setq kill-ring-max 1000)
(require 'browse-kill-ring+)

(require-and-exec 'recentf
                  (setq recentf-auto-cleanup 'never)
                  (recentf-mode t)
                  (setq recentf-exclude '(
                                          "\.recentf"
                                          "\.ido\.last"
                                          "coficore-sh"
                                          "hithhiker-sh"
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
                  (global-set-key (kbd "<f1>") 'multi-term)
                  (global-set-key (kbd "M-<f1>") 'multi-term-next)
                  ;; Super-<f1> for small terminal window (vertical split, above)
                  (global-set-key (kbd "s-<f1>") (lambda ()
                                                   (interactive)
                                                   (split-window nil 10)
                                                   (multi-term)
                                                   ))
                  )

(require-and-exec 'shell-command
                  (shell-command-completion-mode))

(require-and-exec 'framepop
                  (framepop-enable))

(require-and-exec 'eldoc
  (add-hook 'python-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
  (eldoc-add-command 'autopair-insert-opening))

(require-and-exec 'keychain-environment
                  (add-hook 'after-make-frame-functions
                            (lambda (frame)
                              (funcall 'refresh-keychain-environment)))
                  (add-hook 'after-init-hook 'refresh-keychain-environment))
;; ==================================================

;; Settings ========================================
(mapc (lambda (ext)
        (push ext completion-ignored-extensions))
      '(
        ".pdf" ".dvi" ".djvu" ".ps"
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

(setq battery-mode-line-format " [%L %p%%]")

(setq dired-recursive-copies 'always)
(setq ack-prompt-for-directory 'unless-guessed)

(mouse-avoidance-mode 'cat-and-mouse)

(setq vc-handled-backends '(SVN Hg)
      vc-follow-symlinks t)

;; Use UTF-8 dammit
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

;; Spelling
(setq speck-engine 'Hunspell
      speck-hunspell-library-directory "/usr/share/hunspell/"
      speck-hunspell-language-options nil ; defaults can bite
      )

(setq speck-hunspell-dictionary-alist '(("de" . "de_DE")
                                        ("en" . "en_US")))
;; Let speck highlight doublets
(setq speck-doublet t)

;; tab settings
(setq-default indent-tabs-mode nil     ; no nasty tabs i say!
              tab-width 4)

(setq dabbrev-case-replace nil)

(setq backup-directory-alist '(("" . "~/.emacs-backups")))
(setq auto-save-default nil)

(setq-default woman-use-own-frame nil
              woman-use-topic-at-point t)

(setq-default major-mode 'text-mode)
(setq dired-dwim-target t)
(add-hook 'dired-mode-hook 'auto-revert-mode)

(setq autopair-autowrap t)
;; ==================================================

;; Default modes ========================================
(require-and-exec 'autopair
                  (autopair-global-mode 1))
(transient-mark-mode t)
(display-time-mode t)
(global-font-lock-mode t)
(global-hl-line-mode t)
(global-linum-mode t)
(show-paren-mode t)
(when (string= hostname "hitchhiker")
    (display-battery-mode t))

(add-hook 'term-mode-hook (lambda () (linum-mode -1)))
;; ==================================================

;; Alias ========================================
(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'eb 'eval-buffer)
(defalias 'er 'eval-region)

(defalias 'bl 'bookmark-bmenu-list)
(defalias 'bj 'bookmark-jump)
(defalias 'bs 'bookmark-set)
(defalias 'sb 'bookmark-save)
;; ==================================================

;; KeyBindings ========================================
(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "C-M-h") 'backward-kill-word)
(global-set-key (kbd "C-c m") 'idomenu)

(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
(global-set-key (kbd "C-x M-b") 'ido-switch-buffer-other-window)
(global-set-key (kbd "C-x C-d") 'ido-display-buffer)
(global-set-key (kbd "C-x M-d") 'dired-other-window)

(global-set-key (kbd "C-c i") 'magit-status)

(eval-after-load "smex" '(smex-initialize))
(global-set-key (kbd "M-x") 'smex)

(eval-after-load "diff" '(define-key diff-mode-map (kbd "q") 'kill-this-buffer))

(global-set-key (kbd "C-c b") 'revert-buffer)

(require-and-exec 'cofi-func
  (global-set-key (kbd "C-x f") 'recentf-ido-find-file)
  (global-set-key (kbd "C-+") 'increment-number-at-point)
  (global-set-key (kbd "C--") 'decrement-number-at-point)
  (global-set-key (kbd "C-;") 'comment-or-uncomment-current-line-or-region)
  )

(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(define-key isearch-mode-map (kbd "C-h") 'backward-delete-char)

;; M-TAB is shadowed
(define-key function-key-map (kbd "C-TAB") (kbd "M-TAB"))

(global-set-key (kbd "<f12>") (lambda ()
                                (interactive) (switch-to-buffer "*scratch*")))

(global-set-key (kbd "C-c d") (make-hippie-expand-function
                               '(
                                 try-complete-file-name-partially
                                 try-complete-file-name
                                 )))

(when (locate-library "home-end")
  (global-set-key (kbd "<home>") 'home-end-home)
  (global-set-key (kbd "<end>") 'home-end-end))
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
;; ==================================================

;; external programs ========================================
(global-set-key (kbd "C-c f") 'browse-url-firefox)
(setq browse-url-firefox-new-window-is-tab t)
;; ==================================================

(provide 'cofi-ui)
