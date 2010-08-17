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
      ido-use-url-at-point nil
      ido-use-filename-at-point nil
      ido-ignore-extensions t
      ido-max-directory-size 300000)

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
                  )

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

(setq framepop-enable-keybinding "<f11>")
(require-and-exec 'framepop (framepop-enable))

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

(setq comint-prompt-read-only t)

(setq comment-style 'align)

(setq inhibit-startup-screen t)

;; Use UTF-8 dammit
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

;; Spelling
(setq speck-engine 'Hunspell
      speck-hunspell-library-directory "/usr/share/hunspell/")

(setq speck-hunspell-dictionary-alist '( ("de" . "de_DE")
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
              woman-use-topic-at-point t
              woman-cache-filename "~/.wmncach.el")

;; mixedCase to small_words_with_underscores (visually)
(setq glasses-separate-parentheses-p nil
      glasses-uncapitalize-p t
      glasses-uncapitalize-regexp "[A-Z]")

(setq-default major-mode 'text-mode)
(setq dired-dwim-target t)
(add-hook 'dired-mode-hook 'auto-revert-mode)
(setq-default cursor-type 'bar)

(setq autopair-autowrap t)
;; ==================================================

;; Default modes ========================================
(require-and-exec 'autopair
                  (autopair-global-mode 1))
(transient-mark-mode t)
(display-time-mode t)
(global-font-lock-mode t)
(global-hl-line-mode t)
(column-number-mode 1)
(add-hook 'find-file-hook (lambda () (linum-mode 1))) ;start linum only for real files
(show-paren-mode t)
(when (string= hostname "hitchhiker")
    (display-battery-mode t))
;; ==================================================

;; Alias ========================================
(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'eb 'eval-buffer)
(defalias 'er 'eval-region)

(defalias 'cofi/file 'ido-find-file)
(defalias 'cofi/buffer-alternate 'ido-switch-buffer)

(if (fboundp 'anything-buffers+)
    (progn
      (defalias 'cofi/buffer 'anything-buffers+)
      (defalias 'cofi/file-alternate 'cofi/anything-files))
  (defalias 'cofi/buffer 'ido-switch-buffer)
  (defalias 'cofi/file-alternate 'ido-find-file))
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

(eval-after-load "diff" '(define-key diff-mode-map (kbd "q") 'kill-this-buffer))

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

;; M-TAB is shadowed
(define-key function-key-map (kbd "C-TAB") (kbd "M-TAB"))

(require-and-exec 'scratch
                  (global-set-key (kbd "<f12>") 'scratch))

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
