(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(require-and-exec 'diminish
                  (eval-after-load "yasnippet"
                    '(diminish 'yas/minor-mode " Y"))
                  (eval-after-load "autopair"
                    '(diminish 'autopair-mode " p"))
                  )

(require-and-exec 'second-sel
                  (setq secondary-selection-ring-max 1000)
                  )
(setq kill-ring-max 1000)
(require 'browse-kill-ring+)

(require-and-exec 'magit
                  (global-set-key (kbd "C-c i") 'magit-status))

(require-and-exec 'idomenu
                  (global-set-key (kbd "C-c m") 'idomenu))

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
                  )

(require-and-exec 'shell-command
                  (shell-command-completion-mode))

(require-and-exec 'framepop
                  (framepop-enable))

(add-hook 'term-mode-hook (lambda () (linum-mode -1)))

(require-and-exec 'eldoc
  (add-hook 'python-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
  (eldoc-add-command 'autopair-insert-opening)
  (diminish 'eldoc-mode " ED")
  )

(mapc (lambda (ext)
        (push ext completion-ignored-extensions))
      '(
        ".pdf" ".dvi" ".djvu" ".ps"
        ".mov" ".mp4" ".ogv"
        ".mp3" ".ogg"
        ".doc" ".ods" ".odt" ".pps" ".ppt"
        ))

;; IDO
(require-and-exec 'ido
                  (setq ido-enable-regexp t
                        ido-enable-dot-prefix t
                        ido-enable-flex-matching t
                        ido-use-url-at-point nil
                        ido-use-filename-at-point nil
                        ido-everywhere t)
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
                  (setq ido-ignore-extensions t)
                  (global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
                  (global-set-key (kbd "C-x M-b") 'ido-switch-buffer-other-window)
                  (global-set-key (kbd "C-x C-d") 'ido-display-buffer)
                  (global-set-key (kbd "C-x M-d") 'dired-other-window)

                  (ido-mode t))

(require-and-exec 'smex
      (smex-initialize)
      (global-set-key (kbd "M-a") 'smex)
      (global-set-key (kbd "M-x") 'smex))

(setq display-time-24hr-format t
      display-time-string-forms '(" " day "." month " " 24-hours ":" minutes)
      display-time-mail-file 'none
      display-time-default-load-average nil)

(setq frame-title-format "emacs %b - <%f>"
      icon-title-format "emacs %b")

(setq battery-mode-line-format " [%L %p%%]")
(when (string= hostname "hitchhiker")
    (display-battery-mode t))

(setq dired-recursive-copies 'always)

(display-time-mode t)
(global-font-lock-mode t)
(global-hl-line-mode t)
(global-linum-mode t)
(show-paren-mode t)

(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'eb 'eval-buffer)
(defalias 'er 'eval-region)

(defalias 'bl 'bookmark-bmenu-list)
(defalias 'bj 'bookmark-jump)
(defalias 'bs 'bookmark-set)
(defalias 'sb 'bookmark-save)

(mouse-avoidance-mode 'cat-and-mouse)

(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "C-M-h") 'backward-kill-word)

(if (require 'greedy-delete nil t)
    (global-set-key (kbd "C-<backspace>") 'gd-electric-delete)
  (global-set-key (kbd "C-<backspace>") 'fixup-whitespace))

(add-hook 'diff-mode
          (lambda ()
            (local-set-key (kbd "q") 'kill-this-buffer)))

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

(define-key function-key-map (kbd "C-<tab>") [?\M-\t])
(global-set-key (kbd "<f12>") (lambda ()
                                (interactive) (switch-to-buffer "*scratch*")))

(global-set-key (kbd "C-c d") (make-hippie-expand-function
                               '(
                                 try-complete-file-name-partially
                                 try-complete-file-name
                                 )))

(autoload 'ack-same "full-ack" nil t)
(autoload 'ack "full-ack" nil t)
(setq ack-prompt-for-directory 'unless-guessed)

(autoload 'dedicated-mode "dedicated" nil t)

(autoload 'home-end-home "home-end" nil t)
(autoload 'home-end-end "home-end" nil t)

(global-set-key (kbd "<home>") 'home-end-home)
(global-set-key (kbd "<end>") 'home-end-end)

;; enable functions
(put 'narrow-to-defun 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;; external programs
(global-set-key (kbd "C-c f") 'browse-url-firefox)
(setq browse-url-firefox-new-window-is-tab t)

(provide 'cofi-ui)
