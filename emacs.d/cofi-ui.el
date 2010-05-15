(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(require-and-exec 'diminish
                  (eval-after-load "abbrev"
                    '(diminish 'abbrev-mode "Ab"))
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

(global-unset-key (kbd "C-x m"))
(require-and-exec 'idomenu
                  (global-set-key (kbd "C-x m") 'idomenu))

(require-and-exec 'recentf
      (setq recentf-auto-cleanup 'never)
      (recentf-mode t)
)

(require-and-exec 'sml-modeline
                  (sml-modeline-mode t))

(require-and-exec 'highlight-parentheses
                  (highlight-parentheses-mode t))

(require-and-exec 'uniquify
                  (setq uniquify-buffer-name-style 'reverse
                        uniquify-separator "/"
                        uniquify-after-kill-buffer-p t
                        uniquify-ignore-buffers-re "^\\*"))

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
                        ido-use-url-at-point t
                        ido-use-filename-at-point t
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

(when (string= hostname "hitchhiker")
    (display-battery-mode t))

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
(global-set-key (kbd "C-<backspace>") 'fixup-whitespace)

(add-hook 'diff-mode
          (lambda ()
            (local-set-key (kbd "q") 'kill-this-buffer)))

(global-set-key (kbd "C-c r") 'revert-buffer)

(require-and-exec 'cofi-func
  (global-set-key (kbd "C-x f") 'recentf-ido-find-file)
  (global-set-key (kbd "<up>") 'increment-number-at-point)
  (global-set-key (kbd "<down>") 'decrement-number-at-point)
  (global-set-key (kbd "C-;") 'comment-or-uncomment-current-line-or-region)
  )

(define-key function-key-map (kbd "C-<tab>") [?\M-\t])

;; enable functions
(put 'narrow-to-defun 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)

(provide 'cofi-ui)
