(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(require-and-exec 'sml-modeline
                  (sml-modeline-mode t))

(require-and-exec 'highlight-parentheses
                  (highlight-parentheses-mode t))

(require-and-exec 'uniquify 
                  (setq uniquify-buffer-name-style 'reverse
                        uniquify-separator "/"
                        uniquify-after-kill-buffer-p t
                        uniquify-ignore-buffers-re "^\\*"))

(require-and-exec 'ido
                  (setq ido-enable-regexp t
                        ido-enable-dot-prefix t
                        ido-enable-flex-matching t
                        ido-show-dot-for-dired t
                        ido-use-url-at-point t
                        ido-use-filename-at-point t
                        ido-everywhere t)
                  (ido-mode t))

(require-and-exec 'smex
      (smex-initialize)
      (global-set-key (kbd "M-a") 'smex)
      (global-set-key (kbd "M-x") 'smex))

(setq display-time-24hr-format t
      display-time-mail-file 'none
      display-time-default-load-average nil)

(when (string= hostname "hitchhiker")
    (display-battery-mode t))

(display-time-mode t)
(global-font-lock-mode t)
(global-hl-line-mode t)
(global-linum-mode t)
(show-paren-mode t)

(defalias 'yes-or-no-p 'y-or-n-p)       ; don't make me type
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

(mouse-avoidance-mode 'cat-and-mouse)
(color-theme-cofi)

(provide 'cofi-ui)
