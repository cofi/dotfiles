(require-and-exec 'second-sel
                  (setq secondary-selection-ring-max 1000)
                  )
(setq kill-ring-max 1000)
(require 'browse-kill-ring+)

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

(mapc (lambda (ext)
        (add-to-list 'completion-ignored-extensions ext))
      '(
        ".pdf" ".dvi" ".djvu" ".ps"
        ".mov" ".mp4" ".ogv"
        ".mp3" ".ogg"
        ".doc" ".ods" ".odt" ".pps" ".ppt"
        ))

(require-and-exec 'ido
                  (setq ido-enable-regexp t
                        ido-enable-dot-prefix t
                        ido-enable-flex-matching t
                        ido-use-url-at-point t
                        ido-use-filename-at-point t
                        ido-everywhere t)
                  (setq ido-ignore-buffers '("\\` "
                                             "\\`\\*.*\\*"
                                             ))
                  (setq ido-ignore-extensions t)
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

(provide 'cofi-ui)
