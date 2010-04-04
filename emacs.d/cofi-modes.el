(require-and-exec 'autopair (autopair-global-mode t))

(require-and-exec 'sml-modeline (sml-modeline-mode t))

(require-and-exec 'highlight-parentheses (highlight-parentheses-mode t))

(setq display-time-24hr-format t)
;; dirty hack to suppress threshold showing
(setq display-time-load-average-threshold 100)

(mapc (lambda (mode)
        (funcall mode t))
      '(
        display-battery-mode
        display-time-mode
        global-font-lock-mode
        global-hl-line-mode
        global-linum-mode
        show-paren-mode
        transient-mark-mode
        winner-mode
        ))

(provide 'cofi-modes)
