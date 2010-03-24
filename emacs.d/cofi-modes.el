(load "cofi-util")

(mapc 'require-pair
      (list
        (list 'autopair (lambda () (autopair-global-mode t)))
        (list 'sml-modeline (lambda () (sml-modeline-mode t)))
        (list 'highlight-parentheses (lambda () (highlight-parentheses-mode t)))
        )
      )

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

(fringe-mode 'left-only)

(provide 'cofi-modes)
