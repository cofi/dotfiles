(load "cofi-util")

(mapc 'require-pair
      '(
        (autopair . (lambda () (autopair-global-mode t)))
        (sml-modeline . (lambda () (sml-modeline-mode t)))
        (highlight-parentheses . (lambda () (highlight-parentheses-mode t)))
        )
      )

(mapc (lambda (mode)
        (funcall mode t))
      '(
        display-battery-mode
        global-font-lock-mode
        global-hl-line-mode
        global-linum-mode
        show-paren-mode
        transient-mark-mode
        winner-mode
        ))

(fringe-mode 'left-only)
(setq display-time-24hr-format t)
(display-time)

(provide 'cofi-modes)
