(when (require 'autopair nil 'noerror)
        autopair-global-mode
)

(defun require-and-exec (feature fun)
  "Require the feature and call fun if it was successfull loaded."
  (if (require feature nil 'noerror)
      (when fun
        (funcall fun))
    (message (format "%s not loaded" feature))))

(defun require-pair (pair)
  "Unpack a pair and call `require-and-exec' on it."
  (require-and-exec
   (car pair)
   (cadr pair)))

(mapc 'require-pair
      '(
        (autopair . (lambda ()(autopair-global-mode t)))
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
