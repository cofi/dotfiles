(require-and-exec 'flymake
  ;; check only after save
  (setq flymake-no-changes-timeout 9999
        flymake-start-syntax-check-on-newline nil)

  (defun my-flymake-show-help ()
    "Display flymake message at point.
Useful in a tty without mouse-over.
Intended to be run from a local `post-command-hook'."
    (when (get-char-property (point) 'flymake-overlay)
      (let ((help (get-char-property (point) 'help-echo)))
        (if help (message "%s" help)))))

  (add-hook 'flymake-mode-hook
            '(lambda ()
               (add-hook 'post-command-hook 'my-flymake-show-help nil t)))
  )

(provide 'cofi-flymake)
