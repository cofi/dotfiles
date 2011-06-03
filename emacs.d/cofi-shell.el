;;; remove completions in shell-mode automatically
;;; works out of the box from emacs 24
;;; from http://snarfed.org/automatically_close_completions_in_emacs_shell_comint_mode
(when (string< emacs-version "24")
  (defun comint-close-completions ()
    (if comint-dynamic-list-completions-config
        (progn
          (set-window-configuration comint-dynamic-list-completions-config)
          (setq comint-dynamic-list-completions-config nil))))

  (defadvice comint-send-input (after close-completions activate)
    (comint-close-completions))

  (defadvice comint-dynamic-complete-as-filename (after close-completions activate)
    (if ad-return-value (comint-close-completions)))

  (defadvice comint-dynamic-simple-complete (after close-completions activate)
    (if (member ad-return-value '('sole 'shortest 'partial))
        (comint-close-completions)))

  (defadvice comint-dynamic-list-completions (after close-completions activate)
    (comint-close-completions)
    (if (not unread-command-events)
        ;; comint's "Type space to flush" swallows space. put it back in.
        (setq unread-command-events (listify-key-sequence " "))))
  )

(setq ansi-color-for-comint-mode t)

(require-and-exec 'shell-command
                  (shell-command-completion-mode))

(provide 'cofi-shell)
