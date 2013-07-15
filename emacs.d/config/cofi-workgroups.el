(add-to-loadpath "~/.elisp/vendor/workgroups/")

(setq wg-mode-line-on t
      wg-mode-line-left-brace "{"
      wg-mode-line-right-brace "} "
      wg-query-for-save-on-emacs-exit nil
      wg-file (cofi/var-file "emacs/wg")
      wg-morph-on nil)

;; (require-and-exec 'workgroups
;;   (workgroups-mode))

(provide 'cofi-workgroups)
