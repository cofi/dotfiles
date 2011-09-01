(add-to-loadpath "~/.elisp/vendor/workgroups/")

(require-and-exec 'workgroups)

(setq wg-mode-line-on t
      wg-mode-line-left-brace "{"
      wg-mode-line-right-brace "} "
      wg-query-for-save-on-emacs-exit nil
      wg-file (cofi/var-file "emacs/wg")
      wg-morph-on (library-byte-compiled-p "workgroups"))

(provide 'cofi-workgroups)
