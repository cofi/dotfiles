(add-to-list 'load-path "~/.elisp/vendor/workgroups/")

(require 'workgroups)
(workgroups-mode 1)

(setq wg-mode-line-on t
      wg-mode-line-left-brace "{"
      wg-mode-line-right-brace "} "
      wg-query-for-save-on-emacs-exit nil
      wg-file (cofi/var-file "emacs/wg"))

(provide 'cofi-workgroups)
