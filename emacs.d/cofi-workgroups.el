(add-to-list 'load-path "~/.elisp/vendor/workgroups/")

(require 'workgroups)
(workgroups-mode 1)

(setq wg-mode-line-on nil
      wg-query-for-save-on-emacs-exit nil
      wg-file (cofi/var-file "emacs/wg"))

(provide 'cofi-workgroups)
