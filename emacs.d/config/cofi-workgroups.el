(add-to-loadpath "~/.elisp/vendor/workgroups/")

(setq wg-mode-line-on nil
      wg-mode-line-left-brace "{"
      wg-mode-line-right-brace "} "
      wg-query-for-save-on-emacs-exit nil
      wg-file (cofi/var-file "emacs/wg")
      wg-morph-on nil)

(require 'workgroups)
(defun wg-mode-line-add-display ())
(workgroups-mode 1)

(provide 'cofi-workgroups)
