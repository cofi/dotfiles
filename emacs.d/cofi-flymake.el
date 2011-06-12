;; check only after save
(setq flymake-no-changes-timeout 9999
      flymake-start-syntax-check-on-newline nil)
(require 'flymake)
(require 'flymake-cursor)

(provide 'cofi-flymake)
