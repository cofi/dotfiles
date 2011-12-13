(setq programming-hooks '(
                          python-mode-hook
                          haskell-mode-hook
                          emacs-lisp-mode-hook
                          sh-mode-hook
                          lisp-mode-hook
                          clojure-mode-hook
                          c-mode-hook
                          c++-mode-hook
                          java-mode-hook
                          ))

(mapc 'load '(
              "cofi-markers"
              "cofi-flymake"
              "cofi-haskell"
              "cofi-lisp"
              "cofi-python"
              "cofi-tex"
              "cofi-c"
              "cofi-java"
              ))

(defun cofi/close-after-successful-compilation (buffer desc)
  "Close compilation buffer window after successful compilation."
  (with-current-buffer buffer
    (unless (or (string-match "exited abnormally" desc)
               (progn (goto-char 0)
                      (re-search-forward "\\([wW]arning\\)\\|\\([eE]rror\\)" nil 'noerror)))
      (bury-buffer buffer)
      (winner-undo))))

(push 'cofi/close-after-successful-compilation compilation-finish-functions)

(provide 'cofi-programming)
