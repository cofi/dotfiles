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

(mapc 'require '(
              cofi-cedet
              cofi-markers
              cofi-flymake
              cofi-haskell
              cofi-lisp
              cofi-python
              cofi-tags
              cofi-tex
              cofi-c
              cofi-java
              cofi-xml
              ))

(defun cofi/close-after-successful-compilation (buffer desc)
  "Close compilation buffer window after successful compilation."
  (with-current-buffer buffer
    (unless (or (string-match "exited abnormally" desc)
               (progn (goto-char 0)
                      (re-search-forward "\\([wW]arning\\)\\|\\([eE]rror\\)" nil 'noerror)))
      (bury-buffer buffer))))

(push 'cofi/close-after-successful-compilation compilation-finish-functions)

(setq compilation-auto-jump-to-first-error t
      compilation-scroll-output 'first-error)

(provide 'cofi-programming)
