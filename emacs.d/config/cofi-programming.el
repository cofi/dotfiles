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
                          scala-mode-hook
                          ))

(defconst header-which-function-mode-header
  '(t (:eval (which-function))))

(define-minor-mode header-which-function-mode
  "Minor mode to show current functio in header."
  nil nil nil
  (if header-which-function-mode
      (progn
        (which-function-mode 1)
        (push header-which-function-mode-header header-line-format))
    (which-function-mode -1)
    (setq header-line-format (remove header-which-function-mode-header header-line-format))))

(add-to-hooks #'header-which-function-mode '(python-mode-hook
                                             c-mode-common-hook))

(mapc 'require '(
              cofi-cedet
              cofi-markers
              cofi-ess
              cofi-flymake
              cofi-haskell
              cofi-lisp
              cofi-python
              cofi-scala
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
