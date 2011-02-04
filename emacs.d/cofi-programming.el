(setq prog-mode-hooks '(
                        python-mode-hook
                        haskell-mode-hook
                        emacs-lisp-mode-hook
                        sh-mode-hook
                        lisp-mode-hook
                        clojure-mode-hook
                        c-mode-hook
                        c++-mode-hook
                        ))
(mapc 'load '(
              "cofi-markers"
              "cofi-flymake"
              "cofi-haskell"
              "cofi-lisp"
              "cofi-python"
              "cofi-tex"
              "cofi-c"
              ))

(provide 'cofi-programming)
