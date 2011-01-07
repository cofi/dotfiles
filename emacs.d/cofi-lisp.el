;; Add ` '-Pair for elisp
(add-hook 'emacs-lisp-mode-hook
           #'(lambda ()
               (push '(?` . ?')
                     (getf autopair-extra-pairs :comment))
               (push '(?` . ?')
                     (getf autopair-extra-pairs :string))))
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-c") 'eval-buffer)
            (setq mode-name "eL")))

(require-and-exec 'paredit
  (dolist (hook '(lisp-interaction-mode-hook emacs-lisp-mode-hook
                  lisp-mode-hook clojure-mode-hook))
    (add-hook hook #'(lambda () (setq autopair-dont-activate t)))
    (add-hook hook 'enable-paredit-mode t)))

(define-key read-expression-map (kbd "TAB") 'lisp-complete-symbol)

(provide 'cofi-lisp)
