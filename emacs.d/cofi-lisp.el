;; Add ` '-Pair for elisp
(add-hook 'emacs-lisp-mode-hook
           #'(lambda ()
               (push '(?` . ?')
                     (getf autopair-extra-pairs :comment))
               (push '(?` . ?')
                     (getf autopair-extra-pairs :string))))

(add-hook 'lisp-mode-hook 
          (lambda () (local-set-key (kbd "RET") 'newline-and-indent))) 

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (local-set-key (kbd "RET") 'newline-and-indent)
            (local-set-key (kbd "C-c C-c") 'eval-buffer)
            (setq mode-name "elisp")))

(provide 'cofi-lisp)
