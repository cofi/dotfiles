(require 'paredit)
;; Add ` '-Pair for elisp
(add-hook 'emacs-lisp-mode-hook
           #'(lambda ()
               (push '(?` . ?')
                     (getf autopair-extra-pairs :comment))
               (push '(?` . ?')
                     (getf autopair-extra-pairs :string))))
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq mode-name "eL")))

(add-hook 'emacs-lisp-mode-hook
          (gen-fill-keymap-hook emacs-lisp-mode-map
                                "C-c C-c" 'eval-buffer))
(add-hook 'ielm-mode-hook 'eldoc-mode)

(require 'eldoc-eval)

(add-to-hooks #'enable-paredit-mode '(lisp-interaction-mode-hook
                                      emacs-lisp-mode-hook
                                      inferior-emacs-lisp-mode))

(defadvice paredit-mode (after subst-autopair activate)
    "Disable autopair when running paredit."
    (if paredit-mode
        (autopair-mode -1)
        (autopair-mode 1)))

(provide 'cofi-elisp)
