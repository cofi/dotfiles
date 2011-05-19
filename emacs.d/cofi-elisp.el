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
(require 'eldoc-extensions)

(add-to-hooks #'enable-paredit-mode '(lisp-interaction-mode-hook
                                      emacs-lisp-mode-hook))

(defadvice paredit-mode (after subst-autopair activate)
    "Disable autopair when running paredit."
    (setq autopair-dont-activate paredit-mode))

(provide 'cofi-elisp)
