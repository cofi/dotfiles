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
          (gen-local-fill-keymap-hook "C-c C-c" 'eval-buffer
                                      "C-c C-r" 'ielm
                                      "M-TAB"   'anything-lisp-completion-at-point
                                      [remap completion-at-point] 'anything-lisp-completion-at-point))

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
