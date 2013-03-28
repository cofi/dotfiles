(require 'paredit)
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq mode-name "eL")))

(add-hook 'emacs-lisp-mode-hook
          (gen-local-fill-keymap-hook "C-c C-c" 'eval-buffer
                                      "C-c C-r" 'ielm
                                      "M-TAB"   'helm-lisp-completion-at-point
                                      [remap completion-at-point] 'helm-lisp-completion-at-point))

(add-hook 'ielm-mode-hook 'eldoc-mode)

(add-to-hooks #'enable-paredit-mode '(lisp-interaction-mode-hook
                                      emacs-lisp-mode-hook
                                      inferior-emacs-lisp-mode))

(provide 'cofi-elisp)
