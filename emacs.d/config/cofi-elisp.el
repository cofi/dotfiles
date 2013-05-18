(require 'paredit)
(add-to-loadpath "~/.elisp/vendor/macrostep/")
(require 'macrostep)
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq mode-name "eL")))

(add-hook 'emacs-lisp-mode-hook
          (gen-local-fill-keymap-hook "C-c C-c" 'eval-buffer
                                      "C-c C-r" 'ielm
                                      "C-c e"   'macrostep-expand
                                      "M-TAB"   'helm-lisp-completion-at-point
                                      [remap completion-at-point] 'helm-lisp-completion-at-point))

(add-hook 'ielm-mode-hook 'eldoc-mode)

(provide 'cofi-elisp)
