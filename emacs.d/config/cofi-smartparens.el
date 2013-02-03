(add-to-loadpath "~/.elisp/vendor/smartparens/")
(require 'smartparens)

(push 'sldb-mode sp-ignore-modes-list)

(sp-pair "'" nil :unless '(sp-point-after-word-p))

(sp-with-modes '(emacs-lisp-mode
                 inferior-emacs-lisp-mode
                 lisp-interaction-mode
                 common-lisp-mode)
  (sp-local-pair "'" nil :actions nil))

(sp-with-modes '(tex-mode
                 plain-tex-mode
                 latex-mode)
  (sp-local-pair "$" "$")
  (sp-local-pair "\\[" "\\]")
  (sp-local-pair "\\(" "\\)"))

(sp-with-modes '(markdown-mode
                 rst-mode)
  (sp-local-pair "`" "`"))

(add-to-hooks #'smartparens-mode '(text-mode-hook prog-mode-hook))

(provide 'cofi-smartparens)
