(add-to-loadpath "~/.elisp/vendor/smartparens/")
;;; replace default pairs
(setq sp-pairs '((t
                  .
                  ((:open "\"" :close "\"" :actions (insert wrap))
                   (:open "'"  :close "'"  :actions (insert wrap) :unless '(sp-point-after-word-p))
                   (:open "("  :close ")"  :actions (insert wrap))
                   (:open "["  :close "]"  :actions (insert wrap))
                   (:open "{"  :close "}"  :actions (insert wrap))
                   (:open "`"  :close "`"  :actions (insert wrap))))))

(require 'smartparens)

(push 'sldb-mode sp-ignore-modes-list)

(sp-with-modes '(emacs-lisp-mode
                 inferior-emacs-lisp-mode
                 lisp-interaction-mode
                 common-lisp-mode)
  (sp-local-pair "'" nil :actions nil)
  (sp-local-pair "`" "'" :when '(sp-in-string-p) :actions '(insert wrap)))

(sp-with-modes '(text-mode)
  (sp-local-pair "`" "'" :actions '(insert wrap)))

(sp-with-modes '(tex-mode
                 plain-tex-mode
                 latex-mode)
  (sp-local-pair "$" "$")
  (sp-local-pair "\\[" "\\]")
  (sp-local-pair "\\(" "\\)")
  (sp-local-pair "\\left(" "\\right)")
  (sp-local-pair "\\left\\{" "\\right\\}"))

(sp-with-modes '(markdown-mode
                 rst-mode)
  (sp-local-pair "`" "`"))

(add-to-hooks #'smartparens-mode '(text-mode-hook prog-mode-hook))

(provide 'cofi-smartparens)
