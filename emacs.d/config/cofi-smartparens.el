(add-to-loadpath "~/.elisp/vendor/smartparens/")
;;; replace default pairs
(setq sp-pairs '((t
                  .
                  ((:open "\"" :close "\"" :actions (insert wrap))
                   (:open "'"  :close "'"  :actions (insert wrap) :unless (sp-point-after-word-p))
                   (:open "("  :close ")"  :actions (insert wrap))
                   (:open "["  :close "]"  :actions (insert wrap))
                   (:open "{"  :close "}"  :actions (insert wrap))
                   (:open "`"  :close "`"  :actions (insert wrap))))))

(require 'smartparens)

(push 'sldb-mode sp-ignore-modes-list)
(setq sp-highlight-pair-overlay nil
      sp-navigate-close-if-unbalanced t)

(sp-with-modes '(emacs-lisp-mode
                 inferior-emacs-lisp-mode
                 lisp-interaction-mode
                 lisp-mode)
  (sp-local-pair "'" nil :actions nil)
  (sp-local-pair "`" "'" :when '(sp-in-string-p) :actions '(insert wrap)))

(sp-with-modes '(scala-mode)
  (sp-local-pair "'" nil :actions nil))

(sp-with-modes '(text-mode)
  (sp-local-pair "`" "'" :actions '(insert wrap)))

(sp-with-modes '(tex-mode
                 plain-tex-mode
                 latex-mode)
  (sp-local-pair "$" "$")
  (sp-local-pair "\\[" "\\]")
  (sp-local-pair "\\(" "\\)")
  (sp-local-pair "\\{" "\\}")
  (sp-local-pair "\\left(" "\\right)")
  (sp-local-pair "\\left\\{" "\\right\\}"))

(sp-with-modes '(markdown-mode
                 rst-mode)
  (sp-local-pair "`" "`"))

(add-to-hooks #'smartparens-mode '(text-mode-hook prog-mode-hook))

(fill-keymap sp-keymap
 "M-)"   'sp-up-sexp
 "C-)"   'sp-forward-slurp-sexp
 "C-M-f" 'sp-forward-sexp
 "C-M-b" 'sp-backward-sexp

 "C-M-d" 'sp-down-sexp
 "C-M-a" 'sp-backward-down-sexp

 "C-M-e" 'sp-up-sexp
 "C-M-u" 'sp-backward-up-sexp

 "C-M-n" 'sp-next-sexp
 "C-M-p" 'sp-previous-sexp

 "C-M-k" 'sp-kill-sexp

 "M-F" 'sp-forward-symbol
 "M-B" 'sp-backward-symbol

 "C-M-<backspace>" 'sp-backward-unwrap-sexp
 "C-M-<delete>"    'sp-unwrap-sexp

 "M-D"      'sp-splice-sexp
 "M-r"      'sp-splice-sexp-killing-around
 "M-<down>" 'sp-splice-sexp-killing-backward
 "M-<up>"   'sp-splice-sexp-killing-forward)

(provide 'cofi-smartparens)
