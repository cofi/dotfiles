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
(setq sp-highlight-pair-overlay nil)

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

(fill-keymap sp-keymap
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
 "M-<down>" 'sp-splice-sexp-killing-backward
 "M-<up>"   'sp-splice-sexp-killing-forward)

(defun cofi/grab-closing-paren ()
  (interactive)
  (unless (or (cofi/pos-in-comment-p (point))
             (cofi/pos-in-string-p (point)))
    (let ((old-pos (point))
          (paren-pos (save-excursion (search-forward ")" nil t)
                                     (1- (point)))))
      (when (= (save-excursion
                 (skip-chars-forward " \t\n")
                 (point))
               paren-pos)
        (delete-region old-pos paren-pos)))))

(evil-define-key 'insert lisp-mode-shared-map
  (kbd "C-]") 'cofi/grab-closing-paren)

(provide 'cofi-smartparens)
