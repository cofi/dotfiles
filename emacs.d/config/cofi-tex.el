(eval-after-load "tex"
  '(push '("latexmk" "latexmk -pdf %t" TeX-run-TeX nil t :help "Run Latexmk on file")
         TeX-command-list))

(add-hook 'LaTeX-mode-hook
          (gen-local-fill-keymap-hook
              "<f12>" (cmd-arg (override-confirm) "P"
                        (TeX-command "latexmk" 'TeX-master-file override-confirm))
              "C-<f12>" 'TeX-view))

(setq reftex-plug-into-AUCTeX t)
(setq reftex-bibliography-commands '("addbibresource" "bibliography"))
(setq reftex-cite-prompt-optional-args t)
(setq reftex-cite-format '((?t . "\\textcite[]{%l}")
                           (?a . "\\autocite[]{%l}")
                           (?A . "\\citeauthor[]{%l}")
                           (?c . "\\cite[]{%l}")
                           (?s . "\\smartcite[]{%l}")
                           (?f . "\\footcite[]{%l}")
                           (?n . "\\nocite{%l}")
                           (?b . "\\blockcquote[]{%l}{}")))

(eval-after-load 'reftex-vars
  '(setcdr (assoc 'caption reftex-default-context-regexps) "\\\\\\(rot\\|sub\\)?caption\\*?[[{]"))
(eval-after-load 'reftex
  '(progn
     (define-key reftex-mode-map (kbd "C-c -") nil)))

(add-hook 'LaTeX-mode-hook #'reftex-mode)

(add-hook 'LaTeX-mode-hook (if (locate-library "cdlatex")
                              'cdlatex-mode
                             'LaTeX-math-mode))

(setq TeX-auto-save t
      TeX-save-query nil
      TeX-parse-self t
      TeX-newline-function #'reindent-then-newline-and-indent
      TeX-PDF-mode t)
(setq-default TeX-master 'dwim)

(eval-after-load "cdlatex"
  '(progn
     (define-key cdlatex-mode-map "\t" nil)
     (define-key cdlatex-mode-map (kbd "C-<tab>") 'cdlatex-tab)))

(setq cdlatex-paired-parens "")

(setq cdlatex-math-modify-alist
      '(
        (?B "\\mathbb" nil t nil nil)
        (?t "\\text" nil t nil nil)
        ))
(setq cdlatex-math-symbol-alist
      '(
        (?0 ("\\emptyset" "\\varnothing"))
        (?1 ("_1"))
        (?2 ("_2"))
        (?3 ("_3"))
        (?! ("\\neg" "\\not"))
        (?. ("\\cdot" "\\ldots"))
        ))

(add-all-to-hook 'LaTeX-mode-hook
                 (gen-local-fill-keymap-hook "." (cmd (insert ".\n")))
                 #'visual-line-mode)

(require-and-exec 'info-look
  (info-lookup-add-help
   :mode 'latex-mode
   :regexp ".*"
   :parse-rule "\\\\?[a-zA-Z]+\\|\\\\[^a-zA-Z]"
   :doc-spec '(("(latex2e)Concept Index")
               ("(latex2e)Command Index"))))


(provide 'cofi-tex)
