(defun cofi/latex-build ()
  (interactive)
  (let ((fname (TeX-active-master t))
        (builder (lambda ()
                   (if (executable-find "latexmk")
                       (format "latexmk -pdf %s" fname)
                     (format "pdflatex %s && pdflatex %s" fname fname)))))
    (compile (funcall builder))))

(defun cofi/latex-view ()
  (interactive)
  (let ((name (TeX-active-master))
        (opener "xdg-open %s.pdf"))
    (shell-command (format opener name))))

(add-hook 'TeX-mode-hook (lambda ()
                           (setq fill-column 80)
                           (auto-fill-mode 1)))

(when (locate-library "cdlatex")
  (add-hook 'LaTeX-mode-hook (lambda () (setq autopair-dont-activate t)))
  (add-hook 'LaTeX-mode-hook
            (gen-local-fill-keymap-hook
                                  "<f12>" 'cofi/latex-build
                                  "C-<f12>" 'cofi/latex-view)))

(defun turn-on-reftex () (reftex-mode 1))

(setq reftex-plug-into-AUCTeX t)
(add-hook 'LaTeX-mode-hook (if (locate-library "cdlatex")
                              'cdlatex-mode
                             'LaTeX-math-mode))
(add-hook 'LaTeX-mode-hook #'turn-on-reftex)

(setq TeX-auto-save t
      TeX-save-query nil
      TeX-parse-self t
      TeX-newline-function #'reindent-then-newline-and-indent
      TeX-PDF-mode t)

(eval-after-load "cdlatex"
  '(progn
     (define-key cdlatex-mode-map "\t" nil)
     (define-key cdlatex-mode-map (kbd "C-<tab>") 'cdlatex-tab)))

(setq cdlatex-paired-parens "$[({")

(setq cdlatex-math-modify-alist
      '(
        (?B "\\mathbb" nil t nil nil)
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

(defun cofi/german-quotes ()
  (interactive)
  (setq TeX-open-quote "\"`")
  (setq TeX-close-quote "\"'"))

(defun cofi/english-quotes ()
  (interactive)
  (setq TeX-open-quote "``")
  (setq TeX-close-quote "''"))

(provide 'cofi-tex)
