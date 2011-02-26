(defun cofi/latex-build ()
  (interactive)
  (let ((name (TeX-active-master))
        (fname (TeX-active-master t)))
    (compile (format "pdflatex %s && pdflatex %s" fname fname))))

(defun cofi/latex-view ()
  (interactive)
  (let ((name (TeX-active-master))
        (opener "xdg-open %s.pdf"))
    (shell-command (format opener name))))

(add-hook 'TeX-mode-hook (lambda ()
                           (setq fill-column 80)
                           (auto-fill-mode 1)))

(when (locate-library "cdlatex")
  (add-hook 'LaTeX-mode-hook (lambda ()
                               (setq autopair-dont-activate t)
                               (local-set-key (kbd "<f12>") 'cofi/latex-build)
                               (local-set-key (kbd "C-<f12>") 'cofi/latex-view))))

(add-hook 'LaTeX-mode-hook (if (locate-library "cdlatex")
                              'cdlatex-mode
                             'LaTeX-math-mode))

(eval-after-load "cdlatex"
  '(progn
     (define-key cdlatex-mode-map "\t" nil)
     (define-key cdlatex-mode-map (kbd "C-<tab>") 'cdlatex-tab)))

(setq TeX-auto-save t
      TeX-save-query nil
      TeX-parse-self t
      TeX-PDF-mode t)

(defun cofi/german-quotes ()
  (interactive)
  (setq TeX-open-quote "\"`")
  (setq TeX-close-quote "\"'"))

(defun cofi/english-quotes ()
  (interactive)
  (setq TeX-open-quote "``")
  (setq TeX-close-quote "''"))

(provide 'cofi-tex)
