(defun cofi/latex-build-and-view (&optional only-build)
  (interactive "P")
  (let ((name (TeX-active-master))
        (fstring (if TeX-PDF-mode
                     "xdg-open %s.pdf"
                   "xdg-open %s.dvi")))
    (and (TeX-command "LaTeX" 'TeX-master-file)
       ;; Build twice to get references right
       (TeX-command "LaTeX" 'TeX-master-file)
       (not only-build)
       (shell-command (format fstring name)))))

(add-hook 'TeX-mode-hook (lambda ()
                           (setq fill-column 80)
                           (auto-fill-mode 1)))

(when (locate-library "cdlatex")
  (add-hook 'LaTeX-mode-hook (lambda ()
                               (setq autopair-dont-activate t)
                               (local-set-key (kbd "<f12>") 'cofi/latex-build-and-view))))

(add-hook 'LaTeX-mode-hook (if (locate-library "cdlatex")
                              'cdlatex-mode
                             'LaTeX-math-mode))

(setq TeX-view-program-selection '((output-dvi "xdg-open")
                                   (output-pdf "xdg-open")
                                   (output-html "xdg-open")))

(eval-after-load "cdlatex"
  '(progn
     (define-key cdlatex-mode-map "\t" nil)
     (define-key cdlatex-mode-map (kbd "C-<tab>") 'cdlatex-tab)))

(setq TeX-auto-save t
      TeX-parse-self t
      TeX-PDF-mode t)

(provide 'cofi-tex)
