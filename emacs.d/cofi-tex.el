(add-hook 'TeX-mode-hook (lambda ()
                           (setq fill-column 80)
                           (auto-fill-mode 1)))
(add-hook 'TeX-mode-hook 'speck-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

(eval-after-load "cdlatex"
  '(progn
     (remove-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
     (define-key cdlatex-mode-map "\t" nil)
     (define-key cdlatex-mode-map (kbd "C-<tab>") 'cdlatex-tab)))

(when (locate-library "cdlatex")
  (add-hook 'LaTeX-mode-hook (lambda () (setq autopair-dont-activate t)))
  (add-hook 'LaTeX-mode-hook 'cdlatex-mode)
)

;; (add-hook 'TeX-language-de-hook (lambda ()
;;                                   (ispell-change-dictionary "de_DE")))
(setq TeX-output-view-style '(
                              ("^pdf$" "." "okular %o")
                              ("^dvi$" "." "okular %o")
                              ))
(setq TeX-auto-save t
      TeX-parse-self t
      TeX-PDF-mode t)

(provide 'cofi-tex)
