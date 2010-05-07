(add-hook 'TeX-mode-hook (lambda ()
                           (setq fill-column 80)
                           (auto-fill-mode 1)))
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

(add-hook 'TeX-language-de-hook (lambda ()
                                  (ispell-change-dictionary "de_DE-neu")))


(setq TeX-auto-save t
      TeX-parse-self t)

(setq-default TeX-PDF-mode t)

(provide 'cofi-tex)
