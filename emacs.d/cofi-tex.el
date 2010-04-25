(add-hook 'TeX-mode-hook 'auto-fill-mode)
(add-hook 'TeX-mode-hook 'flyspell-mode)

(setq TeX-auto-save t
      TeX-parse-self t)

(setq-default TeX-PDF-mode t)

(provide 'cofi-tex)
