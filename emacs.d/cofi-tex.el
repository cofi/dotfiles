(defun cofi-tex-latex-hook ()
  ;; deactivate auto-complete, flyspell seems to mess with it when first active
  (auto-complete-mode -1)

  (auto-fill-mode 1)
  (flyspell-mode 1)

  ;; reactivate modes
  (auto-complete-mode 1)
  )

(add-hook 'tex-mode-hook 'cofi-tex-latex-hook)
(add-hook 'latex-mode-hook 'cofi-tex-latex-hook)

(setq TeX-auto-save t
      TeX-parse-self t)

(setq-default TeX-PDF-mode t)

(provide 'cofi-tex)
