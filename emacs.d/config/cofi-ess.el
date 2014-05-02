(setq ess-pdf-viewer-pref "kde-open")
(setq ess-ps-viewer-pref "kde-open")
(setq ess-ask-for-ess-directory nil)

(add-to-hooks #'eldoc-mode '(ess-mode-hook
                             inferior-ess-mode-hook))

(add-to-hooks #'smartparens-mode '(ess-mode-hook
                                   inferior-ess-mode-hook))

(add-to-hooks #'auto-complete-mode '(ess-mode-hook
                                     inferior-ess-mode-hook))

(provide 'cofi-ess)
