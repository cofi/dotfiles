(require 'semantic)
(require 'semantic/db-file)
(require 'semantic/idle)
(require 'semantic/ia)
(global-semantic-highlight-func-mode 1)
(setq semanticdb-default-save-directory (cofi/var-file "emacs/semanticdb"))

;;; idle

;;; decoration
(require 'semantic/decorate/mode)
(semantic-toggle-decoration-style "semantic-decoration-on-private-members" t)
(semantic-toggle-decoration-style "semantic-decoration-on-protected-members" t)

;;; tags
(dolist (mode '(c-mode c++-mode java-mode))
  (semanticdb-enable-gnu-global-databases mode))

(add-hook 'c-mode-common-hook #'semantic-mode)
(add-hook 'c-mode-common-hook (gen-local-fill-keymap-hook
                               "C-c c ?" 'semantic-documentation-for-tag
                               "C-c c t" 'semantic-analyze-proto-impl-toggle
                               "C-c c s" 'semantic-ia-show-summary
                               "C-c c g" 'semantic-ia-fast-jump
                               "C-c c j" 'semantic-complete-jump-local
                               "C-c c J" 'semantic-complete-jump
                               "C-c c c" 'semantic-ia-describe-class))

(provide 'cofi-cedet)
