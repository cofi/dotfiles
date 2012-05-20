;; Enable EDE (Project Management) features
(global-ede-mode 1)

(require 'cedet-autogen)

(setq semanticdb-default-save-directory (cofi/var-file "emacs/semanticdb"))
(semantic-load-enable-minimum-features)
(semantic-load-enable-code-helpers)

;;; idle
(setq semantic-idle-scheduler-idle-time 1)
(global-semantic-idle-completions-mode 1)
(setq semantic-idle-work-parse-neighboring-files-flag t)

;;; decoration
(global-semantic-decoration-mode 1)
(require 'semantic-decorate-include)
(global-semantic-idle-local-symbol-highlight-mode 1)
(semantic-toggle-decoration-style "semantic-decoration-on-private-members" t)
(semantic-toggle-decoration-style "semantic-decoration-on-protected-members" t)

(setq semantic-idle-breadcrumbs-format-tag-list-function #'semantic-idle-breadcrumbs--format-innermost-first)
(global-semantic-idle-breadcrumbs-mode 1)

(global-srecode-minor-mode 1)

(cogre-uml-enable-unicode)

;;; tags
(require 'semanticdb-global)
(dolist (mode '(c-mode c++-mode java-mode))
  (semanticdb-enable-gnu-global-databases mode))
(semantic-load-enable-primary-exuberent-ctags-support)

(add-hook 'c-mode-common-hook (gen-local-fill-keymap-hook
                               "C-c c ?" 'semantic-documentation-for-tag
                               "C-c c t" 'semantic-analyze-proto-impl-toggle
                               "C-c c s" 'semantic-ia-show-summary
                               "C-c c c" 'semantic-ia-describe-class))

(provide 'cofi-cedet)
