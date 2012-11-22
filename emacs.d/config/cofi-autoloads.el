(autoload 'any-ini-mode "any-ini-mode")
(add-to-list 'auto-mode-alist '("\\.ini\\'" . any-ini-mode))
(add-to-list 'auto-mode-alist '("\\.conf\\'" . any-ini-mode))

(autoload 'bc-set               "breadcrumb" "Set bookmark in current point."   t)
(autoload 'bc-previous          "breadcrumb" "Go to previous bookmark."         t)
(autoload 'bc-next              "breadcrumb" "Go to next bookmark."             t)
(autoload 'bc-local-previous    "breadcrumb" "Go to previous local bookmark."   t)
(autoload 'bc-local-next        "breadcrumb" "Go to next local bookmark."       t)
(autoload 'bc-goto-current      "breadcrumb" "Go to the current bookmark."      t)
(autoload 'bc-list              "breadcrumb" "List all bookmarks in menu mode." t)
(autoload 'bc-clear             "breadcrumb" "Clear all bookmarks."             t)

(autoload 'cfw:open-org-calendar "calfw-org" nil t)

(autoload 'clojure-mode "clojure-mode.el" "clojure mode" t)
(autoload 'clojure-test-mode "clojure-test-mode.el" "clojure test minor mode" t)

(autoload 'cdlatex-mode "cdlatex" "CDLaTeX Mode" t)

(autoload 'clipper-create "clipper" "Create a new clip." t)
(autoload 'clipper-delete "clipper" "Delete an existing clip." t)
(autoload 'clipper-insert "clipper" "Insert a clip into the current buffer." t)
(autoload 'clipper-edit-clip "clipper" "Edit an existing clip." t)

(autoload 'dired-jump "dired" "Jump to current buffer's file in dired" t)

(autoload 'ebib "ebib" nil t)

(autoload 'goto-last-change "goto-chg" nil t)

(autoload 'monky-status "monky" nil t)

(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(autoload 'multi-term-dedicated-open "multi-term" nil t)
(autoload 'multi-term-next "multi-term" nil t)

(autoload 'point-stack-push "point-stack" nil t)
(autoload 'point-stack-pop "point-stack" nil t)
(autoload 'point-stack-forward-stack-pop "point-stack" nil t)

(autoload 'try-pysmell-complete "pysmell" "PySmell-Hippie-Completer" t)

(autoload 'pylookup-lookup "pylookup"
  "Lookup SEARCH-TERM in the Python HTML indexes." t)

(autoload 'pylookup-update "pylookup"
  "Run pylookup-update and create the database at `pylookup-db-file'." t)

(autoload 'rainbow-mode "rainbow-mode" "Highlight color names in buffer" t)

(add-to-list 'auto-mode-alist '("/rfc[0-9]+\\.txt\\(\\.gz\\)?\\'" . rfcview-mode))
(autoload 'rfcview-mode "rfcview" nil t)

(autoload 'trivial-cite "tc"
  "A simple citation function for use in news/mailreaders." t)

(autoload 'yaml-mode "yaml-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-mode))
;;; additional org

(autoload 'org-agenda-files "org")
(autoload 'org-todo "org")

(provide 'cofi-autoloads)
