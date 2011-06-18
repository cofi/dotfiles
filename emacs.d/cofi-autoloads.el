(autoload 'ack "full-ack" "Run ack." t)
(autoload 'ack-same "full-ack" "Run ack in files matching the current major mode." t)

(autoload 'all "all" "Edit all matching lines" t)

(autoload 'adict-change-dictionary "auto-dictionary"
  "Set buffer language to LANG and stop detecting it automatically." t)
(autoload 'adict-guess-dictionary "auto-dictionary"
  "Automatically change ispell dictionary based on buffer language." t)
(autoload 'auto-dictionary-mode "auto-dictionary"
  "A minor mode that automatically sets `ispell-dictionary`." t)

(autoload 'bison-mode "bison-mode" nil t)
(add-to-list 'auto-mode-alist '(".yp\{2\}?$" . bison-mode))

(autoload 'boxquote-defun "boxquote" "Boxquote the current defun" t)
(autoload 'boxquote-insert-buffer "boxquote" "Insert & boxquote a buffer" t)
(autoload 'boxquote-insert-file "boxquote" "Insert & boxquote a file" t)
(autoload 'boxquote-paragraph "boxquote" "Boxquote the current paragraph" t)
(autoload 'boxquote-region "boxquote" "Boxquote the current region" t)
(autoload 'boxquote-shell-command "boxquote"
  "Insert & boxquote output of shell command" t)
(autoload 'boxquote-unbox "boxquote" "Remove boxquote that surrounds point" t)

(autoload 'bc-set               "breadcrumb" "Set bookmark in current point."   t)
(autoload 'bc-previous          "breadcrumb" "Go to previous bookmark."         t)
(autoload 'bc-next              "breadcrumb" "Go to next bookmark."             t)
(autoload 'bc-local-previous    "breadcrumb" "Go to previous local bookmark."   t)
(autoload 'bc-local-next        "breadcrumb" "Go to next local bookmark."       t)
(autoload 'bc-goto-current      "breadcrumb" "Go to the current bookmark."      t)
(autoload 'bc-list              "breadcrumb" "List all bookmarks in menu mode." t)
(autoload 'bc-clear             "breadcrumb" "Clear all bookmarks."             t)

(autoload 'clojure-mode "clojure-mode.el" "clojure mode" t)
(autoload 'clojure-test-mode "clojure-test-mode.el" "clojure test minor mode" t)

(autoload 'cdlatex-mode "cdlatex" "CDLaTeX Mode" t)
(autoload 'turn-on-cdlatex "cdlatex" "CDLaTeX Mode" nil)

(autoload 'clipper-create "clipper" "Create a new clip." t)
(autoload 'clipper-delete "clipper" "Delete an existing clip." t)
(autoload 'clipper-insert "clipper" "Insert a clip into the current buffer." t)
(autoload 'clipper-edit-clip "clipper" "Edit an existing clip." t)

(autoload 'dedicated-mode "dedicated" "Dedicate currect buffer." t)

(autoload 'dired-jump "dired" "Jump to current buffer's file in dired" t)

(autoload 'flex-mode "flex-mode" nil t)
(add-to-list 'auto-mode-alist '(".f?lex$" . flex-mode))

(autoload 'home-end-end "home-end" "Go to end of line/window/buffer." t)
(autoload 'home-end-home "home-end" "Go to beginning of line/window/buffer." t)

(autoload 'idomenu "idomenu" "Switch to a buffer-local tag from Imenu via Ido." t)

(autoload 'magit-status "magit" nil t)

(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(autoload 'markdown-mode "markdown-mode" "Mode for markdown files" t)

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

(autoload 'smex "smex" "A better M-x" t)

(autoload 'speck-mode "speck" "Auto spellchecking mode." t)
(autoload 'speck-buffer "speck" "Toggle `speck-mode' for current buffer." t)

(autoload 'trivial-cite "tc"
  "A simple citation function for use in news/mailreaders." t)

(autoload 'wl "wl" "Wanderlust" t)
(autoload 'wl-draft "wl-draft" "Write draft with Wanderlust." t)
(autoload 'wl-other-frame "wl" "Wanderlust on new frame." t)
(autoload 'wl-user-agent-compose "wl-draft" "Compose mail with Wanderlust" t)

(autoload 'gist-region-or-buffer "gist" "Post either the current region, or if
mark is not set, the current buffer as a new paste." t)
(autoload 'gist-region-or-buffer-private "gist" "Post either the current region,
or if mark is not set, the current buffer as a new private paste." t)
(autoload 'gist-list "gist" "Displays a list of all of the current user's
gists." t)
(autoload 'gist-fetch "gist" "Fetches a Gist and inserts it into a new buffer"
  t)

(autoload 'offlineimap "offlineimap" nil t)

(autoload 'scratch "scratch" nil t)

(provide 'cofi-autoloads)
