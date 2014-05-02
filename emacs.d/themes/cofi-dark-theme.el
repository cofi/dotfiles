;; -*- mode: emacs-lisp; mode: rainbow -*-

(deftheme cofi-dark
  "cofi theme.")

(let ((bg "#131113")
      (fg "#FFFFFF")
      (vc-added "green")
      (vc-removed "red")
      (vc-changed "blue")
      (vc-context "gray")
      )
    (custom-theme-set-faces
     'cofi-dark
     ;; basic faces
     `(default ((t (:background ,bg :foreground ,fg))))
     `(border ((t (:background "black"))))
     `(header-line ((t (:foreground ,fg :background ,bg :bold t))))
     `(highlight ((t (:background "sienna"))))
     `(fringe ((t (:background ,bg))))

     `(primary-selection ((t (:foreground "gold" :background "black"))))
     `(secondary-selection ((t (:foreground "black" :background "gold"))))

     `(minibuffer-prompt ((t (:foreground "orange" :bold t))))

     `(region ((t (:background "gray33"))))
     `(hl-line ((t (:background "#202020"))))
     `(tooltip ((t (:background "white" :foreground "black"))))

     ;; modeline
     `(mode-line ((t (:background "#2E2D2F" :foreground ,fg :height 80 :box nil))))
     `(mode-line-inactive ((t (:background ,fg :foreground "#000000" :height 80))))
     `(mode-line-buffer ((t (:bold t :foreground "#FFAA00"))))
     `(mode-line-major-mode ((t (:bold t :foreground "gold"))))
     `(mode-line-minor-mode ((t (:foreground "khaki"))))

     ;; diff faces
     `(diff-added-face ((t (:foreground ,vc-added))))
     `(diff-changed-face ((t (:foreground ,vc-changed))))
     `(diff-removed-face ((t (:foreground ,vc-removed))))
     `(diff-context-face ((t (:foreground ,vc-context))))
     `(diff-file-header-face ((t (:foreground "gold" :background unspecified))))
     `(diff-function-face ((t (:foreground "gray50" :background unspecified))))
     `(diff-header-face ((t (:foreground "gold" :background unspecified))))
     `(diff-hunk-header-face ((t (:foreground "gold"))))
     `(diff-index-face ((t (:bold t :background "gray70"))))
     `(diff-nonexistent-face ((t (:bold t :background "gray70"))))

     ;; ibuffer
     `(ibuffer-git-add-face ((t (:foreground ,vc-added))))
     `(ibuffer-git-del-face ((t (:foreground ,vc-removed))))

     ;; magit
     `(magit-diff-file-header ((t (:foreground "gold" :background unspecified))))
     `(magit-diff-hunk-header ((t (:foreground "gold"))))
     `(magit-diff-none ((t (:foreground ,vc-context))))
     `(magit-diff-add ((t (:foreground ,vc-added))))
     `(magit-diff-del ((t (:foreground ,vc-removed))))
     `(magit-item-highlight ((t (:inherit hl-line))))
     `(magit-log-head-label-local ((t (:foreground "LightSkyBlue4" :box 1))))
     `(magit-log-head-label-remote ((t (:foreground "OliveDrab4" :box 1))))

     `(monky-diff-add ((t (:foreground ,vc-added))))
     `(monky-diff-del ((t (:foreground ,vc-removed))))
     `(monky-diff-title ((t (:foreground "gold" :background ,bg))))
     `(monky-header ((t (:foreground "gold" :background unspecified))))

     ;; dired
     `(dired-boring ((t (:foreground "rosy brown"))))
     `(dired-directory ((t (:foreground "blue"))))
     `(dired-flagged ((t (:foreground "red" :bold t))))
     `(dired-marked ((t (:foreground "red" :bold t))))
     `(dired-symlink ((t (:foreground "purple"))))

     ;; ediff
     `(ediff-current-diff-face-A ((t (:background "pale green" :foreground "firebrick"))))
     `(ediff-current-diff-face-Ancestor ((t (:background "violet red" :foreground "black"))))
     `(ediff-current-diff-face-B ((t (:background "yellow" :foreground "dark orchid"))))
     `(ediff-current-diff-face-C ((t (:background "pink" :foreground "navy"))))
     `(ediff-even-diff-face-A ((t (:background "light grey" :foreground "black"))))
     `(ediff-even-diff-face-Ancestor ((t (:background "gray" :foreground "white"))))
     `(ediff-even-diff-face-B ((t (:background "gray" :foreground "white"))))
     `(ediff-even-diff-face-C ((t (:background "light grey" :foreground "black"))))
     `(ediff-fine-diff-face-A ((t (:background "sky blue" :foreground "navy"))))
     `(ediff-fine-diff-face-Ancestor ((t (:background "green" :foreground "black"))))
     `(ediff-fine-diff-face-B ((t (:background "cyan" :foreground "black"))))
     `(ediff-fine-diff-face-C ((t (:background "turquoise" :foreground "black"))))
     `(ediff-odd-diff-face-A ((t (:background "gray" :foreground "white"))))
     `(ediff-odd-diff-face-Ancestor ((t (:background "light gray" :foreground "black"))))
     `(ediff-odd-diff-face-B ((t (:background "light gray" :foreground "black"))))
     `(ediff-odd-diff-face-C ((t (:background "gray" :foreground "white"))))

     ;; font lock
     `(font-lock-builtin-face ((t (:foreground "#EEDCB5"))))
     `(font-lock-comment-face ((t (:foreground "burlywood"))))
     `(font-lock-constant-face ((t (:foreground "#806DC1"))))
     `(font-lock-doc-face ((t (:foreground "#FFDD00"))))
     `(font-lock-doc-string-face ((t (:foreground "#FFDD00"))))
     `(font-lock-function-name-face ((t (:foreground "#C7AF3F"))))
     `(font-lock-keyword-face ((t (:foreground "#D65921" :bold t))))
     `(font-lock-preprocessor-face ((t (:foreground "#60bdf4"))))
     `(font-lock-reference-face ((t (:foreground "violet red"))))
     `(font-lock-string-face ((t (:foreground "#8CBED6"))))
     `(font-lock-type-face ((t (:italic t :foreground "#95cc5e"))))
     `(font-lock-variable-name-face ((t (:foreground "#FD971F"))))
     `(font-lock-warning-face ((t (:bold t :foreground "red"))))

     `(sh-heredoc-face ((t (:foreground "tan"))))

     `(makefile-space-face ((t (:background "hot pink"))))

     ;; highlight changes
     `(highlight-changes-delete-face ((t (:foreground ,vc-removed :underline t))))
     `(highlight-changes-face ((t (:foreground ,vc-changed))))

     ;; calendar
     `(calendar-today-face ((t (:underline t))))
     `(holiday-face ((t (:foreground "gray100" :background "firebrick"))))

     ;; isearch
     `(isearch ((t (:background "magenta4" :foreground "light sky blue"))))
     `(isearch-lazy-highlight-face ((t (:background "#A8DDE0" :foreground ,bg))))

     ;; log view
     `(log-view-file-face ((t (:bold t :background "gray70" :weight bold))))
     `(log-view-message-face ((t (:foreground "sienna"))))

     ;; sml modeline
     `(sml-modeline-vis-face ((t (:foreground "white" :background "gray20"))))
     `(sml-modeline-end-face ((t (:foreground "white" :background "#556B9D"))))

     ;; show paren mode
     `(show-paren-match ((t (:background "#006E29"))))
     `(show-paren-mismatch ((t (:background "red" :foreground "white"))))

     ;; smerge
     `(smerge-base-face ((t (:foreground "red"))))
     `(smerge-markers-face ((t (:background "gray85"))))
     `(smerge-mine-face ((t (:foreground "blue"))))
     `(smerge-other-face ((t (:foreground "DarkOliveGreen4"))))

     ;; rst
     `(rst-level-1-face ((t (:bold t :foreground "#A4C0E4" :background unspecified))))
     `(rst-level-2-face ((t (:bold t :foreground "#80B3FF" :background unspecified))))
     `(rst-level-3-face ((t (:bold t :foreground "#6193CF" :background unspecified))))
     `(rst-level-4-face ((t (:bold t :foreground "#2C72C7" :background unspecified))))
     `(rst-level-5-face ((t (:bold t :foreground "#0066FF" :background unspecified))))
     `(rst-level-6-face ((t (:bold t :foreground "#0057AE" :background unspecified))))

     ;; markdown
     `(markdown-header-face   ((t (:bold t :foreground "#A4C0E4" :background unspecified))))
     `(markdown-header-face-1 ((t (:bold t :foreground "#A4C0E4" :background unspecified))))
     `(markdown-header-face-2 ((t (:bold t :foreground "#80B3FF" :background unspecified))))
     `(markdown-header-face-3 ((t (:bold t :foreground "#6193CF" :background unspecified))))
     `(markdown-header-face-4 ((t (:bold t :foreground "#2C72C7" :background unspecified))))
     `(markdown-header-face-5 ((t (:bold t :foreground "#0066FF" :background unspecified))))
     `(markdown-header-face-6 ((t (:bold t :foreground "#0057AE" :background unspecified))))

     ;; Org
     `(org-hide ((t (:foreground ,bg))))
     `(org-level-1 ((t (:bold t :foreground "#DEBC85"))))
     `(org-level-2 ((t (:bold t :foreground "#CEAC75"))))
     `(org-level-3 ((t (:bold t :foreground "#BE9C65"))))
     `(org-level-4 ((t (:bold t :foreground "#AE8C55"))))
     `(org-level-5 ((t (:bold t :foreground "#AA7D50"))))
     `(org-level-6 ((t (:bold t :foreground "#9C7045"))))
     `(org-level-7 ((t (:bold t :foreground "#906F40"))))
     `(org-level-8 ((t (:bold t :foreground "#8F6B32"))))
     `(org-date ((t (:underline t :foreground "sky blue"))))
     `(org-sexp-date ((t (:underline t :foreground "sky blue"))))
     `(org-link ((t (:underline t :foreground "royal blue"))))
     `(org-footnote ((t (:foreground "dark violet"))))
     `(org-special-keyword ((t (:bold t :foreground "medium orchid"))))
     `(org-todo ((t (:bold t :foreground "firebrick"))))
     `(org-done ((t (:foreground "OliveDrab4" :strike-through t))))
     `(org-agenda-done ((t (:foreground "OliveDrab4"))))
     `(org-agenda-structure ((t (:foreground "cornsilk4"))))
     `(org-agenda-date-weekend ((t (:foreground "brown"))))
     `(org-meta-line ((t (:foreground "tan4"))))

     ;; helm
     `(helm-header ((t (:foreground "gold" :background "black"))))
     `(helm-selection ((t (:background "gray15" :underline t))))

     ;; ido
     `(ido-first-match ((t (:foreground "#d0bf8f" :bold t))))
     `(ido-only-match ((t (:foreground "#dfaf8f"))))
     `(ido-subdir ((t (:foreground  "#8c5353"))))

     ;; erc
     `(erc-input-face ((t (:foreground ,fg))))
     `(erc-notice-face ((t (:foreground "royal blue"))))
     `(erc-current-nick-face ((t (:foreground "orange" :bold t))))
     `(erc-prompt-face ((t (:foreground "orange" :bold t :background unspecified))))
     `(erc-timestamp-face ((t (:foreground "lawn green"))))

     ;; workgroups
     `(wg-mode-line-face ((t (:foreground "light sky blue"))))
     `(wg-brace-face ((t (:foreground "light slate blue"))))
     `(wg-divider-face ((t (:foreground "light slate blue"))))
     `(wg-filename-face ((t (:foreground "light sky blue"))))
     `(wg-command-face ((t (:foreground "aquamarine" :bold t))))
     `(wg-frame-face ((t (:foreground ,fg))))

     ;; flyspell
     `(flyspell-duplicate ((t (:underline "gold3"))))
     `(flyspell-incorrect ((t (:underline "OrangeRed"))))

     ;; ace-jump
     `(ace-jump-face-foreground ((t (:foreground "orange"))))
     `(ace-jump-face-background ((t (:foreground "#4e4d4f"))))

     ;; misc
     `(which-func ((t (:foreground "gold"))))

     `(linum ((t (:background "#2E2E37" :foreground "#D0D0A0"))))

     `(trailing-whitespace ((t (:background "red"))))

     `(pp^L-highlight ((t (:box (:line-width 1 :color "black" :style nil)))))
     ))

(provide-theme 'cofi-dark)
