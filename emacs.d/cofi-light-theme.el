;; -*- mode: emacs-lisp; mode: rainbow -*-

(deftheme cofi-light
  "cofi theme.")

(let ((bg "#fdf6e3")
      (fg "#002b36")
      (vc-added "green4")
      (vc-removed "red4")
      (vc-changed "blue4")
      (vc-context "gray")
      (header-color "gold4")
      )
    (custom-theme-set-faces
     'cofi-light
     `(default ((((min-colors 4096)) (:background ,bg :foreground ,fg))))

     `(italic ((t (:italic t :family "Dejavu Sans Mono" :height 80))))
     `(border ((t (:background "black"))))

     `(highlight ((t (:background "#c0d0e0"))))

     `(change-log-acknowledgement-face ((t (:foreground "firebrick"))))
     `(change-log-conditionals-face ((t (:foreground "dark goldenrod"))))
     `(change-log-date-face ((t (:foreground "rosy brown"))))
     `(change-log-email-face ((t (:foreground "dark goldenrod"))))
     `(change-log-file-face ((t (:foreground "blue"))))
     `(change-log-function-face ((t (:foreground "dark goldenrod"))))
     `(change-log-list-face ((t (:foreground "purple"))))
     `(change-log-name-face ((t (:foreground "cadet blue"))))
     `(comint-highlight-input ((t (:bold t :weight bold))))
     `(comint-highlight-prompt ((t (:foreground "dark blue"))))

     `(diff-added-face ((t (:foreground ,vc-added))))
     `(diff-changed-face ((t (:foreground ,vc-changed))))
     `(diff-removed-face ((t (:foreground ,vc-removed))))
     `(diff-context-face ((t (:foreground ,vc-context))))
     `(diff-file-header-face ((t (:foreground ,header-color :background ,bg))))
     `(diff-function-face ((t (:foreground "gray50" :background ,bg))))
     `(diff-header-face ((t (:foreground ,header-color :background nil))))
     `(diff-hunk-header-face ((t (:foreground ,header-color))))
     `(diff-index-face ((t (:bold t :weight bold :background "gray70"))))
     `(diff-nonexistent-face ((t (:bold t :weight bold :background "gray70"))))

     `(ibuffer-git-add-face ((t (:foreground ,vc-added))))
     `(ibuffer-git-del-face ((t (:foreground ,vc-removed))))

     `(magit-diff-file-header ((t (:foreground ,header-color :background ,bg))))
     `(magit-diff-hunk-header ((t (:foreground ,header-color))))
     `(magit-diff-none ((t (:foreground ,vc-context))))
     `(magit-diff-add ((t (:foreground ,vc-added))))
     `(magit-diff-del ((t (:foreground ,vc-removed))))

     `(dired-boring ((t (:foreground "rosy brown"))))
     `(dired-directory ((t (:foreground "blue"))))
     `(dired-flagged ((t (:foreground "red" :weight bold))))
     `(dired-marked ((t (:foreground "red" :weight bold))))
     `(dired-symlink ((t (:foreground "purple"))))

     `(diredp-flag-mark-line ((t (:background "cornsilk2" :foreground "blue violet"))))
     `(diredp-read-priv ((t (:background ,bg :foreground "green"))))
     `(diredp-write-priv ((t (:background ,bg :foreground "firebrick"))))
     `(diredp-exec-priv ((t (:background ,bg :foreground "light grey"))))
     `(diredp-no-priv ((t (:background ,bg :foreground "white"))))
     `(diredp-dir-priv ((t (:foreground "sky blue"))))

     `(diredp-file-name ((t (:foreground "slate blue"))))
     `(diredp-file-suffix ((t (:foreground "light grey"))))
     `(diredp-compressed-file-suffix ((t (:foreground "gold"))))
     `(diredp-inode+size ((t (:foreground "cyan"))))
     `(diredp-date-time ((t (:foreground "navy"))))
     `(diredp-dir-heading ((t (:background ,bg :foreground "deep sky blue"))))

     `(ediff-current-diff-face-A ((t (:background "pale green" :foreground "firebrick"))))
     `(ediff-current-diff-face-Ancestor ((t (:background "violet red" :foreground "black"))))
     `(ediff-current-diff-face-B ((t (:background "#b58900" :foreground "dark orchid"))))
     `(ediff-current-diff-face-C ((t (:background "pink" :foreground "navy"))))
     `(ediff-even-diff-face-A ((t (:background "light grey" :foreground "black"))))
     `(ediff-even-diff-face-Ancestor ((t (:background "gray" :foreground "white"))))
     `(ediff-even-diff-face-B ((t (:background "gray" :foreground "white"))))
     `(ediff-even-diff-face-C ((t (:background "light grey" :foreground "black"))))
     `(ediff-fine-diff-face-A ((t (:background "sky blue" :foreground "navy"))))
     `(ediff-fine-diff-face-Ancestor ((t (:background "green" :foreground "black"))))
     `(ediff-fine-diff-face-B ((t (:background "cyan" :foreground "black"))))
     `(ediff-fine-diff-face-C ((t (:background "turquoise" :foreground "black"))))
     `(ediff-odd-diff-face-A ((t (:background "gray" :foreground "White"))))
     `(ediff-odd-diff-face-Ancestor ((t (:background "light grey" :foreground "black"))))
     `(ediff-odd-diff-face-B ((t (:background "light grey" :foreground "black"))))
     `(ediff-odd-diff-face-C ((t (:background "gray" :foreground "white"))))

     `(font-lock-builtin-face ((t (:foreground "#dc322f"))))
     `(font-lock-comment-face ((t (:inherit italic :foreground "#586e75"))))
     `(font-lock-constant-face ((t (:foreground "#859900"))))
     `(font-lock-doc-face ((t (:foreground "#b58900"))))
     `(font-lock-function-name-face ((t (:foreground "#268bd2"))))
     `(font-lock-keyword-face ((t (:foreground "#dc322f"))))
     `(font-lock-preprocessor-face ((t (:foreground "#d30102"))))
     `(font-lock-string-face ((t (:foreground "firebrick"))))
     `(font-lock-type-face ((t (:italic t :foreground "#cb4b16"))))
     `(font-lock-variable-name-face ((t (:foreground "#2aa198"))))
     `(font-lock-warning-face ((t (:bold t :foreground "red" :weight bold))))

     `(fringe ((t (:background ,bg))))

     `(highlight-changes-delete-face ((t (:foreground "red" :underline t))))
     `(highlight-changes-face ((t (:foreground "red"))))

     `(calendar-today-face ((t (:underline t))))
     `(holiday-face ((t (:foreground "gray100" :background "firebrick"))))

     `(isearch ((t (:background "magenta4" :foreground "white"))))
     `(isearch-lazy-highlight-face ((t (:background "pale turquoise"))))

     `(log-view-file-face ((t (:bold t :background "gray70" :weight bold))))
     `(log-view-message-face ((t (:foreground "sienna"))))

     `(makefile-space-face ((t (:background "hot pink"))))

     `(mode-line ((t (:background "#8089a0" :foreground "white"))))
     `(mode-line-inactive ((t (:background "gray80" :foreground "#8089a0"))))
     `(mode-line-buffer-id ((t (:foreground "black" :background "#8089a0"))))
     `(sml-modeline-vis-face ((t (:foreground "white" :background "gray20"))))
     `(sml-modeline-end-face ((t (:foreground "white" :background "#334B7D"))))

     `(which-func ((t (:foreground ,header-color))))

     `(primary-selection ((t (:foreground "gold" :background "black"))))
     `(secondary-selection ((t (:foreground "black" :background "gold"))))

     `(region ((t (:background "#586e75"))))
     `(hl-line ((t (:background "#c0d0e0"))))

     `(sh-heredoc-face ((t (:foreground "tan"))))

     `(show-paren-match-face ((t (:background "forest green"))))
     `(show-paren-mismatch-face ((t (:background "red" :foreground "white"))))

     `(smerge-base-face ((t (:foreground "red"))))
     `(smerge-markers-face ((t (:background "gray85"))))
     `(smerge-mine-face ((t (:foreground "blue"))))
     `(smerge-other-face ((t (:foreground "DarkOliveGreen4"))))

     `(speedbar-button-face ((t (:foreground "green4"))))
     `(speedbar-directory-face ((t (:foreground "blue4"))))
     `(speedbar-file-face ((t (:foreground "cyan4"))))
     `(speedbar-highlight-face ((t (:background "green"))))
     `(speedbar-selected-face ((t (:foreground "red" :underline t))))
     `(speedbar-tag-face ((t (:foreground "brown"))))

     `(tooltip ((t (:background ,bg :foreground ,fg))))
     `(trailing-whitespace ((t (:background "red" :foreground "yellow"))))

     `(linum ((t (:foreground "#000000" :background "#c0d0e0"))))

     `(minibuffer-prompt ((t (:foreground "firebrick" :bold t))))

     `(viper-minibuffer-insert ((t (:background ,bg :foreground ,fg))))
     `(viper-minibuffer-emacs ((t (:background ,bg :foreground ,fg))))
     `(viper-minibuffer-vi ((t (:background ,bg :foreground ,fg))))

     `(rst-level-1-face ((t (:bold t :foreground "chocolate4" :background ,bg))))
     `(rst-level-2-face ((t (:bold t :foreground "chocolate3" :background ,bg))))
     `(rst-level-3-face ((t (:bold t :foreground "chocolate2" :background ,bg))))
     `(rst-level-4-face ((t (:bold t :foreground "orange3" :background ,bg))))
     `(rst-level-5-face ((t (:bold t :foreground "dark orange" :background ,bg))))
     `(rst-level-6-face ((t (:bold t :foreground "dark khaki" :background ,bg))))

     `(org-hide ((t (:foreground ,bg))))
     `(org-level-1 ((t (:bold t :foreground "chocolate4"))))
     `(org-level-2 ((t (:bold t :foreground "chocolate3"))))
     `(org-level-3 ((t (:bold t :foreground "chocolate2"))))
     `(org-level-4 ((t (:bold t :foreground "orange3"))))
     `(org-level-5 ((t (:bold t :foreground "dark orange"))))
     `(org-level-6 ((t (:bold t :foreground "dark khaki"))))
     `(org-level-7 ((t (:bold t :foreground "khaki"))))
     `(org-level-8 ((t (:bold t :foreground "khaki1"))))
     `(org-date ((t (:underline t :foreground "forest green"))))
     `(org-sexp-date ((t (:underline t :foreground "forest green"))))
     `(org-link ((t (:underline t :foreground "blue4"))))
     `(org-footnote ((t (:foreground "dark violet"))))
     `(org-special-keyword ((t (:bold t :foreground "medium orchid"))))
     `(org-todo ((t (:blod t :foreground "firebrick"))))
     `(org-done ((t (:foreground "OliveDrab4" :strike-through t))))
     `(org-agenda-done ((t (:foreground "OliveDrab4"))))
     `(org-agenda-structure ((t (:foreground "cornsilk4"))))
     `(org-agenda-date-weekend ((t (:foreground "brown"))))
     `(org-meta-line ((t (:foreground "tan4"))))

     `(anything-header ((t (:foreground "gold" :background "black"))))
     ))

(provide-theme 'cofi-light)
