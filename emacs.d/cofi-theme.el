;; -*- mode: emacs-lisp; mode: rainbow -*-

(deftheme cofi
  "cofi theme.")

(let ((bg "#202020")
      (fg "#FFFFFF")
      (vc-added "green")
      (vc-removed "red")
      (vc-changed "blue")
      (vc-context "gray")
      )
    (custom-theme-set-faces
     'cofi
     `(default ((((min-colors 4096)) (:background ,bg :foreground ,fg))))

     `(italic ((t (:italic t :family "Dejavu Sans Mono" :height 80))))
     `(border ((t (:background "black"))))

     `(highlight ((t (:background "sienna"))))

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

     `(cursor ((t (:background "OrangeRed2"))))

     `(diff-added-face ((t (:foreground ,vc-added))))
     `(diff-changed-face ((t (:foreground ,vc-changed))))
     `(diff-removed-face ((t (:foreground ,vc-removed))))
     `(diff-context-face ((t (:foreground ,vc-context))))
     `(diff-file-header-face ((t (:foreground "gold" :background ,bg))))
     `(diff-function-face ((t (:foreground "gray50" :background ,bg))))
     `(diff-header-face ((t (:foreground "gold" :background nil))))
     `(diff-hunk-header-face ((t (:foreground "gold"))))
     `(diff-index-face ((t (:bold t :weight bold :background "gray70"))))
     `(diff-nonexistent-face ((t (:bold t :weight bold :background "gray70"))))

     `(ibuffer-git-add-face ((t (:foreground "green"))))
     `(ibuffer-git-del-face ((t (:foreground "red"))))

     `(magit-diff-file-header ((t (:foreground "gold" :background ,bg))))
     `(magit-diff-hunk-header ((t (:foreground "gold"))))
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
     `(ediff-odd-diff-face-A ((t (:background "gray" :foreground "White"))))
     `(ediff-odd-diff-face-Ancestor ((t (:background "light grey" :foreground "black"))))
     `(ediff-odd-diff-face-B ((t (:background "light grey" :foreground "black"))))
     `(ediff-odd-diff-face-C ((t (:background "gray" :foreground "white"))))
     
     `(font-lock-builtin-face ((t (:foreground "LightSalmon4"))))
     `(font-lock-comment-face ((t (:inherit italic :foreground "burlywood"))))
     `(font-lock-constant-face ((t (:foreground "firebrick"))))
     `(font-lock-doc-face ((t (:foreground "yellow"))))
     `(font-lock-doc-string-face ((t (:foreground "yellow"))))
     `(font-lock-function-name-face ((t (:foreground "steel blue"))))
     `(font-lock-keyword-face ((t (:foreground "tomato2"))))
     `(font-lock-preprocessor-face ((t (:foreground "cadet blue"))))
     `(font-lock-reference-face ((t (:foreground "violet red"))))
     `(font-lock-string-face ((t (:foreground "sienna1"))))
     `(font-lock-type-face ((t (:italic t :foreground "forest green"))))
     `(font-lock-variable-name-face ((t (:foreground "gold"))))
     `(font-lock-warning-face ((t (:bold t :foreground "red" :weight bold))))
     
     `(fringe ((t (:background "gray17"))))
     
     `(highlight-changes-delete-face ((t (:foreground "red" :underline t))))
     `(highlight-changes-face ((t (:foreground "red"))))
     
     `(calendar-today-face ((t (:underline t))))
     `(holiday-face ((t (:foreground "gray100" :background "firebrick"))))
     
     `(info-header-node ((t (:italic t :bold t :weight bold :slant italic :foreground "brown"))))
     `(info-header-xref ((t (:bold t :weight bold :foreground "magenta4"))))
     `(info-menu-5 ((t (:foreground "red1"))))
     `(info-menu-header ((t (:bold t :family "helv" :weight bold))))
     `(info-node ((t (:italic t :bold t :foreground "brown" :slant italic :weight bold))))
     `(info-xref ((t (:bold t :foreground "magenta4" :weight bold))))
     
     `(isearch ((t (:background "magenta4" :foreground "light sky blue"))))
     `(isearch-lazy-highlight-face ((t (:background "pale turquoise"))))
     
     `(log-view-file-face ((t (:bold t :background "gray70" :weight bold))))
     `(log-view-message-face ((t (:foreground "sienna"))))
     
     `(makefile-space-face ((t (:background "hot pink"))))
     
     `(mode-line ((t (:background "#334B7D" :foreground ,fg))))
     `(mode-line-inactive ((t (:background ,fg :foreground "#000000"))))
     `(mode-line-buffer-id ((t (:bold t :foreground "gold" :background "#334B7D"))))
     
     `(which-func ((t (:foreground "gold"))))
     
     `(primary-selection ((t (:background "lightgoldenrod2"))))
     
     `(reb-match-0 ((t (:background "light blue"))))
     `(reb-match-1 ((t (:background "aquamarine"))))
     `(reb-match-2 ((t (:background "spring green"))))
     `(reb-match-3 ((t (:background "yellow"))))
     
     `(region ((t (:background "gray33"))))
     `(hl-line ((default (:background "gray20")) (nil nil)))
     
     `(secondary-selection ((t (:background "yellow"))))
     `(sh-heredoc-face ((t (:foreground "tan"))))
     
     `(show-paren-match-face ((t (:background "forest green"))))
     `(show-paren-mismatch-face ((t (:background "red" :foreground "white"))))
     `(show-tabs-space-face ((t (:foreground "yellow"))))
     `(show-tabs-tab-face ((t (:foreground "red"))))
     
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
     
     `(strokes-char-face ((t (:background "light grey"))))
     
     `(tex-math-face ((t (:foreground "rosy brown"))))
     `(texinfo-heading-face ((t (:foreground "blue"))))
     
     `(tooltip ((t (:background "white" :foreground "black"))))
     `(trailing-whitespace ((t (:background "red"))))
     `(variable-pitch ((t (:family "helv"))))
     `(linum ((t (:background "#2E2E37" :foreground "#D0D0A0"))))
     `(vcursor ((t (:background "grey20" :foreground "blue" :underline t))))
     
     `(minibuffer-prompt ((t (:foreground "orange"))))
     
     `(viper-minibuffer-insert ((t (:background "white" :foreground "black"))))
     `(viper-minibuffer-emacs ((t (:background "gray13" :foreground "white"))))
     `(viper-minibuffer-vi ((t (:background "gray" :foreground nil))))
     `(viper-search-face ((t (:background "khaki" :foreground "Black"))))
     
     `(zmacs-region ((t (:background "lightgoldenrod2"))))
     
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
     `(org-date ((t (:underline t :foreground "sky blue"))))
     `(org-sexp-date ((t (:underline t :foreground "sky blue"))))
     `(org-link ((t (:underline t :foreground "royal blue"))))
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

(provide-theme 'cofi)
