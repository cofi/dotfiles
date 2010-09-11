;; -*- mode: emacs-lisp; mode: rainbow -*-
(require-and-exec 'color-theme
    (defun color-theme-cofi ()
      (interactive)
      (color-theme-install
       '(color-theme-cofi
         ((background-color . "gray13")
          (background-mode . dark)
          (border-color . "black")
          (cursor-color . "OliveDrab4")
          (foreground-color . "gray100")
          (mouse-color . "black"))
         ((help-highlight-face . underline)
          (ispell-highlight-face . highlight)
          (list-matching-lines-face . bold)
          (view-highlight-face . highlight)
          )

         (default ((t (:background "gray13" :foreground "gray100"))))
         (italic ((t (:italic t :family "Dejavu Sans Mono" :height 80))))
         (underline ((t (:underline t))))
         (bold ((t (:bold t))))
         (bold-italic ((t (:italic t :bold t))))

         (border ((t (:background "black"))))

         (change-log-acknowledgement-face ((t (:foreground "firebrick"))))
         (change-log-conditionals-face ((t (:foreground "dark goldenrod"))))
         (change-log-date-face ((t (:foreground "rosy brown"))))
         (change-log-email-face ((t (:foreground "dark goldenrod"))))
         (change-log-file-face ((t (:foreground "blue"))))
         (change-log-function-face ((t (:foreground "dark goldenrod"))))
         (change-log-list-face ((t (:foreground "purple"))))
         (change-log-name-face ((t (:foreground "cadet blue"))))

         (comint-highlight-input ((t (:bold t :weight bold))))
         (comint-highlight-prompt ((t (:foreground "dark blue"))))

         (cursor ((t (:background "OrangeRed2"))))

         (diff-added-face ((t (:foreground "green"))))
         (diff-changed-face ((t (:foreground "blue"))))
         (diff-removed-face ((t (:foreground "red"))))
         (diff-context-face ((t (:foreground "gray"))))
         (diff-file-header-face ((t (:foreground "gold" :background nil))))
         (diff-function-face ((t (:foreground "gray50" :background nil))))
         (diff-header-face ((t (:foreground "gold" :background nil))))
         (diff-hunk-header-face ((t (:foreground "gold"))))
         (diff-index-face ((t (:bold t :weight bold :background "gray70"))))
         (diff-nonexistent-face ((t (:bold t :weight bold :background "gray70"))))

         (ibuffer-git-add-face ((t (:foreground "green"))))
         (ibuffer-git-del-face ((t (:foreground "red"))))

         (magit-diff-file-header ((t (:foreground "gold" :background nil))))
         (magit-diff-hunk-header ((t (:foreground "gold"))))
         (magit-diff-none ((t (:foreground "gray"))))
         (magit-diff-add ((t (:foreground "green"))))
         (magit-diff-del ((t (:foreground "red"))))

         (dired-boring ((t (:foreground "rosy brown"))))
         (dired-directory ((t (:foreground "blue"))))
         (dired-flagged ((t (:foreground "red" :weight bold))))
         (dired-marked ((t (:foreground "red" :weight bold))))
         (dired-symlink ((t (:foreground "purple"))))

         (ediff-current-diff-face-A ((t (:background "pale green" :foreground "firebrick"))))
         (ediff-current-diff-face-Ancestor ((t (:background "VioletRed" :foreground "Black"))))
         (ediff-current-diff-face-B ((t (:background "Yellow" :foreground "DarkOrchid"))))
         (ediff-current-diff-face-C ((t (:background "Pink" :foreground "Navy"))))
         (ediff-even-diff-face-A ((t (:background "light grey" :foreground "Black"))))
         (ediff-even-diff-face-Ancestor ((t (:background "Gray" :foreground "White"))))
         (ediff-even-diff-face-B ((t (:background "Gray" :foreground "White"))))
         (ediff-even-diff-face-C ((t (:background "light grey" :foreground "Black"))))
         (ediff-fine-diff-face-A ((t (:background "sky blue" :foreground "Navy"))))
         (ediff-fine-diff-face-Ancestor ((t (:background "Green" :foreground "Black"))))
         (ediff-fine-diff-face-B ((t (:background "cyan" :foreground "Black"))))
         (ediff-fine-diff-face-C ((t (:background "Turquoise" :foreground "Black"))))
         (ediff-odd-diff-face-A ((t (:background "Gray" :foreground "White"))))
         (ediff-odd-diff-face-Ancestor ((t (:background "light grey" :foreground "Black"))))
         (ediff-odd-diff-face-B ((t (:background "light grey" :foreground "Black"))))
         (ediff-odd-diff-face-C ((t (:background "Gray" :foreground "White"))))

         (font-lock-builtin-face ((t (:foreground "LightSalmon4"))))
         (font-lock-comment-face ((t (:inherit italic :foreground "burlywood"))))
         (font-lock-constant-face ((t (:foreground "firebrick"))))
         (font-lock-doc-face ((t (:foreground "yellow"))))
         (font-lock-doc-string-face ((t (:foreground "yellow"))))
         (font-lock-function-name-face ((t (:foreground "steel blue"))))
         (font-lock-keyword-face ((t (:foreground "tomato2"))))
         (font-lock-preprocessor-face ((t (:foreground "cadet blue"))))
         (font-lock-reference-face ((t (:foreground "violet red"))))
         (font-lock-string-face ((t (:foreground "sienna1"))))
         (font-lock-type-face ((t (:italic t :foreground "forest green"))))
         (font-lock-variable-name-face ((t (:foreground "gold"))))
         (font-lock-warning-face ((t (:bold t :foreground "red" :weight bold))))

         (fringe ((t (:background "gray17"))))

         (highlight-changes-delete-face ((t (:foreground "red" :underline t))))
         (highlight-changes-face ((t (:foreground "red"))))

         (calendar-today-face ((t (:underline t))))
         (holiday-face ((t (:background "pink"))))

         (info-header-node ((t (:italic t :bold t :weight bold :slant italic :foreground "brown"))))
         (info-header-xref ((t (:bold t :weight bold :foreground "magenta4"))))
         (info-menu-5 ((t (:foreground "red1"))))
         (info-menu-header ((t (:bold t :family "helv" :weight bold))))
         (info-node ((t (:italic t :bold t :foreground "brown" :slant italic :weight bold))))
         (info-xref ((t (:bold t :foreground "magenta4" :weight bold))))

         (isearch ((t (:background "magenta4" :foreground "light sky blue"))))
         (isearch-lazy-highlight-face ((t (:background "pale turquoise"))))

         (log-view-file-face ((t (:bold t :background "gray70" :weight bold))))
         (log-view-message-face ((t (:foreground "sienna"))))

         (makefile-space-face ((t (:background "hot pink"))))

         (mode-line ((t (:background "gray10" :foreground "olive drab"))))
         (mode-line-inactive ((t (:background "gray20" :foreground "OliveDrab3"))))
         (mode-line-buffer-id ((t (:bold t :foreground "gold"))))

         (which-func ((t (:foreground "gold"))))

         (primary-selection ((t (:background "lightgoldenrod2"))))

         (reb-match-0 ((t (:background "light blue"))))
         (reb-match-1 ((t (:background "aquamarine"))))
         (reb-match-2 ((t (:background "spring green"))))
         (reb-match-3 ((t (:background "yellow"))))

         (region ((t (:background "gray33"))))
         (hl-line ((default (:background "gray20")) (nil nil)))

         (secondary-selection ((t (:background "yellow"))))
         (sh-heredoc-face ((t (:foreground "tan"))))

         (show-paren-match-face ((t (:background "forest green"))))
         (show-paren-mismatch-face ((t (:background "red" :foreground "white"))))
         (show-tabs-space-face ((t (:foreground "yellow"))))
         (show-tabs-tab-face ((t (:foreground "red"))))

         (smerge-base-face ((t (:foreground "red"))))
         (smerge-markers-face ((t (:background "gray85"))))
         (smerge-mine-face ((t (:foreground "blue"))))
         (smerge-other-face ((t (:foreground "DarkOliveGreen4"))))

         (speedbar-button-face ((t (:foreground "green4"))))
         (speedbar-directory-face ((t (:foreground "blue4"))))
         (speedbar-file-face ((t (:foreground "cyan4"))))
         (speedbar-highlight-face ((t (:background "green"))))
         (speedbar-selected-face ((t (:foreground "red" :underline t))))
         (speedbar-tag-face ((t (:foreground "brown"))))

         (strokes-char-face ((t (:background "light grey"))))

         (tex-math-face ((t (:foreground "rosy brown"))))
         (texinfo-heading-face ((t (:foreground "blue"))))

         (tooltip ((t (:background "white" :foreground "black"))))
         (trailing-whitespace ((t (:background "red"))))
         (variable-pitch ((t (:family "helv"))))
         (linum ((t (:inherit (shadow default) :background "slate gray" :foreground "khaki1"))))
         (vcursor ((t (:background "grey20" :foreground "blue" :underline t))))

         (minibuffer-prompt ((t (:foreground "orange"))))

         (viper-minibuffer-insert ((t (:background "white" :foreground "black"))))
         (viper-minibuffer-emacs ((t (:background "gray13" :foreground "white"))))
         (viper-minibuffer-vi ((t (:background "gray" :foreground nil))))
         (viper-search-face ((t (:background "khaki" :foreground "Black"))))

         (zmacs-region ((t (:background "lightgoldenrod2"))))

         (org-hide ((t (:foreground "gray13"))))
         (org-level-1 ((t (:bold t :foreground "chocolate4"))))
         (org-level-2 ((t (:bold t :foreground "chocolate3"))))
         (org-level-3 ((t (:bold t :foreground "chocolate2"))))
         (org-level-4 ((t (:bold t :foreground "orange3"))))
         (org-level-5 ((t (:bold t :foreground "dark orange"))))
         (org-level-6 ((t (:bold t :foreground "dark khaki"))))
         (org-level-7 ((t (:bold t :foreground "khaki"))))
         (org-level-8 ((t (:bold t :foreground "khaki1"))))
         (org-date ((t (:underline t :foreground "slate blue"))))
         (org-sexp-date ((t (:underline t :foreground "slate blue"))))
         (org-link ((t (:underline t :foreground "steel blue"))))
         (org-footnote ((t (:foreground "dark violet"))))
         (org-special-keyword ((t (:bold t :foreground "medium orchid"))))
         (org-todo ((t (:blod t :foreground "firebrick"))))
         (org-done ((t (:foreground "OliveDrab4" :strike-through t))))
         (org-agenda-done ((t (:foreground "OliveDrab4"))))
         (org-agenda-structure ((t (:foreground "cornsilk4"))))
         (org-meta-line ((t (:foreground "tan4"))))
         ))))

(provide 'cofi-color)
