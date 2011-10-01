;; -*- mode: emacs-lisp; mode: rainbow -*-

(deftheme cofi-light
  "cofi theme.")

(let ((bg "#fdf6e3")
      (fg "black")
      (vc-added "green4")
      (vc-removed "red4")
      (vc-changed "blue4")
      (vc-context "#526578")
      (header-color "gold4")
      )
    (custom-theme-set-faces
     'cofi-light
     ;; basic faces
     `(default ((((min-colors 4096)) (:background ,bg :foreground ,fg))))
     `(italic ((t (:italic t :family "Dejavu Sans Mono" :height 80 :underline nil))))
     `(border ((t (:background "black"))))
     `(header-line ((t (:foreground ,fg :background ,bg :bold t))))
     `(highlight ((t (:background "#c0d0e0"))))
     `(fringe ((t (:background ,bg))))

     `(primary-selection ((t (:foreground "gold" :background "black"))))
     `(secondary-selection ((t (:foreground "black" :background "gold"))))

     `(minibuffer-prompt ((t (:foreground "firebrick" :bold t))))

     `(region ((t (:background "#586e75"))))
     `(hl-line ((t (:background "#c0d0e0"))))
     `(tooltip ((t (:background ,bg :foreground ,fg))))

     ;; modeline
     `(mode-line ((t (:background "#8089a0" :foreground "black"))))
     `(mode-line-inactive ((t (:background "gray80" :foreground "#8089a0"))))
     `(mode-line-buffer-id ((t (:foreground "black" :background "#8089a0"))))
     `(mode-line-buffer ((t (:bold t :foreground "black"))))
     `(mode-line-major-mode ((t (:bold t :foreground "gold"))))
     `(mode-line-minor-mode ((t (:foreground "khaki2"))))

     ;; diff
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

     ;; ibuffer
     `(ibuffer-git-add-face ((t (:foreground ,vc-added))))
     `(ibuffer-git-del-face ((t (:foreground ,vc-removed))))

     ;; magit
     `(magit-diff-file-header ((t (:foreground ,header-color :background ,bg))))
     `(magit-diff-hunk-header ((t (:foreground ,header-color))))
     `(magit-diff-none ((t (:foreground ,vc-context))))
     `(magit-diff-add ((t (:foreground ,vc-added))))
     `(magit-diff-del ((t (:foreground ,vc-removed))))
     `(magit-item-highlight ((t (:inherit highlight))))

     ;; dired
     `(dired-boring ((t (:foreground "rosy brown"))))
     `(dired-directory ((t (:foreground "#8B0017"))))
     `(dired-flagged ((t (:foreground "red" :bold t))))
     `(dired-marked ((t (:foreground "red" :bold t))))
     `(dired-symlink ((t (:foreground "#00316E"))))

     ;; dired+
     `(diredp-flag-mark-line ((t (:background "cornsilk2" :foreground "red" :bold t))))
     `(diredp-flag-mark ((t (:background "purple" :foreground "white"))))
     `(diredp-read-priv ((t (:background ,bg :foreground "#008C00"))))
     `(diredp-write-priv ((t (:background ,bg :foreground "#0000BF"))))
     `(diredp-exec-priv ((t (:background ,bg :foreground "firebrick"))))
     `(diredp-no-priv ((t (:background ,bg :foreground ,fg))))
     `(diredp-dir-priv ((t (:foreground "#8B0017"))))
     `(diredp-symlink ((t (:foreground "#00316E"))))

     `(diredp-file-name ((t (:foreground "#803F00"))))
     `(diredp-file-suffix ((t (:foreground "#638000"))))
     `(diredp-compressed-file-suffix ((t (:foreground "gold2"))))
     `(diredp-inode+size ((t (:foreground "#8F6B32"))))
     `(diredp-date-time ((t (:foreground "navy"))))
     `(diredp-dir-heading ((t (:background ,bg :foreground "#6A0056"))))

     ;; ediff
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

     ;; font lock -- syntax
     `(font-lock-builtin-face ((t (:foreground "#8B0017"))))
     `(font-lock-comment-face ((t (:inherit italic :foreground "#526578"))))
     `(font-lock-constant-face ((t (:foreground "#BF5E00"))))
     `(font-lock-doc-face ((t (:foreground "#75511A"))))
     `(font-lock-function-name-face ((t (:foreground "#2642D2"))))
     `(font-lock-keyword-face ((t (:foreground "#AC4311"))))
     `(font-lock-preprocessor-face ((t (:foreground "#3D355D"))))
     `(font-lock-string-face ((t (:foreground "#B20000"))))
     `(font-lock-type-face ((t (:italic t :foreground "#802A00"))))
     `(font-lock-variable-name-face ((t (:foreground "#1E5976"))))
     `(font-lock-warning-face ((t (:bold t :foreground "red" :weight bold))))

     `(makefile-space-face ((t (:background "hot pink"))))

     `(sh-heredoc-face ((t (:foreground "tan"))))

     ;; highlight changes
     `(highlight-changes-delete-face ((t (:foreground "red" :underline t))))
     `(highlight-changes-face ((t (:foreground "red"))))

     ;; isearch
     `(isearch ((t (:background "magenta4" :foreground "white"))))
     `(isearch-lazy-highlight-face ((t (:background "pale turquoise"))))

     ;; log view
     `(log-view-file-face ((t (:bold t :background "gray70" :weight bold))))
     `(log-view-message-face ((t (:foreground "sienna"))))

     ;; sml modeline
     `(sml-modeline-vis-face ((t (:foreground "white" :background "gray20"))))
     `(sml-modeline-end-face ((t (:foreground "white" :background "#334B7D"))))

     ;; show paren
     `(show-paren-match-face ((t (:background "forest green"))))
     `(show-paren-mismatch-face ((t (:background "red" :foreground "white"))))

     ;; smerge
     `(smerge-base-face ((t (:foreground "red"))))
     `(smerge-markers-face ((t (:background "gray85"))))
     `(smerge-mine-face ((t (:foreground "blue"))))
     `(smerge-other-face ((t (:foreground "DarkOliveGreen4"))))

     ;; viper
     `(viper-minibuffer-insert ((t (:background ,bg :foreground ,fg))))
     `(viper-minibuffer-emacs ((t (:background ,bg :foreground ,fg))))
     `(viper-minibuffer-vi ((t (:background ,bg :foreground ,fg))))

     ;; rst
     `(rst-level-1-face ((t (:bold t :foreground "chocolate4" :background ,bg))))
     `(rst-level-2-face ((t (:bold t :foreground "chocolate3" :background ,bg))))
     `(rst-level-3-face ((t (:bold t :foreground "chocolate2" :background ,bg))))
     `(rst-level-4-face ((t (:bold t :foreground "orange3" :background ,bg))))
     `(rst-level-5-face ((t (:bold t :foreground "dark orange" :background ,bg))))
     `(rst-level-6-face ((t (:bold t :foreground "dark khaki" :background ,bg))))

     ;; org
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
     `(org-todo ((t (:bold t :foreground "firebrick"))))
     `(org-done ((t (:foreground "OliveDrab4" :strike-through t))))
     `(org-agenda-done ((t (:foreground "OliveDrab4"))))
     `(org-agenda-structure ((t (:foreground "cornsilk4"))))
     `(org-agenda-date-weekend ((t (:foreground "brown"))))
     `(org-meta-line ((t (:foreground "tan4"))))

     ;; calendar
     `(calendar-today-face ((t (:underline t))))
     `(holiday-face ((t (:foreground "gray100" :background "firebrick"))))


     ;; anything
     `(anything-header ((t (:foreground "gold" :background "black"))))

     ;; ido
     `(ido-first-match ((t (:foreground "#798447" :bold t))))
     `(ido-only-match ((t (:foreground "#596235"))))
     `(ido-subdir ((t (:foreground  "#8c5353"))))

     ;; workgroups
     `(wg-mode-line-face ((t (:foreground "white"))))
     `(wg-brace-face ((t (:foreground ,fg))))
     `(wg-divider-face ((t (:foreground ,fg))))
     `(wg-filename-face ((t (:foreground "slate blue"))))
     `(wg-command-face ((t (:foreground ,fg :bold t))))
     `(wg-frame-face ((t (:foreground ,fg))))

     ;; flyspell
     `(flyspell-duplicate ((t (:underline "gold3"))))
     `(flyspell-incorrect ((t (:underline "OrangeRed"))))

     ;; misc
     `(which-func ((t (:foreground "gold"))))

     `(linum ((t (:foreground "#000000" :background "#c0d0e0"))))

     `(trailing-whitespace ((t (:background "red" :foreground "yellow"))))
     ))

(provide-theme 'cofi-light)
