(add-to-list 'load-path "~/.elisp/vendor/vimpulse")
(setq viper-toggle-key [pause])
(setq-default viper-ESC-moves-cursor-back nil)
(setq-default viper-auto-indent t)
(setq viper-vi-style-in-minibuffer nil)
(setq viper-ex-style-editing nil)
(setq viper-fast-keyseq-timeout 0)
(setq viper-shift-width 4)
(setq viper-re-search t)
(require-and-exec 'vimpulse
  (require-and-exec 'vimpulse-surround)
  (require-and-exec 'goto-chg)

  ;; Window keybindings ========================================
  ;; kill vimpulse bindings (new defined in windowing)
  (define-key viper-vi-basic-map "\C-w" nil)
  ;; ============================================================

  ;; Keybindings ==========
  (fill-keymap viper-vi-global-user-map
               ;; Oh why are you compatible to that?
               "Y" (cmd-arg (arg) "P" (viper-goto-eol (cons arg ?y)))
               "_" 'viper-bol-and-skip-white
               "+" 'viper-next-line-at-bol
               "C-t" 'transpose-chars
               "C-e" 'viper-goto-eol
               "C-S-d" 'viper-scroll-down
               "C-S-f" 'viper-scroll-screen-back)

  (fill-keymap viper-insert-global-user-map
               "C-h" 'backward-delete-char
               "C-y" 'yank
               "C-e" 'viper-goto-eol)


  (vimpulse-imap "j" (cofi/maybe-exit-insert 'viper-exit-insert-state ?j ?k))

  (when (string< vimpulse-version "0.5")
    (when (featurep 'goto-last-change)
       (define-key viper-vi-global-user-map (kbd "g i") 'goto-last-change))
    (when (featurep 'undo-tree)
       (define-key viper-vi-basic-map (kbd "C-r") 'undo-tree-redo)))

  (fill-keymap viper-vi-global-user-map
               "SPC"   'ace-jump-mode
               "S-SPC" 'ace-jump-word-mode
               "C-SPC" 'ace-jump-line-mode
               "+"     'cofi/inc-at-pt
               "-"     'cofi/dec-at-pt
               "g;"    'goto-last-change
               "g,"    'goto-last-change-reverse
               "go"    'goto-char)

  (add-hook 'change-major-mode-hook 'viper-exit-insert-state)

  ;; no need for ex, gimme elisp
  (define-key viper-vi-basic-map (kbd "C-:") 'eval-expression)
  (define-key viper-vi-basic-map (kbd ":") 'execute-extended-command)
  ;; ==================================================

  ;; Mapleader ========================================
  ;; <leader> key in normal-mode
  (define-key viper-vi-global-user-map vim-mapleader cofi/vim-mapleader-map)
  ;; C-<leader> key in insert-mode
  (define-key viper-insert-global-user-map (read-kbd-macro
                                            (format "C-%s" vim-mapleader))
                                           cofi/vim-mapleader-map)
  ;; C-<leader> key in emacs-mode
  (define-key viper-emacs-global-user-map (read-kbd-macro
                                            (format "C-%s" vim-mapleader))
                                           cofi/vim-mapleader-map)
  ;; ==================================================

  ;; Vim-like backspace (backspace=indent,eol,start) ==========
  (require-and-exec 'sackspace
                    (sack/install-in-viper))
  ;; ==================================================

  ;; Search keybindings ========================================
  (when (string< vimpulse-version "0.5")
    (fill-keymap viper-vi-basic-map
                 "/" 'isearch-forward-regexp
                 "?" 'isearch-backward-regexp
                 "n" 'isearch-repeat-forward
                 "N" 'isearch-repeat-backward)
    (fill-keymap isearch-mode-map
                 "C-n" 'isearch-repeat-forward
                 "C-p" 'isearch-repeat-backward)
    )
  (push '("nohighlight" (isearch-done)) ex-token-alist)
  ;; ==================================================

  ;; Additional Modes ================================
;;   (define-key viper-emacs-global-user-map (kbd "C-\\") 'viper-escape-to-vi)
;;   (define-key viper-insert-global-user-map (kbd "C-\\") 'viper-intercept-ESC-key)
;;   (define-key viper-vi-global-user-map (kbd "C-\\") 'viper-escape-to-emacs)

  (dolist (mode '(magit-mode
                  magit-key-mode
                  magit-show-branches-mode
                  prolog-inferior-mode
                  inferior-python-mode
                  monky-mode))
    (push mode viper-emacs-state-mode-list))

  (dolist (mode '(clojure-mode))
    (push mode viper-vi-state-mode-list))

  (dolist (mode '(gnus-article-mode))
    (delete mode viper-vi-state-mode-list))
  ;; ===================================================

  ;; Conflicts ========================================
  (define-key viper-insert-basic-map (kbd "C-d") nil) ; conflicts with yasnippet
  (define-key viper-vi-basic-map (kbd "C-c /") nil)  ; conflicts with org
  ;; ==================================================

  ;; Misc ========================================
  (defun cofi/vimpulse-define-keys (mode state &rest pairs)
    "Define groups of key cmd `PAIRS' for `MODE' in `STATE'."
    (dolist (mapping (group pairs 2))
      (vimpulse-define-key mode state (read-kbd-macro (car mapping)) (cadr mapping))))

  (cofi/vimpulse-define-keys 'org-mode 'vi-state
                             "RET" 'org-open-at-point
                             "za" 'org-cycle
                             "zA" 'org-shifttab
                             "zm" 'hide-body
                             "zr" 'show-all
                             "zo" 'show-subtree
                             "zO" 'show-all
                             "zc" 'hide-subtree
                             "zC" 'hide-all)

  (cofi/vimpulse-define-keys 'org-mode 'insert-state
                             "M-l" 'org-metaright
                             "M-h" 'org-metaleft)

  (defun cofi/viper-bar ()
    "Change cursor color according to viper-state."
    (let ((default       "OliveDrab4")
          (cursor-colors '((insert-state  . "dark orange")
                           (replace-state . "red")
                           (emacs-state   . "sienna"))))
      (setq cursor-type 'bar)
      (set-cursor-color (def-assoc viper-current-state cursor-colors default))))

  (add-hook 'post-command-hook #'cofi/viper-bar)

  ;; Disable annoying vim compatibilities
  (define-key viper-vi-basic-map (kbd "C-y") nil)

  (defadvice vimpulse-goto-definition (around vimpulse-goto-lisp-def activate)
    "Make use of emacs' and slime's possibilities for finding definitions."
    (case major-mode
      (lisp-mode (if slime-mode
                     (or (slime-find-definitions (symbol-name (symbol-at-point)))
                         ad-do-it)
                   ad-do-it))
      (emacs-lisp-mode (condition-case nil
                           (find-function (symbol-at-point))
                         (error (condition-case nil
                                    (find-variable (symbol-at-point))
                                  (error ad-do-it)))))
      (otherwise ad-do-it))))

(provide 'cofi-vimpulse)
