;; Clean UI ========================================
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'tooltip-mode) (tooltip-mode -1))
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'blink-cursor-mode) (blink-cursor-mode -1))
;; ==================================================

(eval-after-load "isearch"
  '(progn
     (defun cofi/isearch-accept-even-partial-match ()
       (interactive)
       (isearch-done))

     (eval-after-load "evil"
       '(cofi/set-key evil-ex-search-keymap "M-RET" #'cofi/isearch-accept-even-partial-match))
     (cofi/set-key isearch-mode-map "M-RET" #'cofi/isearch-accept-even-partial-match)))

;; other packages ========================================
(require-and-exec 'highlight-parentheses
                  (add-hook 'find-file-hook 'highlight-parentheses-mode))

(require-and-exec 'uniquify
                  (setq uniquify-buffer-name-style 'post-forward
                        uniquify-separator "/"
                        uniquify-after-kill-buffer-p t
                        uniquify-ignore-buffers-re "^\\*"))

(require-and-exec 'eldoc
  (add-to-hooks 'eldoc-mode '(python-mode-hook
                              emacs-lisp-mode-hook)))

(eval-after-load 'auto-dictionary
  '(progn
     (defun cofi/maybe-cancel-adict-timer ()
       (when adict-timer
         (cancel-timer adict-timer)))
     (add-hook 'kill-buffer-hook #'cofi/maybe-cancel-adict-timer)))

(setq doc-view-continuous t)

(setq pp^L-^L-string (concat (make-string 30 ? ) "⁂" (make-string 30 ? ))
      pp^L-^L-string-pre "")
(when (fboundp 'pretty-control-l-mode)
  (pretty-control-l-mode 1))

(require-and-exec 'keychain-environment
  (run-with-timer 100 (* 5 60) 'keychain/refresh))

(require-and-exec 'rdictcc
  (setq rdictcc-program "pdictcc"
        rdictcc-program-args "-c"))
;; ==================================================

;; Settings ========================================
(setq gc-cons-threshold (* 20 (expt 2 20))) ; gc after 20MB

(setq frame-title-format "emacs %b - <%f>"
      icon-title-format "emacs %b")

;; no, my sentences don't end with two instead of one space
(setq sentence-end-double-space nil)

(setq comment-style 'plain)

(setq x-select-enable-primary t)

(setq use-dialog-box nil)

(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

;;; Use string syntax for re-builder
(setq reb-re-syntax 'string)

(setq inhibit-startup-screen t)

(setq ediff-split-window-function 'split-window-horizontally
      ediff-window-setup-function 'ediff-setup-windows-plain)

;; Use UTF-8 dammit
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(setq browse-url-browser-function '(("/usr/share/doc/hyperspec" . w3m-browse-url)
                                    ("."                        . browse-url-firefox)))
;; tab settings
(setq-default tab-stop-list (range 4 160 4)
              indent-tabs-mode nil     ; no nasty tabs i say!
              tab-width 4)
(setq-default fill-column 80)
(setq tab-always-indent 'complete)

(setq dabbrev-case-replace nil)

;;; Help files
(setq-default woman-use-own-frame nil
              woman-use-topic-at-point t
              woman-imenu t
              woman-cache-filename (cofi/var-file "emacs/woman-cache"))

(require 'info)
(add-to-list 'Info-directory-list "~/doc/info")
;;; ========================================

;; mixedCase to small_words_with_underscores (visually)
(setq glasses-separate-parentheses-p nil
      glasses-uncapitalize-p t
      glasses-uncapitalize-regexp "[a-zA-Z]")

(setq-default major-mode 'text-mode)
(setq-default cursor-type 'bar)

(setq font-lock-verbose nil)
;; ==================================================

;; Default modes ========================================
(require-and-exec 'saveplace
                  (setq save-place-file (cofi/var-file "emacs/places"))
                  (setq-default save-place t))
(transient-mark-mode 1)
(global-font-lock-mode 1)
(global-hl-line-mode 1)
;; enable (and re-enable) linum-mode only on real files
(add-to-hooks (turn-on-file linum-mode)
              '(find-file-hook
                mumamo-after-change-major-mode-hook
                change-major-mode-hook))

(setq show-paren-style 'expression)
(show-paren-mode t)
(when on-mobile?
  (ignore-errors
    (display-battery-mode 1)))

(defadvice save-buffers-kill-emacs (around no-process-query activate)
  (cl-flet ((process-list ()))
    ad-do-it))
(setq mumamo-chunk-coloring 1)
;; ==================================================

;; enable functions ========================================
(put 'narrow-to-defun 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'set-goal-column 'disabled nil)
(defalias 'yes-or-no-p 'y-or-n-p)
;; ==================================================

;; prettyfying ==============================
(add-to-list 'load-path "~/.elisp/vendor/pretty-mode/")
(require-and-exec 'pretty-mode
  (defadvice indent-region (around no-pretty-on-indent activate)
    (if pretty-mode
        (progn
          (pretty-mode -1)
          ad-do-it
          (pretty-mode 1))
      ad-do-it))
  (dolist (mode '(python-mode c-mode java-mode cpp-mode))
          (pretty-add-keywords mode '(("="  . "←")
                                      ("==" . "≡"))))
  (global-pretty-mode 1))

;; ========================================
;;; scratch ====================
(defun save-a-scratch ()
  "Prevent *scratch* buffer from being killed.
Intended as `kill-buffer-query-functions' fun."
  (not (string= "*scratch*" (buffer-name))))

(push #'save-a-scratch kill-buffer-query-functions)
;;; ==============================

(setq multi-term-dedicated-select-after-open-p t)

(provide 'cofi-ui)
