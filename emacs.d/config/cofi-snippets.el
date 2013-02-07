(add-to-loadpath "~/.elisp/vendor/yasnippet")
(setq yas-prompt-functions '(yas-ido-prompt
                             yas-completing-prompt))
(setq yas-snippet-dirs '("~/.yasnippets" "~/dev/snippets"))
(setq yas-also-auto-indent-first-line nil)
(setq yas-indent-line 'fixed)
(setq yas-triggers-in-field t)
(setq yas-verbosity 2)
(require-and-exec 'yasnippet
  (yas-global-mode 1))

(define-key yas-minor-mode-map "\C-c&" nil)

(defun yas-org-very-safe-expand ()
  (let ((yas-fallback-behavior 'return-nil)) (yas-expand)))
(add-hook 'org-mode-hook
          (lambda ()
            ;; yasnippet (using the new org-cycle hooks)
            (add-to-list 'org-tab-first-hook 'yas-org-very-safe-expand)
            (define-key yas-keymap [tab] 'yas-next-field)))

(defun cofi/yas-expand-or-spc (count)
  (interactive "p")
  (let ((yas-fallback-behavior 'return-nil))
    (unless (yas-expand)
      (insert (make-string count ? )))))

(defun cofi/no-yas ()
  (setq yas-dont-activate t))

(add-to-hooks 'cofi/no-yas '(magit-mode-hook
                            calc-mode-hook))

(defun cofi/region-to-snippet (begin end)
  "Write new snippet based on current region."
  (interactive "r")
  (let ((region (buffer-substring begin end)))
    (yas-new-snippet)
    (save-excursion
      (goto-char (point-max))
      (insert region))))

;;; snippet helpers
(defun cofi/snippet-in-string ()
  (let ((face-props (get-text-property (point) 'face)))
    (cofi/contains-any (if (listp face-props)
                           face-props
                         (list face-props))
                       '(font-lock-doc-face
                         font-lock-string-face))))

(defun cofi/snippet-in-comment ()
  (let ((face-props (get-text-property (point) 'face)))
    (cofi/contains-any (if (listp face-props)
                           face-props
                         (list face-props))
                       '(font-lock-comment-face))))

(defun cofi/snippet-in-code ()
  (not (or (cofi/snippet-in-comment)
        (cofi/snippet-in-string))))

(provide 'cofi-snippets)
