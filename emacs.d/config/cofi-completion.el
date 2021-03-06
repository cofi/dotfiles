(require 'cl-lib)
(require 'cofi-util)
(require 'cofi-snippets)
(require-and-exec 'auto-complete
    (require 'auto-complete-config)
    (require 'auto-complete-latex)
    (setq ac-l-dict-directory "~/.emacs.d/completion-dicts/ac-l-dict/")
    (add-hook 'LaTeX-mode-hook #'ac-l-setup)

    (ac-define-source pysmell
      '((depends . pysmell)
        (candidates . pysmell-get-all-completions)))

    (require-and-exec 'org-contacts
      (defun ac-org-contacts-candidates ()
        (loop for (name _ properties) in (org-contacts-filter)
              for email = (cdr (assoc org-contacts-email-property properties))
              when email
              collect (format "%s <%s>" (org-no-properties name) email)))

      (defun ac-email-prefix ()
        (when (save-excursion (re-search-backward "^\\(?:To\\|Cc\\|Bcc\\): \\(.*\\)" nil t))
          (let ((simple-match-point (match-beginning 1)))
            ;; check for multiple addresses
            (if (re-search-backward ", \\(.*\\)" simple-match-point t)
                (match-beginning 1)
              simple-match-point))))

      (ac-define-source org-contacts
        '((depends . org-contacts)
          (candidates . ac-org-contacts-candidates)
          (prefix . ac-email-prefix)
          (cache . t))))

    (defun ac-yasnippet-candidates ()
      (cl-remove-if (lambda (name) (ends-with name ".snippet"))
       (cofi/active-snippet-names)))

    (setq ac-auto-start 2
          ac-auto-show-menu t
          ac-quick-help-delay 0.3
          ac-quick-help-height 50)

    (fill-keymap ac-completing-map
      "<return>" 'ac-complete
      "RET" 'ac-complete
      [M-h] 'ac-quick-help
      "C-l" 'ac-expand-common
      "C-j" 'ac-next
      "C-k" 'ac-previous
      "ESC" 'ac-stop)

    (setq ac-dictionary-directories '("~/.emacs.d/completion-dicts"))

    (mapc (lambda (mode)
            (push mode ac-modes))
          '(
            rst-mode
            latex-mode
            text-mode
            org-mode
            message-mode
            ess-mode
            inferior-ess-mode
            ))

    (ac-define-source words-in-same-mode-buffers
      '((init . ac-update-word-index)
        (candidates . (ac-word-candidates
                       (lambda (buffer)
                         (eq major-mode (buffer-local-value 'major-mode buffer)))))))

    (defvar cofi/ac-base-sources '(ac-source-semantic
                                   ac-source-words-in-buffer
                                   ac-source-words-in-same-mode-buffers
                                   ac-source-yasnippet))

    (setq-default ac-sources cofi/ac-base-sources)

    (defvar cofi/ac-mode-sources
      '((emacs-lisp-mode . (ac-source-symbols
                            ac-source-functions
                            ac-source-variables
                            ac-source-features))
        (lisp-mode       . (ac-source-words-in-buffer
                            ac-source-slime-fuzzy
                            ac-source-slime-simple))
        (python-mode     . (ac-source-words-in-buffer
                            ac-source-jedi-direct
                            ac-source-nropemacs
                            ac-source-nropemacs-dot))
        (java-mode       . (ac-source-words-in-buffer
                            ac-source-eclim))
        (html-mode       . (ac-source-words-in-buffer
                            ac-source-css-property))
        (message-mode    . (ac-source-words-in-buffer
                            ac-source-org-contacts))
        (ess-mode        . (ac-source-R-args
                            ac-source-R-objects
                            ac-source-R))
        ))

    (defvar cofi/ac-mode-aliases
      '(((inferior-emacs-lisp-mode lisp-interaction-mode) . emacs-lisp-mode)
        ((inferior-lisp-mode slime-repl-mode)             . lisp-mode)
        ((nxhtml-mode css-mode)                           . html-mode)
        ((inferior-python-mode)                           . python-mode)))

    (defun cofi/set-ac-sources ()
      (let* ((alias (cdr (cl-find major-mode cofi/ac-mode-aliases :key #'car :test #'member)))
             (mode-sources (cdr (assoc (or alias major-mode) cofi/ac-mode-sources))))
        (setq ac-sources (cl-remove-duplicates (append mode-sources
                                                       cofi/ac-base-sources)))))

    (add-to-hooks #'cofi/set-ac-sources '(after-change-major-mode-hook
                                          mumamo-after-change-major-mode-hook))
    (global-auto-complete-mode t)
    )

;; Manual completion -------------------------------------------------
(require 'hippie-exp)
(defvar cofi/base-completers
 '(try-expand-dabbrev-visible
   try-expand-dabbrev
   try-expand-dabbrev-all-buffers
   )
 "List of functions which are used by hippie-expand in all cases.")

(defvar cofi/uncommon-completers
  '(
    try-complete-file-name-partially
    try-complete-file-name
    )
 "List of uncommon completers")

(defvar cofi/mode-completers
  `((emacs-lisp-mode . (try-expand-list
                        try-complete-lisp-symbol-partially
                        try-complete-lisp-symbol))
    (python-mode     . ,(if (locate-library "pysmell")
                            '(try-pysmell-complete)))))

(defvar cofi/completion-mode-aliases cofi/ac-mode-aliases)

(defun cofi/completion-functions ()
  "Get a completion function according to current major mode."
  (let ((alias (cdr (cl-find major-mode cofi/completion-mode-aliases :key #'car :test #'member))))
    (cl-remove-duplicates
     (append (cdr (assoc (or alias major-mode) cofi/mode-completers))
             cofi/base-completers))))

(defun cofi/complete (prefix)
  "Do hippie-completion based on current major-mode."
  (interactive "P")
  (funcall (make-hippie-expand-function (cofi/completion-functions) t) prefix))

(defun  cofi/uncommon-complete (prefix)
  "Do hippie-expand with uncommon completers"
  (interactive "P")
  (let ((hippie-expand-try-functions-list cofi/uncommon-completers)
        (hippie-expand-verbose t))
    (hippie-expand prefix)))

(add-to-list 'completion-styles 'substring 'append)

(setq completion-styles '(basic partial-completion initials substring))

(provide 'cofi-completion)
