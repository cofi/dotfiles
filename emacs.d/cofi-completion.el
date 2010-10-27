(add-to-list 'load-path "~/.elisp/vendor/auto-complete")
(require-and-exec 'auto-complete 
    (require 'auto-complete-config)
    (require 'auto-complete-latex)

    (setq ac-fuzzy-enable t)
    (setq-default ac-auto-start 3)
    (define-key ac-complete-mode-map (kbd "C-l") 'ac-expand-common)
    (define-key ac-complete-mode-map (kbd "C-j") 'ac-next)
    (define-key ac-complete-mode-map (kbd "C-k") 'ac-previous)
    (define-key ac-complete-mode-map (kbd "ESC") 'keyboard-quit)

    (setq ac-dictionary-directories '("~/.emacs.d/completion-dicts"))

    (mapc (lambda (mode)
            (push mode ac-modes))
          '(
            rst-mode
            latex-mode
            text-mode
            org-mode
            ))

    (defvar ac-sources '(
                         ac-source-words-in-buffer
                         ac-source-words-in-same-mode-buffers
                         ac-source-yasnippet
                         ac-source-dictionary
                         ac-source-filename
                         ac-source-abbrev
                         ))
    (make-variable-buffer-local 'ac-sources)

    (defun cofi/ac-elisp-setup ()
      (setq ac-sources '(
                         ac-source-symbols
                         ac-source-functions
                         ac-source-variables
                         ac-source-features
                         ac-source-words-in-buffer
                         ac-source-words-in-same-mode-buffers
                         ac-source-yasnippet
                         )))


    (add-hook 'emacs-lisp-mode-hook 'cofi/ac-elisp-setup)
    (global-auto-complete-mode t)
    )

;; Manual completion -------------------------------------------------
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

(defconst cofi/lisp-completers
  '(
    try-expand-list
    try-complete-lisp-symbol-partially
    try-complete-lisp-symbol)
  "List of completers used for Lisp.")

(defconst cofi/python-completers
  (if (locate-library "pysmell")
      '(try-pysmell-complete)
    '())
  "List of completers used for Python.")

(defun cofi/completion-functions ()
  "Get a completion function according to current major mode."
  (cond
   ((in-mode? 'emacs-lisp-mode) (append cofi/base-completers cofi/lisp-completers))
   ((in-mode? 'lisp-mode) (append cofi/base-completers cofi/lisp-completers))
   ((in-mode? 'python-mode) (append cofi/python-completers cofi/base-completers))
   (t cofi/base-completers)))

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

(global-set-key (kbd "M-/") 'cofi/complete)
(global-set-key (kbd "C-M-/") 'cofi/uncommon-complete)

(provide 'cofi-completion)
