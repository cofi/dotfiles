;;;
;; Smart Tab
;; Taken from http://www.emacswiki.org/cgi-bin/wiki/TabCompletion

(defvar tab-base-completers
 '(try-expand-dabbrev-visible
   try-expand-dabbrev
   try-expand-dabbrev-all-buffers
   )
 "List of functions which are used by hippie-expand in all cases.")
(setq lisp-completers '(try-expand-list
                        try-complete-lisp-symbol-partially
                        try-complete-lisp-symbol))

(defvar tab-mode-completers
  (list
   (list 'emacs-lisp-mode (append tab-base-completers lisp-completers))
    (list 'lisp-mode (append tab-base-completers lisp-completers))
    )
  "List of major modes in which to use additional mode specific completion
functions.")


(defun get-completion-function()
  "Get a completion function according to current major mode."
  (let ((mode-functions
         (second (assq major-mode tab-mode-completers))))

    (if (null mode-functions)
        (make-hippie-expand-function tab-base-completers t)
      (make-hippie-expand-function mode-functions t)
      )))
 
(defun smart-tab (prefix)
  "Needs `transient-mark-mode' to be on. This smart tab is
minibuffer compliant: it acts as usual in the minibuffer.
 
In all other buffers: if PREFIX is \\[universal-argument], calls
`smart-indent'. Else if point is at the end of a symbol,
expands it. Else calls `smart-indent'."
  (interactive "P")
  (if (minibufferp)
      (minibuffer-complete)
    (if (smart-tab-must-expand prefix)
          (funcall (get-completion-function) prefix)
      (smart-indent))))
 
(defun smart-tab-must-expand (&optional prefix)
  "If PREFIX is \\[universal-argument], answers no.
Otherwise, analyses point position and answers."
  (unless (or (consp prefix)
              mark-active)
    (looking-at "\\_>")))

(defun smart-indent ()
  "Indents region if mark is active, or current line otherwise."
  (interactive)
  (if mark-active
      (indent-region (region-beginning)
                     (region-end))
    (indent-for-tab-command)))

(mapc (lambda (mode-hook)
        (add-hook mode-hook (lambda ()
                              (local-set-key (kbd "<tab>") 'smart-tab))
                  ))
      '(
        latex-mode-hook
        tex-mode-hook
        python-mode-hook
        emacs-lisp-mode-hook
        lisp-mode-hook
        ))

(provide 'cofi-tab)
