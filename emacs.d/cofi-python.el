(add-to-loadpath "~/.elisp/vendor/python")
;;; setup for ipython 0.11
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args ""
      python-shell-prompt-regexp "In \\[[0-9]+\\]: "
      python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
      python-shell-completion-setup-code "from IPython.core.completerlib import module_completion"
      python-shell-completion-module-string-code "';'.join(module_completion('''%s'''))\n"
      python-shell-completion-string-code "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

(require-and-exec 'flymake
  (defun flymake-pylint-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "epylint" (list local-file))))

  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.pyw?\\'" flymake-pylint-init)))

;; Folding
(add-hook 'python-mode-hook 'hs-minor-mode)

;; setup pymacs
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)
;; setup ropemacs
(setq ropemacs-enable-autoimport t
      ropemacs-autoimport-modules '( "os"
                                     "os.path"
                                     "sys"
                                    )
      ropemacs-global-prefix "C-c p"
      ropemacs-guess-project t
      ropemacs-separate-doc-buffer nil
      ropemacs-enable-shortcuts nil)

(add-all-to-hook 'python-mode-hook
                 (turn-on autopair-mode)
                 (turn-on show-paren-mode)
                 #'turn-on-auto-fill
                 (lambda ()
                   (setq tab-width 4)
                   (setq mode-name "py"))
                 (gen-local-fill-keymap-hook
                     "M-n"       'flymake-goto-next-error
                     "M-p"       'flymake-goto-prev-error
                     "C-c C-SPC" 'flymake-mode
                     "M-?"       'rope-code-assist
                     "C-M-?"     'rope-lucky-assist
                     "C-c g"     'rope-goto-definition
                     "C-c d"     'rope-show-doc
                     "C-c t"     'rope-show-calltip
                     "C-c ?"     'pylookup-lookup)
                 )

(setq pylookup-db-file (cofi/var-file "pylookup.db")
      pylookup-html-locations '("~/doc/python-2.7/")
      pylookup-completing-read #'anything-completing-read)

;; Triple strings for autopair
(add-hook 'python-mode-hook
          #'(lambda ()
              ;; move single quote to string class for quote pairing
              (modify-syntax-entry ?' "\"")
              (setq autopair-handle-action-fns
                    (list #'autopair-default-handle-action
                          #'autopair-python-triple-quote-action))))

(add-hook 'python-mode-hook
          #'(lambda ()
              (pymacs-load "ropemacs" "rope-")))
(when (fboundp 'column-marker-1)
  (add-hook 'python-mode-hook 'highlight-80))
(provide 'cofi-python)
