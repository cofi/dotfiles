(require-and-exec 'flymake
  (defun flymake-pylint-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "epylint" (list local-file))))

  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pylint-init)))

;; Folding
(add-hook 'python-mode-hook 'hs-minor-mode)

(setq python-python-command "ipython")

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
      ropemacs-enable-shortcuts nil)

;; Modes
(add-hook 'python-mode-hook
          (lambda ()
              (show-paren-mode 1)
              (auto-fill-mode 1)
              (setq tab-width 4)
              (setq mode-name "py")))

;; Keybindings
(add-hook 'python-mode-hook
          (gen-local-fill-keymap-hook
              "M-n"       'flymake-goto-next-error
              "M-p"       'flymake-goto-prev-error
              "C-c C-SPC" 'flymake-mode
              "M-?"       'rope-code-assist
              "C-M-?"     'rope-lucky-assist
              "C-c g"     'rope-goto-definition
              "C-c d"     'rope-show-doc
              "C-c t"     'rope-show-calltip
              "C-c ?"     'pylookup-lookup))

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
