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
                                    ))
(setq ropemacs-enable-shortcuts nil)
(pymacs-load "ropemacs" "rope-")
;; Modes
(add-hook 'python-mode-hook
          (lambda ()
              (show-paren-mode 1)
              (auto-fill-mode 1)
              (setq tab-width 4)
              (setq mode-name "py")))

;; Keybindings
(add-hook 'python-mode-hook
          (lambda ()
              (local-set-key (kbd "M-n") 'flymake-goto-next-error)
              (local-set-key (kbd "M-p") 'flymake-goto-prev-error)
              (local-set-key (kbd "C-c C-SPC") 'flymake-mode)
              (local-set-key (kbd "C-c SPC") 'python-shell)
              (local-set-key (kbd "M-?") 'rope-code-assist)
              (local-set-key (kbd "C-M-?") 'rope-lucky-assist)
              (local-set-key (kbd "C-c g") 'rope-goto-definition)
              (local-set-key (kbd "C-c d") 'rope-show-doc)
              (local-set-key (kbd "C-c t") 'rope-show-calltip)))

;; Triple strings for autopair
(add-hook 'python-mode-hook
          #'(lambda ()
              ;; move single quote to string class for quote pairing
              (modify-syntax-entry ?' "\"")
              (setq autopair-handle-action-fns
                    (list #'autopair-default-handle-action
                          #'autopair-python-triple-quote-action))))

(provide 'cofi-python)
