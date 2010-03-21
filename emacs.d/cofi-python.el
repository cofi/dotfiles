(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))

(autoload 'python-mode "python" "Python editing mode." t)

(when (locate-library "pymacs")
  (autoload 'pymacs-apply "pymacs")
  (autoload 'pymacs-call "pymacs")
  (autoload 'pymacs-eval "pymacs" nil t)
  (autoload 'pymacs-exec "pymacs" nil t)
  (autoload 'pymacs-load "pymacs" nil t)
  (pymacs-load "ropemacs" "rope-")
  (setq ropemacs-enable-autoimport t))


(when (load "flymake" t)
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

;; Modes
(add-hook 'python-mode-hook
          (lambda ()
            (progn
              (require 'virtualenv)
              (show-paren-mode 1)
              (auto-fill-mode 1))))

;; Keybindings
(add-hook 'python-mode-hook
          (lambda ()
            (progn
              (local-set-key (kbd "M-s") 'flymake-mode)
              (local-set-key (kbd "RET") 'newline-and-indent)
              (local-set-key (kbd "C-x n") 'flymake-goto-next-error)
              (local-set-key (kbd "M-n") 'python-describe-symbol)
              (local-set-key (kbd "C-c SPC") 'python-switch-to-python))))

;; Triple strings for autopair
(add-hook 'python-mode-hook
          #'(lambda ()
              (setq autopair-handle-action-fns
                    (list #'autopair-default-handle-action
                          #'autopair-python-triple-quote-action))))

(provide 'cofi-python)
