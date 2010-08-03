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
              (local-set-key (kbd "C-c SPC") 'python-shell)))

;; Triple strings for autopair
(add-hook 'python-mode-hook
          #'(lambda ()
              (setq autopair-handle-action-fns
                    (list #'autopair-default-handle-action
                          #'autopair-python-triple-quote-action))))

(provide 'cofi-python)
