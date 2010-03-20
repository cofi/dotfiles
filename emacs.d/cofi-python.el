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

(defun python-outline-level ()
  (let (buffer-invisibility-spec)
    (save-excursion
      (skip-chars-forward "\t ")
      (current-column))))

;; Outlining
(add-hook 'python-mode-hook
          (lambda ()
            (progn
              (set-variable 'outline-regexp "[^ \t]\\|[ \t]*\\(def\\|class\\) ")
              (setq outline-level 'python-outline-level)
              (local-set-key (kbd "C-x SPC") 'outline-toggle-children)
              (hide-body))))

;; Modes
(add-hook 'python-mode-hook
          (lambda ()
            (progn
              (require 'virtualenv)
              (show-paren-mode 1)
              (auto-fill-mode 1)
              (outline-minor-mode 1))))

;; Keybindings
(add-hook 'python-mode-hook
          (lambda ()
            (progn
              (local-set-key (kbd "M-s") 'flymake-mode)
              (local-set-key (kbd "RET") 'newline-and-indent)
              (local-set-key (kbd "C-x n") 'flymake-goto-next-error)
              (local-set-key (kbd "M-n") 'python-describe-symbol)
              (local-set-key (kbd "C-c SPC") 'python-switch-to-python))))

(provide 'cofi-python)
