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

(add-to-list 'load-path "~/.elisp/python-mode/")
(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))

(require 'ipython)
(setq py-python-command-args '( "-colors" "Linux"))

(defadvice py-execute-buffer (around python-keep-focus activate)
  "return focus to python code buffer"
  (save-excursion ad-do-it))

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
          (lambda ()
            (fill-keymap py-mode-map
              "M-n"       'flymake-goto-next-error
              "M-p"       'flymake-goto-prev-error
              "C-c C-SPC" 'flymake-mode
              "M-?"       'rope-code-assist
              "C-M-?"     'rope-lucky-assist
              "C-c g"     'rope-goto-definition
              "C-c d"     'rope-show-doc
              "C-c t"     'rope-show-calltip
              "C-c ?"     'pylookup-lookup)))

(setq pylookup-db-file "~/var/pylookup.db"
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
