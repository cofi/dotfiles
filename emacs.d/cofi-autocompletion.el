(add-to-list 'load-path "~/.elisp/vendor/auto-complete")
(require-and-exec 'auto-complete 
    (require 'auto-complete-config)
    (require 'auto-complete-latex)

    (setq ac-fuzzy-enable t)
    (setq-default ac-auto-start 2)
    (define-key ac-complete-mode-map (kbd "C-l") 'ac-expand-common)
    (define-key ac-complete-mode-map (kbd "C-j") 'ac-next)
    (define-key ac-complete-mode-map (kbd "C-k") 'ac-previous)
    (define-key ac-complete-mode-map (kbd "ESC") 'keyboard-quit)

    (push "~/.emacs.d/completion-dicts" ac-dictionary-directories)

    (mapc (lambda (mode)
            (push mode ac-modes))
          '(
            rst-mode
            latex-mode
            text-mode
            ))
    (defconst cofi-ac-default-sources
      '(
        ac-source-yasnippet
        ac-source-abbrev
        ac-source-dictionary
        ac-source-words-in-same-mode-buffers
        ac-source-words-in-buffer
        ))

    (defun ac-common-setup ()
      (mapc (lambda (source)
              (push source ac-sources))
            cofi-ac-default-sources))

    (defun ac-lisp-mode-setup ()
      (ac-common-setup)
      (mapc (lambda (source)
              (push source ac-sources))
            '(
              ac-source-symbols
              ac-source-functions
              ac-source-variables
              ac-source-features
              )))

    (add-hook 'emacs-lisp-mode-hook 'ac-lisp-mode-setup)
    (add-hook 'lisp-mode-hook 'ac-lisp-mode-setup)

    (defun ac-python-mode-setup ()
      (ac-common-setup)
      (mapc (lambda (source)
              (push source ac-sources))
            '(
              ac-source-ropemacs
              )))

    (add-hook 'python-mode-hook 'ac-python-mode-setup)
    (add-hook 'auto-complete-mode-hook 'ac-common-setup)
    (global-auto-complete-mode t)
    )

(provide 'cofi-autocompletion)
