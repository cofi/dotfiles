(add-to-loadpath "~/.elisp/vendor/slime/"
                 "~/.elisp/vendor/clojure-mode/")

(dolist (hook '(clojure-mode-hook lisp-mode-hook))
  (add-hook hook
            (gen-local-fill-keymap-hook
            ;; viper overshadows slime-repl binding
             "C-c M-p" 'slime-repl-set-package
             "C-c '"   'slime-selector
             "C-c C-;" 'slime-insert-balanced-comments
             "C-c M-;" 'slime-remove-balanced-comments)))

;;; CL
(add-major-mode "\\.cl$" 'lisp-mode)
(add-hook 'lisp-mode-hook (lambda ()
                            (set (make-local-variable 'lisp-indent-function) 'common-lisp-indent-function)))
(add-hook 'lisp-mode-hook (lambda () (unless (slime-connected-p)
                                       (save-excursion (call-interactively #'slime))))
          'append)

;;; paredit
(require 'paredit)
(let ((paredit-mode-hooks '(lisp-mode-hook
                            clojure-mode-hook
                            slime-repl-mode-hook)))
  (add-to-hooks #'enable-paredit-mode paredit-mode-hooks))

(defadvice paredit-newline (before slime-eval-print-in-scratch activate)
  "Call `SLIME-EVAL-PRINT-LAST-EXPRESSION' in slime scratch."
  (if (and slime-mode (string= "*slime-scratch*" (buffer-name)))
      (call-interactively #'slime-eval-print-last-expression)))

;;; slime
(setq slime-lisp-implementations
      `((sbcl ("sbcl" "--core" ,(expand-file-name "~/var/sbcl.swank-core"))
              :init (lambda (port-file _)
                      (format "(swank:start-server %S)\n" port-file))
              :coding-system utf-8-unix)))

(setq slime-net-coding-system 'utf-8-unix)

(add-hook 'sldb-mode-hook #'(lambda () (setq autopair-dont-activate t)))

(require-and-exec 'slime
                  (slime-setup '(slime-fancy
                                 slime-banner
                                 anything-slime)))
(setq slime-protocol-version 'ignore)
(when (fboundp 'anything-slime-complete)
  (setq slime-complete-symbol-function #'anything-slime-complete))

(define-key read-expression-map (kbd "TAB") 'lisp-complete-symbol)

;;; from http://bc.tech.coop/blog/070425.html
(defun slime-new-repl (&optional new-port)
  "Create additional REPL for the current Lisp connection."
  (interactive)
  (if (slime-current-connection)
      (let ((port (or new-port (slime-connection-port (slime-connection)))))
        (slime-eval `(swank::create-server :port ,port))
        (slime-connect slime-lisp-host port))
    (error "Not connected")))

(provide 'cofi-lisp)
