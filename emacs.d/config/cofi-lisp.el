(add-to-loadpath "~/.elisp/slime/"
                 "~/.elisp/slime/contrib"
                 "~/.elisp/vendor/ac-slime")

(add-hook 'slime-mode-hook (gen-fill-keymap-hook 'slime-mode-map
                             "C-c '"       'slime-selector
                             "C-c C-;"     'slime-insert-balanced-comments
                             "C-c M-;"     'slime-remove-balanced-comments
                             "C-c C-d C-a" 'slime-arglist))

;;; CL
(add-major-mode "\\.cl$" 'lisp-mode)
(add-hook 'lisp-mode-hook (lambda ()
                            (set (make-local-variable 'lisp-indent-function) 'common-lisp-indent-function)))

(setq lisp-lambda-list-keyword-alignment t
      lisp-lambda-list-keyword-parameter-alignment t)

(require-and-exec 'info-look
  (info-lookup-add-help
   :mode 'lisp-mode
   :regexp "[^][()'\" \t\n]+"
   :ignore-case t
   :doc-spec '(("(ansicl)Symbol Index"))))

;;; paredit
(require 'paredit)
(let ((paredit-mode-hooks '(lisp-mode-hook
                            clojure-mode-hook
                            slime-repl-mode-hook)))
  (add-to-hooks #'enable-paredit-mode paredit-mode-hooks))

(require 'slime)
(require 'ac-slime)
(defadvice paredit-newline (before slime-eval-print-in-scratch activate)
  "Call `SLIME-EVAL-PRINT-LAST-EXPRESSION' in slime scratch."
  (if (and slime-mode (string= "*slime-scratch*" (buffer-name)))
      (call-interactively #'slime-eval-print-last-expression)))

;;; TODO: This maybe needs integration
;; (defvar common-lisp-octothorpe-quotation-characters '(?P))
;; (defvar common-lisp-octothorpe-parameter-parenthesis-characters '(?A))
;; (defvar common-lisp-octothorpe-parenthesis-characters '(?+ ?- ?C))

;; (defun paredit-space-for-delimiter-predicate-common-lisp (endp delimiter)
;;   (or endp
;;       (let ((case-fold-search t)
;;             (look
;;              (lambda (prefix characters n)
;;                (looking-back
;;                 (concat prefix (regexp-opt (mapcar 'string characters)))
;;                 (min n (point))))))
;;         (let ((oq common-lisp-octothorpe-quotation-characters)
;;               (op common-lisp-octothorpe-parenthesis-characters)
;;               (opp common-lisp-octothorpe-parameter-parenthesis-characters))
;;           (cond ((eq (char-syntax delimiter) ?\()
;;                  (and (not (funcall look "#" op 2))
;;                       (not (funcall look "#[0-9]*" opp 20))))
;;                 ((eq (char-syntax delimiter) ?\")
;;                  (not (funcall look "#" oq 2)))
;;                 (t t))))))

;; (add-hook 'lisp-mode-hook
;;   (defun common-lisp-mode-hook-paredit ()
;;     (make-local-variable 'paredit-space-for-delimiter-predicates)
;;     (add-to-list 'paredit-space-for-delimiter-predicates
;;                  'paredit-space-for-delimiter-predicate-common-lisp)))
;;; integration end

;;; slime
(setq slime-lisp-implementations
      `((sbcl ("sbcl" "--core" ,(expand-file-name "~/var/sbcl.swank-core"))
              :init (lambda (port-file _)
                      (format "(swank:start-server %S)\n" port-file))
              :coding-system utf-8-unix)))

(setq slime-net-coding-system 'utf-8-unix)

;;; keep our repl clean
(setq slime-repl-history-trim-whitespaces t
      slime-repl-history-remove-duplicates t)

;;; spare me this fasl
(let ((fasl-dir (expand-file-name "~/tmp/slime-fasls/")))
  (make-directory fasl-dir t)
  (setq slime-compile-file-options `(:fasl-directory ,fasl-dir)))

(require-and-exec 'slime-autoloads
  (slime-setup '(slime-fancy
                 slime-banner
                 helm-slime
                 slime-autodoc
                 slime-editing-commands
                 slime-fancy-inspector
                 slime-fancy
                 slime-fontifying-fu
                 slime-fuzzy
                 slime-indentation
                 slime-package-fu
                 slime-references
                 slime-scratch
                 slime-xref-browser
                 slime-presentations
                 ))
  (setq slime-complete-symbol-function (f-alt 'helm-slime-complete
                                              'slime-fuzzy-complete-symbol
                                              'slime-simple-complete-symbol))
  (setq slime-protocol-version 'ignore))

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

(require 'slime-repl)
(defslime-repl-shortcut slime-quickload ("quickload" "ql")
  (:handler #'cofi/slime-repl-quickload)
  (:one-liner "Load system from quickload distribution"))

(defun cofi/slime-repl-quickload ()
  (interactive)
  (let ((system-name (completing-read "System: " (slime-eval '(cl:mapcar 'ql-dist:system-file-name
                                                                         (ql:system-list)))
                                      nil t)))
    (slime-eval-async `(cl:progn (ql:quickload ,system-name)
                                 (cl:format t "; Loaded system \"~A\".~%" ,system-name)))))

(provide 'cofi-lisp)
