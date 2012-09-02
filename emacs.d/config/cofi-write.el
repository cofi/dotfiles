(defvar cofi/ispell-change-dictionary-hook nil
  "Run after ispell dictionary is changed via ispell-change-dictionary")

(defadvice ispell-change-dictionary (after run-change-hooks activate)
  (run-hooks 'cofi/ispell-change-dictionary-hook))

(let ((modes `(flyspell-mode
               auto-fill-mode
               auto-dictionary-mode)))

  (add-to-hook 'rst-mode-hook modes)
  (add-to-hook 'markdown-mode-hook modes))

(defun cofi/atd-compile ()
  (interactive)
  (let ((language (def-assoc
                    (or ispell-local-dictionary ispell-dictionary)
                    '(("german" . "de")
                      ("deutsch" . "de"))
                    "en")))
    (when (executable-find "atdtool")
      (compile (format "atdtool --atdlang=%s %s"
                       language
                       (shell-quote-argument buffer-file-name))))))

(require 'rst)

(defun cofi/rst-imenu-create-index ()
  (rst-reset-section-caches)
  (cl-flet ((headerp (x) (and (consp x) (stringp (first x)) (markerp (second x)))))
    (cl-loop for subtree in (cdr (rst-section-tree))
             if (and (headerp (first subtree))
                     (consp (rest subtree)))
             collect (cons (first (first subtree))
                           (cofi/rst-create-subtree
                            (cons (list (list "." (second (first subtree))))
                                  (rest subtree))))
             else
             collect (cons (first (first subtree)) (second (first subtree))))))

(add-hook 'rst-mode-hook (lambda ()
                           (setq imenu-create-index-function 'cofi/rst-imenu-create-index)))

(setq ispell-program-name "hunspell"
      ispell-silently-savep t
      ispell-local-dictionary-alist (let ((dicts '((nil "A-Za-z" "en_US")
                                                   ("default" "A-Za-z" "en_US")
                                                   ("english" "A-Za-z" "en_US")
                                                   ("british" "A-Za-z" "en_GB")
                                                   ("german" "A-ZÄÖÜa-zäöüß" "de_DE")
                                                   ("deutsch" "A-ZÄÖÜa-zäöüß" "de_DE"))))
                                      (loop for (name chars dict) in dicts
                                            collect `(,name
                                                      ,(format "[%s]" chars)
                                                      ,(format "[^%s]" chars)
                                                      "[']" t ("-d" ,dict) nil utf-8))))

(defun cofi/flyspell-correct-previous-word (position)
  "Autocorrect previous word and return to current position.
But you can't cycle corrections."
  (interactive "d")
  (unwind-protect
      (flyspell-auto-correct-previous-word position)
   (goto-char position)))

(defkeymap flyspell-mode-map
  "M-n"       'flyspell-goto-next-error
  "C-<tab>"   'cofi/flyspell-correct-previous-word
  "C-M-<tab>" 'cofi/helm-flyspell-correct)

;;; input method ====================
(setq default-input-method 'german-postfix)
(add-hook 'cofi/ispell-change-dictionary-hook
          (defun cofi/update-input-method-after-dictionary-change ()
            (if (member ispell-current-dictionary '("german" "deutsch"))
                (activate-input-method 'german-postfix)
              (when (eq current-input-method 'german-postfix)  ; we changed from a german dictionary
                (deactivate-input-method)))))
(provide 'cofi-write)
