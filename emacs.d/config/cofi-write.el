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
(defun cofi/update-input-method-after-dictionary-change ()
  (if (member ispell-current-dictionary '("german" "deutsch"))
      (activate-input-method 'german-postfix)
    (when (eq current-input-method 'german-postfix)  ; we changed from a german dictionary
      (deactivate-input-method))))

(add-hook 'cofi/ispell-change-dictionary-hook #'cofi/update-input-method-after-dictionary-change)

(defadvice flyspell-mode (after set-input-method activate)
  (when flyspell-mode
    (cofi/update-input-method-after-dictionary-change)))

(provide 'cofi-write)
