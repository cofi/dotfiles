(require 'cofi-util)
(require 'cl-lib)

(setq-default c-basic-offset 4)
(setq c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
                        (c-mode . "linux")))

(setq comment-multi-line t)

(defun cofi-c-line-comments ()
  (setq comment-start "//"
        comment-end ""))
(add-hook 'c-mode-hook 'cofi-c-line-comments)

(defun cofi/maybe-change-c-to-c++ ()
  (when (and (buffer-file-name) (ends-with (buffer-file-name) ".h"))
    (let* ((name (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))
           (siblings (ls-files "." nil name)))
      (when (cl-find-if (lambda (f)
                          (string-match (gen-extension-re "cc" "c++" "cpp" "cxx") (downcase f)))
                        siblings)
        (c++-mode)))))

(add-hook 'c-mode-hook #'cofi/maybe-change-c-to-c++)

(require-and-exec 'c-eldoc
  (add-hook 'c-mode-hook 'c-turn-on-eldoc-mode))
(provide 'cofi-c)
