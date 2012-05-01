
(add-to-list 'auto-mode-alist (cons (gen-extension-re "html" "htm") 'nxml-mode))
;;; add html5 schemas
(require-and-exec 'rng-loc
  (push (cofi/var-file "schema/schemas.xml") rng-schema-locating-files))

(setq nxml-slash-auto-complete-flag t)

(eval-after-load "nxml-mode"
  '(progn
     (defun nxml-which-xpath ()
       (let (path)
         (save-excursion
           (save-restriction
             (widen)
             (while (condition-case nil
                        (progn
                          (nxml-backward-up-element)
                          t)
                      (error nil))
               (push (xmltok-start-tag-local-name) path)))
           (concat "/" (mapconcat 'identity path "/")))))

     (eval-after-load "which-func"
       '(pushnew nxml-which-xpath which-func-functions))))

(provide 'cofi-xml)
