(defun require-and-exec (feature fun)
  "Require the feature and call fun if it was successfull loaded."
  (if (require feature nil 'noerror)
      (when fun
        (fun))
    (message (format "%s not loaded" feature))))

(defun require-pair (pair)
  "Unpack a pair and call `require-and-exec' on it."
  (require-and-exec
   (car pair)
   (cadr pair)))

(provide 'cofi-util)
