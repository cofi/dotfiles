(defconst marker-regexp "\\<\\(HACK\\|FIXME\\|TODO\\|XXX+\\|BUG\\):"
  "Regexp that matches the markers.")

(defun annotate-markers ()
  "Put fringe marker on marker lines in the current buffer"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward marker-regexp nil t)
      (let ((overlay (make-overlay (- (point) 5) (point))))
        (overlay-put overlay 'before-string (propertize "A"
                                                        'display '(left-fringe right-triangle)))))))

;; TODO: add function to remove overlays

(defun highlight-markers ()
  "Adds fontlocks for markers."
  (font-lock-add-keywords nil
                          '((marker-regexp 1 font-lock-warning-face prepend))))

(defun list-markers ()
  "List all markers in current buffer."
  (interactive)
  (occur marker-regexp 0))

(add-hook 'find-file-hook 'highlight-markers)
(add-hook 'find-file-hook 'annotate-markers)

(provide 'cofi-markers)
