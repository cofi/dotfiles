(defun annotate-markers ()
  "Put fringe marker on marker lines in the current buffer"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\<\\(FIXME\\|TODO\\|XXX+\\|BUG\\):" nil t)
      (let ((overlay (make-overlay (- (point) 5) (point))))
        (overlay-put overlay 'before-string (propertize "A"
                                                        'display '(left-fringe right-triangle)))))))

;; TODO: add function to remove overlays

(defun highlight-markers ()
  "Adds fontlocks for markers."
  (font-lock-add-keywords nil
                          '(("\\<\\(FIXME\\|TODO\\|XXX+\\|BUG\\):"
                             1 font-lock-warning-face prepend))))

;; TODO: add function to display all markers in buffer in another one

(add-hook 'find-file-hook 'highlight-markers)
(add-hook 'find-file-hook 'annotate-markers)

(provide 'cofi-markers)
