(defconst marker-regexp "\\<\\(HACK\\|FIXME\\|TODO\\|XXX+\\|BUG\\):"
  "Regexp that matches the markers.")
(when (require 'fringe-helper nil 'noerror)
  (defvar marker-overlays '())

  (defun annotate-markers ()
    "Put fringe marker on marker lines in the current buffer"
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward marker-regexp nil t)
        (push (fringe-helper-insert (fringe-lib-load fringe-lib-wave)
                                    (point)
                                    'left-fringe
                                    'font-lock-warning-face)
              marker-overlays)
        )))

  (defun clean-marker-annotations ()
    "Remove the overlay annotations."
    (interactive)
    (mapc 'fringe-helper-remove marker-overlays))

  (defun refresh-marker-annotations ()
    "Refresh the marker annotations."
    (interactive)
    (progn
      (clean-marker-annotations)
      (annotate-markers)))
  )

(defun marker-fontlock ()
  (font-lock-add-keywords nil
                          (list (list marker-regexp
                                      1 'font-lock-warning-face 'prepend))))

(defun list-markers ()
  "List all markers in current buffer."
  (interactive)
  (occur marker-regexp 0))

(add-hook 'find-file-hook 'annotate-markers)
(add-hook 'find-file-hook 'marker-fontlock)

(provide 'cofi-markers)
