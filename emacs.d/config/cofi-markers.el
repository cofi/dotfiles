(setq fic-highlighted-words '( "HACK" "FIXME" "TODO" "XXX" "BUG"))

(require-and-exec 'fic-mode
   (add-to-hooks 'fic-mode `(,@programming-hooks rst-mode-hook TeX-mode-hook org-mode-hook)))

(defconst marker-regexp "\\<\\(HACK\\|FIXME\\|TODO\\|XXX+\\|BUG\\):"
  "Regexp that matches the markers.")

(require-and-exec 'fringe-helper
                  (defvar marker-fringe-overlays nil)
                  (make-variable-buffer-local 'marker-fringe-overlays)

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
                              marker-fringe-overlays)
                        )))

                  (defun clean-marker-annotations ()
                    "Remove the marker annotations."
                    (interactive)
                    (mapc 'fringe-helper-remove marker-fringe-overlays))

                  (defun refresh-marker-annotations ()
                    "Refresh the marker annotations."
                    (interactive)
                    (progn
                      (clean-marker-annotations)
                      (annotate-markers)))

                  (add-hook 'after-save-hook 'refresh-marker-annotations)

                  ;; fringe markers for flymake
                  (require-and-exec 'flymake
                                    (defvar flymake-fringe-overlays nil)
                                    (make-variable-buffer-local 'flymake-fringe-overlays)

                                    (defadvice flymake-make-overlay (after add-to-fringe first
                                                                           (beg end tooltip-text face mouse-face)
                                                                           activate compile)
                                      (push (fringe-helper-insert-region
                                             beg end
                                             (fringe-lib-load (if (eq face 'flymake-errline)
                                                                  fringe-lib-exclamation-mark
                                                                fringe-lib-question-mark))
                                             'left-fringe 'font-lock-warning-face)
                                            flymake-fringe-overlays))

                                    (defadvice flymake-delete-own-overlays (after remove-from-fringe activate
                                                                                  compile)
                                      (mapc 'fringe-helper-remove flymake-fringe-overlays)
                                      (setq flymake-fringe-overlays nil))
                                    )
                  (add-hook 'find-file-hook 'annotate-markers))

(defun marker-fontlock ()
  (font-lock-add-keywords nil (list (list marker-regexp 1
                                            'font-lock-warning-face 'prepend))))
(when (not (locate-library "fic-mode.el"))
  (add-hook 'find-file-hook 'marker-fontlock))

(defun list-markers ()
  "List all markers in current buffer."
  (interactive)
  (occur marker-regexp 0))


(provide 'cofi-markers)
