(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun recentf-ido-find-file ()
  "Find a recent file using Ido."
  (interactive)
  (let* ((file-assoc-list
          (mapcar (lambda (x)
                    (cons (file-name-nondirectory x)
                          x))
                  recentf-list))
         (filename-list
          (remove-duplicates (mapcar #'car file-assoc-list)
                             :test #'string=))
         (filename (ido-completing-read "Choose recent file: "
                                        filename-list
                                        nil
                                        t)))
    (when filename
      (find-file (cdr (assoc filename
                             file-assoc-list))))))

(defun ac-flyspell-prog-mode ()
  "Activate Flyspell prog mode and call ac workaround."
  (interactive)
  (flyspell-prog-mode)
  (auto-complete-mode 1)
  (ac-flyspell-workaround))

(defun ac-flyspell-mode ()
  "Activate Flyspell mode and call ac workaround."
  (interactive)
  (flyspell-mode 1)
  (auto-complete-mode 1)
  (ac-flyspell-workaround))

(defun speck-prog-mode (&optional arg)
  "Turn speck mode on and make it aware of syntax."
  (interactive "P")
  (set (make-local-variable 'speck-syntactic) t)
  (speck-mode arg))

(defun turn-on-speck ()
  "Turn speck mode on."
  (interactive)
  (speck-mode 1))

(defun turn-on-speck-prog ()
  "Turn programming speck mode on."
  (interactive)
  (speck-prog-mode 1))

(defun cofi/inc-at-pt (amount)
  "Increment the number at point by `amount'"
  (interactive "p")
  (let* ((old-pos (point))
         (number-str "-.0123456789")
         (end (progn (skip-chars-forward number-str)
                     (point)))
         (start (progn (skip-chars-backward number-str)
                     (point)))
         (number (string-to-int (buffer-substring-no-properties start end))))
    (when (numberp number)
      (delete-region start end)
      (insert (number-to-string (+ number amount)))
      (goto-char old-pos))))

(defun cofi/dec-at-pt (amount)
  "Decrement the number at point by `amount'"
  (interactive "p")
  (cofi/inc-at-pt (- (abs amount))))

(defun comment-or-uncomment-current-line-or-region ()
  "Comments or uncomments current current line or whole lines in region."
  (interactive)
  (save-excursion
    (let (min max)
      (if (region-active-p)
          (setq min (region-beginning) max (region-end))
        (setq min (point) max (point)))
      (comment-or-uncomment-region
       (progn (goto-char min) (line-beginning-position))
       (progn (goto-char max) (line-end-position))))))

(defun cofi/macro-dwim (arg)
  "Start, end, execute or clear macro.
Call with 0 to clear last macro. If there is no last macro define a new one, if\
currently defining macro end and it and if there is a last macro call that.
Note if `arg' is 1 (that is called without prefix or numeric-argument 1), that
will not be passed to `start-kbd-macro'."
  (interactive "p")
  (if (= arg 0)
      (setq last-kbd-macro nil)
    (if defining-kbd-macro
        (end-kbd-macro arg)
      (if last-kbd-macro
          (call-last-kbd-macro arg)
        (if (= arg 1)
            (start-kbd-macro nil)
          (start-kbd-macro arg))))))

(defun cofi/reset-macro ()
  "Reset kbd-macro."
  (interactive)
  (cofi/macro-dwim 0))

(defconst cofi-alias-file (expand-file-name "~/config/diralias")
  "File contains line separated entries of `DIR' `ALIAS'.
Neither dir nor alias may contain spaces.")
(defconst cofi-aliases
  (with-temp-buffer
    (insert-file-contents cofi-alias-file)
    (mapcar (lambda (line)
              (let ((split (split-string line " ")))
                (cons (cadr split) (car split))))
            (split-string (buffer-string) "\n" t))))

(defun cofi-cd-alias (alias)
  "Change directory to aliased one."
  (interactive (list (ido-completing-read "Alias: "
                                          (mapcar #'car
                                                  cofi-aliases) nil t)))
  (let ((dir (cdr (assoc alias cofi-aliases))))
    (cd dir)))

(defun cofi-dired-alias (alias)
  "Open dired on aliased directory."
  (interactive (list (ido-completing-read "Alias: "
                                          (mapcar #'car
                                                  cofi-aliases) nil t)))
  (let ((dir (cdr (assoc alias cofi-aliases))))
    (dired dir)))

(defun cofi-find-at-alias (alias)
  "Find file in aliased directory."
  (interactive (list (ido-completing-read "Alias: "
                                          (mapcar #'car
                                                  cofi-aliases) nil t)))
  (let ((dir (cdr (assoc alias cofi-aliases))))
    (ido-find-file-in-dir dir)))

;; Taken from http://www.emacswiki.org/emacs/ArtistMode
(defun artist-ido-select-operation (type)
  "Use ido to select a drawing operation in artist-mode"
  (interactive
   (list (ido-completing-read "Drawing operation: " 
           (list "Pen" "Pen Line" "line" "straight line" "rectangle" 
                 "square" "poly-line" "straight poly-line" "ellipse" 
                 "circle" "text see-thru" "text-overwrite" "spray-can" 
                 "erase char" "erase rectangle" "vaporize line" "vaporize lines" 
                 "cut rectangle" "cut square" "copy rectangle" "copy square" 
                 "paste" "flood-fill"))))
  (artist-select-operation type))

(defun artist-ido-select-settings (type)
  "Use ido to select a setting to change in artist-mode"
  (interactive
   (list (ido-completing-read "Setting: " 
           (list "Set Fill" "Set Line" "Set Erase" "Spray-size" "Spray-chars" 
                 "Rubber-banding" "Trimming" "Borders"))))
  (if (equal type "Spray-size") 
      (artist-select-operation "spray set size")
    (call-interactively (artist-fc-get-fn-from-symbol 
                         (cdr (assoc type '(("Set Fill" . set-fill)
                                            ("Set Line" . set-line)
                                            ("Set Erase" . set-erase)
                                            ("Rubber-banding" . rubber-band)
                                            ("Trimming" . trimming)
                                            ("Borders" . borders)
                                            ("Spray-chars" . spray-chars))))))))

(defun cofi/end-prog-line ()
  "End the physical line with modes statement end char."
  (interactive)
  (let* ((modes '((c-mode    . ";")
                  (cpp-mode  . ";")
                  (java-mode . ";")
                  (js-mode   . ";")))
         (char (or (cdr (assq major-mode modes))
                   ""))
         (p (point)))
    (move-end-of-line nil)
    (insert char)
    (goto-char p)))

(provide 'cofi-func)
