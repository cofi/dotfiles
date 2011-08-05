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

(defun cofi/inc-at-pt (amount)
  "Increment the number at point by `amount'"
  (interactive "p")
  (let* ((old-pos (point))
         (number-str "-.0123456789")
         (end (progn (skip-chars-forward number-str)
                     (point)))
         (start (progn (skip-chars-backward number-str)
                     (point)))
         (number (string-to-number (buffer-substring-no-properties start end))))
    (when (numberp number)
      (delete-region start end)
      (insert (number-to-string (+ number amount)))
      (goto-char old-pos))))

(defun cofi/dec-at-pt (amount)
  "Decrement the number at point by `amount'"
  (interactive "p")
  (cofi/inc-at-pt (- (abs amount))))

(defun toggle-comment-on-line-or-region ()
  "Comments or uncomments current current line or whole lines in region."
  (interactive)
  (if (region-active-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position)
                                 (line-end-position))))

(defadvice comment-region (before copy-region-on-comment (beg end &optional arg) activate)
  "Copy commented region before commenting."
  (copy-region-as-kill beg end))

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
                  (c++-mode  . ";")
                  (java-mode . ";")
                  (js-mode   . ";")))
         (char (or (cdr (assq major-mode modes))
                   ""))
         (p (point)))
    (move-end-of-line nil)
    (insert char)
    (goto-char p)))

(defvar cofi/switched-from nil)
(make-variable-buffer-local 'cofi/switched-from)

(defun cofi/open-header (fname)
  "Open associated .h for `fname' and set `fname' as cofi-switched-from."
  (find-file (replace-regexp-in-string
              "\\(.*\\)\\..+\\'" "\\1.h"
              fname t))
  (setq cofi/switched-from fname))

(defun cofi/switch-file ()
  "Switch to file associated to current file.
Major mode determines association."
  (interactive)
  (let* ((modes '((c-mode    . (lambda () (cofi/open-header buffer-file-name)))
                  (c++-mode  . (lambda () (cofi/open-header buffer-file-name)))))
         (fun (cdr (assq major-mode modes))))
    (if (boundp 'cofi-switched-from)
        (find-file cofi-switched-from)
      (if fun
          (funcall fun)))))

(defun count-words (&optional start end)
  "Count words in region."
  (interactive "r")
  (message (format "Words: %s" (count-matches "\\w+" start end))))

(defun count-words-buffer ()
  "Count words in buffer."
  (interactive)
  (count-words (point-min) (point-max)))

(defvar cofi-file-assoc (cofi/make-ring [cofi-file-standard cofi-file-alt]))
(defun cofi-next-file-assoc ()
  "Move to next set of file associations."
  (interactive)
  (funcall (cofi/ring-next cofi-file-assoc)))

(defun cofi-file-standard ()
  (setq org-file-apps '((auto-mode . emacs)
                        ("\.x?html?" . default)
                        ("\.pdf" . default)
                        ("\.pdf::\\([0-9]+\\)" . "okular --page=%1 %s")))
  (setq TeX-view-program-selection '((output-dvi "xdg-open")
                                     (output-pdf "xdg-open")
                                     (output-html "xdg-open")))
  (message "Using standard file openers."))

(defun cofi-file-alt ()
  (setq org-file-apps '((auto-mode . emacs)
                        ("\.x?html?$" . "conkeror %s")
                        ("\.pdf" . "zathura %s")
                        ("\.pdf::\\([0-9]+\\)" . "okular --page=%1 %s")))
  (setq TeX-view-program-selection '((output-dvi "xdg-open")
                                     (output-pdf "zathura")
                                     (output-html "conkeror")))
  (message "Using alternative file openers."))

(defun cofi/cdlatex (prefix)
  (interactive "P")
  (if (or (in-mode? 'org-mode)
         orgstruct-mode)
      (call-interactively 'org-cdlatex-mode nil)
    (call-interactively 'cdlatex-mode nil)))

(defun highlight-80 ()
  (interactive)
  (column-marker-1 80))

(defvar cofi/colorschemes (cofi/make-ring [cofi-dark cofi-light]))
(defun cofi/next-colorscheme (&optional arg)
  "Move to next colorscheme. If `ARG' is non-nil reload current."
  (interactive "P")
  (disable-theme (cofi/ring-current cofi/colorschemes))
  (load-theme
   (if arg
       (cofi/ring-current cofi/colorschemes)
     (cofi/ring-next cofi/colorschemes))))

(defun cofi/copy-sha1-of-buffer (buffer &optional start end)
  (interactive "b")
  (kill-new (sha1 buffer start end)))

(defun NOP ()
  "Non-op."
  t)

(push '("^\\*Pp Eval Output\\*" . View-mode) auto-mode-alist)

(provide 'cofi-func)
