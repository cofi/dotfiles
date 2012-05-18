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

(defun* cofi/format-binary (number &optional width (fillchar ?0))
  "Format `NUMBER' as binary.
Fill up to `WIDTH' with `FILLCHAR' (defaults to ?0) if binary
representation of `NUMBER' is smaller."
  (let (nums)
    (do ((num number (truncate num 2)))
        ((= num 0))
      (push (number-to-string (% num 2)) nums))
    (let ((len (length nums)))
      (apply #'concat
             (if (and width (< len width))
                 (make-string (- width len) fillchar)
               "")
             nums))))

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

(defun cofi-find-helm-at-alias (alias)
  "Find file in aliased directory."
  (interactive (list (helm-completing-read-default "Alias: "
                                                  (mapcar #'car
                                                          cofi-aliases) nil t)))
  (let ((default-directory (cdr (assoc alias cofi-aliases))))
    (helm-find-files nil)))

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
  (let* ((modes '((c-mode    . (f-alt #'eassist-switch-h-cpp (lambda () (cofi/open-header buffer-file-name)))
                  (c++-mode  . (f-alt #'eassist-switch-h-cpp (lambda () (cofi/open-header buffer-file-name))))))
         (fun (cdr (assq major-mode modes))))
    (if (boundp 'cofi-switched-from)
        (find-file cofi-switched-from)
      (if fun
          (funcall fun)))))

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

(defvar cofi/current-colorscheme nil)
(defvar cofi/colorschemes '("cofi-dark" "cofi-light"))
(defun cofi/colorscheme (scheme)
  "Move to next colorscheme. If `ARG' is non-nil reload current."
  (interactive (list (completing-read "Colorscheme: " cofi/colorschemes)))
  (setq scheme (if (stringp scheme)
                   (intern scheme)
                 scheme))
  (disable-theme cofi/current-colorscheme)
  (load-theme scheme)
  (setq cofi/current-colorscheme scheme))

(defun cofi/copy-sha-of-buffer (buffer)
  (interactive "b")
  (let ((buffer (if (typep buffer 'buffer)
                    buffer
                  (get-buffer buffer))))
    (kill-new (secure-hash 'sha256 (current-buffer)))))

(defun NOP ()
  "Non-op."
  t)

(defun toggle-region-read-only (begin end)
  "Toggle read-only on region and set face to `italic'."
  (interactive "r")
  (let ((read-only-properties '(read-only t face italic))
        (remove (text-property-any begin end 'read-only t)))
    (if remove
        (let ((inhibit-read-only t))
          (remove-text-properties begin end read-only-properties))
      (add-text-properties begin end read-only-properties))))

(defun cofi-full-calc ()
  (interactive)
  (calc nil t)
  (set-frame-name "calc"))

(defun cofi-dwim-bol (force-to-indentation)
  "Move to bol, indentation or previous line depending on point.

If point is bolp move to previous line,
if point is at indentation move to bol,
else move to indentation.

With a prefix argument you can force to move back to indentation."
  (interactive "P")
  (if force-to-indentation
      (back-to-indentation)
    (cond
     ((bolp) (forward-line -1))
     ((= (point)
         (save-excursion
           (back-to-indentation)
           (point)))
      (beginning-of-line))
     (t (back-to-indentation)))))

(defun cofi-update-all-buffers ()
  "Update all buffers that are visiting a file, have no unsaved changes but are
modifier outside of Emacs."
  (interactive)
  (loop for buffer in (buffer-list)
        when (and (buffer-file-name buffer)
                  (not (buffer-modified-p buffer))
                  (not (verify-visited-file-modtime buffer)))
        do (with-current-buffer buffer
             (revert-buffer t t t))))

(defun cofi/mail-return-keep-citation-markers (dont-fill)
  "RETURN that keeps citation markers.
Fill new sentence unless called with prefix or was at eol."
  (interactive "P")
  (let* ((bol (point-at-bol))
         (s (buffer-substring bol (point)))
         (match (save-match-data
                  (if (string-match "^\\(>+ \\)" s)
                      (match-string 1 s)))))
    (if (and  (/= (point) (point-at-eol)) match)
        (progn
          (newline)
          (insert match)
          (save-excursion
            (unless dont-fill
              (fill-region-as-paragraph (point-at-bol) (progn (forward-sentence)
                                                              (point))))
            (delete-trailing-whitespace bol (point))))
      (newline))))

(provide 'cofi-func)
