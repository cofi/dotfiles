;; -*- mode: emacs-lisp; mode: rainbow -*-
(require 'cl-lib)
(add-to-loadpath "~/.elisp/vendor/calfw/"
                 "~/.elisp/vendor/org-agenda-property/"
                 "~/.elisp/vendor/org-mode/lisp"
                 "~/.elisp/vendor/org-mode/contrib/lisp")
;;; remove bundled org from load path
(setq load-path (cl-remove-if  (p (string-match "/usr/share/emacs/.*/org" x)) load-path))
(setq org-modules '(org-bibtex org-docview org-gnus org-info org-w3m org-toc org-contacts))

(require 'cofi-autoloads)

(if (file-directory-p "~/Org")
    (setq org-directory "~/Org/"
          org-agenda-files "~/Org/agenda")
  (setq org-directory "~/"))

(require 'org-agenda-property)

(setq org-refile-targets '((buffer-file-name :maxlevel . 3)
                           (org-agenda-files :maxlevel . 2)
                           )
      org-refile-use-outline-path 'file)

(setq org-completion-use-ido t
      org-outline-path-complete-in-steps nil)

(defun cofi/visit-org-agenda-files (fname)
  "Visit agenda files.
Note: This assumes all files are in the org-directory."
  (interactive (list (completing-read "Visit file: "
                                    (mapcar 'file-name-nondirectory (org-agenda-files))
                                    nil t)))
  (find-file (concat org-directory fname)))

(defun cofi/helm-org-files ()
  (interactive)
  (helm-other-buffer (cofi/helm-dir-deep "org-files"
                                                 org-directory t
                                                 "\.org\\(_archive\\)?")
                         "*helm org*"))

(add-hook 'org-mode-hook (lambda () (org-display-inline-images t)))
(add-hook 'org-mode-hook #'visual-line-mode)
(add-hook 'org-mode-hook
          (gen-local-fill-keymap-hook
                                "M-n" 'outline-next-visible-heading
                                "M-p" 'outline-previous-visible-heading
                                "M-d" (cmd done (org-todo "DONE"))
                                "C-c M-g" 'org-toc-show
                                "C-c M-e" 'org-export-as-pdf
                                "C-c C-M-e" 'org-export-as-pdf-and-open
                                "C-M-<return>" (lambda ()
                                                 (interactive)
                                                 (end-of-line)
                                                 (org-meta-return))))

(defun cofi/org-get-full-formatted-outline-path ()
  (if (org-before-first-heading-p)
      ""
      (mapconcat #'identity (append
                             (org-get-outline-path)
                             (list (substring-no-properties (org-get-heading))))
                 " > ")))

(add-hook 'org-mode-hook (lambda ()
                           (setq header-line-format
                                 '((:eval (cofi/org-get-full-formatted-outline-path))))))

;; Agenda
(setq org-agenda-skip-unavailable-files t
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-done t
      org-agenda-skip-scheduled-if-deadline-is-shown t
      org-agenda-start-on-weekday nil
      org-agenda-span 21
      org-indirect-buffer-display 'current-window
      org-agenda-restore-windows-after-quit t
      org-agenda-window-setup 'other-window
      org-agenda-show-all-dates t)

(setq org-agenda-day-face-function
      (lambda (date)
        "`org-agenda-date-weekend'-face for entries in `Vacation' and `Holiday' category."
        (unless (org-agenda-todayp date)
          (dolist (file (org-agenda-files nil 'ifmode))
            (let ((face
                   (dolist (entry (org-agenda-get-day-entries file date))
                     (let ((category (with-temp-buffer
                                       (insert entry)
                                       (org-get-category (point-min)))))
                       (when (or (string= "Holidays" category)
                                (string= "Vacation" category))
                         (return 'org-agenda-date-weekend))))))
              (when face (return face)))))))

(defun cofi/agenda-frame ()
  (modify-frame-parameters nil
                           '( (name . "Agenda Frame")
                              (width . 80)
                              (height . 15)
                              (vertical-scroll-bars . nil)
                              (menu-bar-lines . nil)
                              (tool-bar-lines . nil)))
  (if (fboundp 'x-focus-frame)
      (x-focus-frame nil))
  (let ((org-agenda-window-setup 'current-window))
    (org-agenda-list)))

(add-hook 'org-agenda-mode-hook (lambda ()
                                  (if (string= (frame-parameter nil 'name)
                                               "Agenda Frame")
                                      (cofi/set-key 'local "x" (cmd agenda-frame-exit (org-agenda-exit)
                                                                    (delete-frame)))
                                    (cofi/set-key 'local "x" 'org-agenda-exit))))

(setq org-google-weather-format "%L: %i %c, %l-%h %s")

(setq org-footnote-auto-label 'plain
      org-footnote-auto-adjust t)

(setq org-special-ctrl-a/e t)

(setq org-use-speed-commands t)
(defun cofi/evil-org-select-speed-command (char)
  (interactive "c")
  (let* ((default (assoc (string char) org-speed-commands-default))
         (user (assoc (string char) org-speed-commands-user))
         (cmd (or (cdr default) (cdr user))))
    (when cmd
      (cond
       ((commandp cmd) (call-interactively cmd))
       ((listp cmd) (eval cmd))))))

(eval-after-load "evil"
  '(evil-define-key 'normal org-mode-map (kbd "C-'") #'cofi/evil-org-select-speed-command))

;; ToDo
(setq org-todo-keywords '((sequence
                          "TODO"
                          "STARTED"
                          "WAITING"
                          "DEFERRED"
                          "|"           ; Separator, no more action necessary
                          "DELEGATED"
                          "CANCELLED"
                          "DONE"
                          )))

(setq org-todo-keyword-faces
      '(("TODO" . org-warning)
        ("STARTED" . "yellow")
        ("WAITING" . "sienna")
        ("DEFERRED" . "orange")
        ("DELEGATED" . "slate blue")
        ("CANCELLED" . "dark violet")
        ("DONE" . "green")))

(setq org-hide-leading-stars t)

;; Tbl
(eval-after-load "org-table"
  '(define-key orgtbl-mode-map (kbd "C-c t") 'orgtbl-insert-radio-table))

;; Babel
(setq org-src-fontify-natively t
      org-src-preserve-indentation t)

;; Appt
(setq appt-message-warning-time 15 ;; warn 15 min in advance
      appt-display-mode-line t
      appt-display-format 'window)
(appt-activate 1)

(require 'notifications)
(defvar cofi/org-appt-id nil)
;;; FIXME: Update for emacs24
(setq appt-disp-window-function
      (lambda (min-to-app new-time msg)
        (unless (string< min-to-app (number-to-string (- appt-message-warning-time 5)))
          (setq cofi/org-appt-id nil))
        (shell-command (format "appt-file In %s m: %s" min-to-app msg))
        (setq cofi/org-appt-id
              (notifications-notify :title (format "Appointment in %s minutes" min-to-app)
                                    :body msg
                                    :app-name "Org appointment"
                                    :replaces-id cofi/org-appt-id))))

(setq appt-delete-window-function
      (lambda ()
        (shell-command "appt-file -clear")))
(add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt)

;; Capture ==============================
(setq org-default-notes-file (concat org-directory "capture.org"))

;; Adapted from http://emacs-fu.blogspot.com/2009/04/remember.html
(defun cofi/capture-frame ()
  (modify-frame-parameters nil
                           '( (name . "Capture Frame")
                              (width . 80)
                              (height . 15)
                              (vertical-scroll-bars . nil)
                              (menu-bar-lines . nil)
                              (tool-bar-lines . nil)))
  (if (fboundp 'x-focus-frame)
      (x-focus-frame nil))
  (org-capture)
  (linum-mode -1)
  (delete-other-windows))

(defun cofi/capture-wrap (fun)
  "Wraps the call to `fun' with actions needed if frame is a capture frame."
  (let ((capture-frame? (string= (frame-parameter nil 'name) "Capture Frame")))
    (if capture-frame?
        (make-frame-invisible))
    (funcall fun)
    (if capture-frame?
        (delete-frame))))

(defun cofi/capture-frame-finalize ()
  "Special treatment for capture frames when finalizing."
  (interactive)
  (cofi/capture-wrap #'org-capture-finalize))

(defun cofi/capture-frame-kill ()
  "Special treatment for capture frames when killing."
  (interactive)
  (cofi/capture-wrap #'org-capture-kill))

(defun cofi/capture-frame-refile ()
  "Special treatment for capture frames when refiling."
  (interactive)
  (cofi/capture-wrap #'org-capture-refile))

(eval-after-load "org-capture"
  '(fill-keymap org-capture-mode-map
     "C-c C-c" #'cofi/capture-frame-finalize
     "C-c C-k" #'cofi/capture-frame-kill
     "C-c C-w" #'cofi/capture-frame-refile))

(setq org-capture-templates
      '(("t" "Todo" entry (file (format "%s/todo.org" org-directory))
         "* TODO %?\n  %i\n  %a")
        ("x" "Note with Clipboard" entry (file (format "%s/notes.org" org-directory))
         "* %?\n  %i\n  %x")
        ("n" "Note" entry (file (format "%s/notes.org" org-directory))
         "* %?\n  %i\n  %a")
        ;; dedicated templates
        ("s" "Save link for reading" entry (file+headline
                                            (format "%s/links.org" org-directory)
                                            "Unsorted")
         "* %:description\n  %:link\n  %U"
         )
        ("c" "Contacts" entry (file (format "%s/contacts.org" org-directory))
         "* %(org-contacts-template-name)
:PROPERTIES:
:EMAIL: %(org-contacts-template-email)
:END:")
        ))
;; ==================================================

;; Exporting
(setq org-hide-emphasis-markers t)
(setq org-export-latex-listings t)
(setq org-export-latex-default-class "koma-article")

(setq org-latex-pdf-process '("latexmk -bibtex -pdf %b"))

(add-hook 'org-mode-hook 'reftex-mode)

(require 'org-protocol)

;; time sexp ==============================
(defvar cofi-boundary nil)
(defconst cofi-org-ws-boundary
  '(2013 10 14
    2014  2 14))
(defconst cofi-org-ws-pause-weeks
  '(52 1))

(defconst cofi-org-ss-boundary
  '(2013 4 15
    2013 7 19))

(defconst cofi-org-ss-pause-weeks
  '())

(defconst cofi-org-dayname-to-weekday
  ["So" "Mo" "Di" "Mi" "Do" "Fr" "Sa"])

(defun cofi-org-term-class (day term-boundary term-pause)
  (let ((weekday (if (stringp day)
                     (find-index day cofi-org-dayname-to-weekday)
                   day)))
    (apply #'org-class `(,@term-boundary ,weekday ,@term-pause))))

(defun cofi-org-ws-class (day)
  (cofi-org-term-class day cofi-org-ws-boundary cofi-org-ws-pause-weeks))

(defun cofi-org-ss-class (day)
  (cofi-org-term-class day cofi-org-ss-boundary cofi-org-ss-pause-weeks))

(defun cofi-org-ws-cyclic (n year month day &optional mark)
  (let ((cofi-boundary (apply #'org-block cofi-org-ws-boundary)))
    (cofi-org-cyclic n year month day mark)))

(defun cofi-org-ss-cyclic (n year month day &optional mark)
  (let ((cofi-boundary (apply #'org-block cofi-org-ss-boundary)))
    (cofi-org-cyclic n year month day mark)))

(defun cofi-org-cyclic (n year month day &optional mark)
  "Uses `COFI-BOUNDARY' from outer scope.
Same arguments as in diary cyclic."
  (and cofi-boundary
       (org-cyclic n year month day mark)))
;; ========================================
;; contacts ====================
(setq org-contacts-files `(,(format "%s/contacts.org" org-directory)))
;;  ========================================
;; Misc ==============================
(setq org-link-mailto-program '(compose-mail "%a" "%s"))

;;; weather
(add-to-loadpath "~/.elisp/vendor/google-weather-el")
(require 'org-google-weather)
(setq org-google-weather-icon-directory "/usr/share/icons/oxygen/16x16/status/")
;; ==================================================
(setq org-journal-dir "~/Journal/")

(provide 'cofi-org)
