(add-to-list 'load-path "~/.elisp/vendor/google-weather-el")
(require 'org-google-weather)
(setq org-google-weather-icon-directory "/usr/share/icons/oxygen/16x16/status/")

(add-to-list 'load-path "~/.elisp/vendor/org-mode/lisp")
(add-to-list 'load-path "~/.elisp/vendor/org-mode/contrib/lisp")
(require 'org-install)

(if (file-directory-p "~/Org")
    (setq org-directory "~/Org/"
          org-agenda-files "~/Org/agenda")
  (setq org-directory "~/"))

(setq org-refile-targets '((buffer-file-name :maxlevel . 3)
                           (org-agenda-files :maxlevel . 2)
                           )
      org-refile-use-outline-path 'file)

(setq org-completion-use-ido t
      org-outline-path-complete-in-steps nil)

(autoload 'org-agenda-files "org")
(defun cofi/visit-org-agenda-files (fname)
  "Visit agenda files.
Note: This assumes all files are in the org-directory."
  (interactive (list (ido-completing-read "Visit file: "
                                    (mapcar 'file-name-nondirectory (org-agenda-files)))))
  (find-file (concat org-directory fname)))

(autoload 'org-todo "org.el")
(defmacro cofi/set-todo-state (state)
  `(lambda () (interactive)
    (org-todo ,state)))

(defvar cofi/org-state-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "t") (cofi/set-todo-state "TODO"))
    (define-key map (kbd "s") (cofi/set-todo-state "STARTED"))
    (define-key map (kbd "w") (cofi/set-todo-state "WAITING"))
    (define-key map (kbd "f") (cofi/set-todo-state "DEFERRED"))
    ;; 
    (define-key map (kbd "l") (cofi/set-todo-state "DELEGATED"))
    (define-key map (kbd "x") (cofi/set-todo-state "CANCELLED"))
    (define-key map (kbd "d") (cofi/set-todo-state "DONE"))
    map))

(defvar cofi/org-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a") 'org-agenda-list)
    (define-key map (kbd "t") (lambda () (interactive) (org-todo-list 0)))
    (define-key map (kbd "o a") (lambda () (interactive)
                                  (let ((org-indirect-buffer-display 'other-window))
                                    (org-agenda-list))))
    (define-key map (kbd "o t") (lambda () (interactive)
                                  (let ((org-indirect-buffer-display 'other-window))
                                    (org-todo-list 0))))
    (define-key map (kbd "r") 'org-capture)
    (define-key map (kbd "s") cofi/org-state-map)
    (define-key map (kbd "l") 'org-store-link)
    (define-key map (kbd "v") 'cofi/visit-org-agenda-files)
    (define-key map (kbd "c") 'calendar)
    (define-key map (kbd "f") 'org-footnote-action)
    map))
(global-set-key (kbd "<f5>") cofi/org-mode-map)
(global-set-key (kbd "C-c o") cofi/org-mode-map)

(when (fboundp 'anything-other-buffer)
  (defun cofi/anything-org-files ()
    (interactive)
    (anything-other-buffer (cofi/anything-dir-deep "org-files"
                                                   org-directory t
                                                   "\.org\\(_archive\\)?")
                           "*anything org*"))
  (define-key cofi/org-mode-map (kbd "V") 'cofi/anything-org-files))

(add-hook 'org-mode-hook
          (lambda ()
            (org-display-inline-images t)
            (local-set-key (kbd "M-n") 'outline-next-visible-heading)
            (local-set-key (kbd "M-p") 'outline-previous-visible-heading)
            (local-set-key (kbd "C-M-<return>") (lambda ()
                                                  (interactive)
                                                  (end-of-line)
                                                  (org-meta-return)))))

;; Agenda
(setq org-agenda-skip-unavailable-files t
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-done t
      org-agenda-start-on-weekday nil
      org-indirect-buffer-display 'current-window
      org-agenda-restore-windows-after-quit t)

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

(setq org-google-weather-format "%L: %i %c, %l-%h %s")

(setq org-footnote-auto-label 'plain)

(setq org-special-ctrl-a/e t)

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
(setq org-hide-leading-stars t)

;; Tbl
(eval-after-load "org-table"
  '(define-key orgtbl-mode-map (kbd "C-c t") 'orgtbl-insert-radio-table))

;; Babel
(setq org-src-fontify-natively t
      org-src-preserve-indentation t
      org-babel-load-languages '((emacs-lisp . t)
                                 (python . t)
                                 (clojure . t)
                                 (gnuplot . t)
                                 (latex . t)
                                 (sh . t)))

;; Appt
(setq appt-message-warning-time 15 ;; warn 15 min in advance
      appt-display-mode-line t
      appt-display-format 'window)
(appt-activate 1)

(setq appt-disp-window-function
      (lambda (min-to-app new-time msg)
        (progn
          (shell-command (format "appt-file In %s m: %s" min-to-app msg))
          (send-notification msg (format "Appointment in %s minutes" min-to-app)))))
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

(add-hook 'org-capture-mode-hook
          (lambda ()
            (define-key org-capture-mode-map (kbd "C-c C-c")
                        #'cofi/capture-frame-finalize)
            (define-key org-capture-mode-map (kbd "C-c C-k")
                        #'cofi/capture-frame-kill)
            (define-key org-capture-mode-map (kbd "C-c C-w")
                        #'cofi/capture-frame-refile)
            ))

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
         "* %(org-contacts-template-wl-name)
:PROPERTIES:
:EMAIL: %(org-contacts-template-wl-email)
:END:")
        ))
;; ==================================================

;; Exporting
(setq org-emphasis-alist
      '(("*" bold "<b>" "</b>")
        ("/" italic "<i>" "</i>")
        ("_" underline "<span style=\"text-decoration:underline;\">" "</span>")
        ("=" org-code "<code>" "</code>" verbatim)
        ("~" org-verbatim "<code>" "</code>" verbatim)
        ("+" (:strike-through t) "<del>" "</del>")
        ("@" org-warning "<b>" "</b>")))

(setq org-export-latex-emphasis-alist
      '(("*" "\\textbf{%s}" nil)
        ("/" "\\emph{%s}" nil)
        ("_" "\\underline{%s}" nil)
        ("+" "\\st{%s}" nil)
        ("=" "\\verb=%s=" nil)
        ("~" "\\verb~%s~" t)
        ("@" "\\alert{%s}" nil)
        ("$" "\\(%s\\)" nil)))

(setq org-export-latex-listings t)
(setq org-export-latex-default-class "scrartcl")
(setq org-export-latex-default-packages-alist
      '(("AUTO" "inputenc" t)
        ("T1" "fontenc" t)
        ("ngerman" "babel" nil)
        ("" "graphicx" t)
        ("" "textcomp" t)
        ("" "marvosym" t)
        ("" "amsmath" t)
        ("" "amssymb" t)
        ("" "hyperref" nil)
        ("" "soul" t)
        "\\tolerance=1000"))

(eval-after-load "org-latex"
  '(add-to-list 'org-export-latex-classes
               '("scrartcl" "\\documentclass[a4paper,10pt]{scrartcl}"
                ("\\section{%s}"       . "\\section*{%s}")
                ("\\subsection{%s}"    . "\\subsection*{%s}")
                ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                ("\\paragraph{%s}"     . "\\paragraph*{%s}")
                ("\\subparagraph{%s}"  . "\\subparagraph*{%s}"))))

(require 'org-protocol)

;; time sexp ==============================
(defconst cofi-org-ws-boundary
  '(2010 10 18
    2011  2 18))
(defconst cofi-org-ws-pause-weeks
  '(49 50 51 52 1))

(defconst cofi-org-ss-boundary
  '(2011 4 11
    2011 7 15))
(defconst cofi-org-ss-pause-weeks
  '())

(defconst cofi-org-dayname-to-weekday
  '(
    ("So" . 0)
    ("Mo" . 1)
    ("Di" . 2)
    ("Mi" . 3)
    ("Do" . 4)
    ("Fr" . 5)
    ("Sa" . 6)))

(defun cofi-org-term-class (day term-boundary term-pause)
  (let ((weekday (if (stringp day)
                     (cdr (assoc day cofi-org-dayname-to-weekday))
                   day)))
    (cofi-with-sane-calendar
     (eval `(org-diary-class ,@term-boundary ,weekday ,@term-pause)))))

(defun cofi-org-ws-class (day)
  (cofi-org-term-class day cofi-org-ws-boundary cofi-org-ws-pause-weeks))

(defun cofi-org-ss-class (day)
  (cofi-org-term-class day cofi-org-ss-boundary cofi-org-ss-pause-weeks))

(defun cofi-org-ws-cyclic (n year month day &optional mark)
  (cofi-with-sane-calendar
   (let ((cofi-boundary (eval `(diary-block ,@cofi-org-ws-boundary))))
     (cofi-org-cyclic n year month day mark))))

(defun cofi-org-ss-cyclic (n year month day &optional mark)
  (cofi-with-sane-calendar
   (let ((cofi-boundary (eval `(diary-block ,@cofi-org-ss-boundary))))
     (cofi-org-cyclic n year month day mark))))

(defun cofi-org-cyclic (n year month day &optional mark)
  "Uses `ENTRY' and `COFI-BOUNDARY' from outer scope.
Same arguments as in diary cyclic."
    (cofi-with-sane-calendar
     (when (and
            (or (not (boundp 'cofi-boundary)) cofi-boundary)
            (diary-cyclic n year month day mark))
       entry)))

(defmacro cofi-with-sane-calendar (&rest body)
  `(let ((calendar-date-style 'iso))
       ,@body))
;; ========================================
;; contacts ====================
(setq org-contacts-files `(,(format "%s/contacts.org" org-directory)))
(require 'org-contacts)

(require 'std11)
(require 'elmo)
(require 'wl-address)
(require 'wl-summary)

(defun wl-get-from-header-content ()
  (save-excursion
    (set-buffer (org-capture-get :original-buffer))
    (cond
     ((eq major-mode 'wl-summary-mode) (when wl-summary-buffer-elmo-folder
                                         (elmo-message-field
                                          wl-summary-buffer-elmo-folder
                                          (wl-summary-message-number)
                                          'from)))
     ((eq major-mode 'mime-view-mode) (std11-narrow-to-header)
                                      (prog1
                                          (std11-fetch-field "From")
                                        (widen))))))

(defun org-contacts-template-wl-name (&optional return-value)
  (let ((from (wl-get-from-header-content)))
    (or (and from (wl-address-header-extract-realname from))
       return-value
       "%^{Name}")))

(defun org-contacts-template-wl-email (&optional return-value)
  (let ((from (wl-get-from-header-content)))
    (or (and from (wl-address-header-extract-address from))
       return-value
       (concat "%^{" org-contacts-email-property "}p"))))
;;  ========================================
;; Misc ==============================
(setq org-link-mailto-program '(compose-mail "%a" "%s"))
;; ==================================================

(provide 'cofi-org)
