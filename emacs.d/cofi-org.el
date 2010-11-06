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
    (define-key map (kbd "s") cofi/org-state-map)
    (define-key map (kbd "t") (lambda () (interactive) (org-todo-list 0)))
    (define-key map (kbd "l") 'org-store-link)
    (define-key map (kbd "v") 'cofi/visit-org-agenda-files)
    (define-key map (kbd "c") 'calendar)
    (define-key map (kbd "f") 'org-footnote-action)
    map))
(global-set-key (kbd "<f5>") cofi/org-mode-map)
(global-set-key (kbd "C-c o") cofi/org-mode-map)

(add-hook 'org-mode-hook
          (lambda ()
            (org-display-inline-images t)
            (local-set-key (kbd "M-n") 'outline-next-visible-heading)
            (local-set-key (kbd "M-p") 'outline-previous-visible-heading)
            (local-set-key (kbd "C-M-<return>") (lambda ()
                                                  (interactive)
                                                  (end-of-line)
                                                  (org-meta-return)))))

(setq org-file-apps '((auto-mode . emacs)
                      ("\.x?html?" . default)
                      ("\.pdf" . default)
                      ("\.pdf::\\([0-9]+\\)" . "okular --page=%1 %s")))

;; Agenda
(setq org-agenda-skip-unavailable-files t
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-done t
      org-agenda-start-on-weekday nil
      )

(setq org-footnote-auto-label 'plain)

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

;; Appt
(setq appt-message-warning-time 15 ;; warn 15 min in advance
      appt-display-mode-line t
      appt-display-format 'window)
(appt-activate 1)

(setq appt-disp-window-function
      (lambda (min-to-app new-time msg)
        (send-notification msg (format "Appointment in %s minutes" min-to-app))))
(add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt)

;; Capture ==============================
(setq org-default-notes-file (concat org-directory "remember.org"))
(global-set-key (kbd "C-c r") 'org-capture)

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
    (cofi/capture-wrap (function org-capture-finalize)))

(defun cofi/capture-frame-kill ()
  "Special treatment for capture frames when killing."
  (interactive)
    (cofi/capture-wrap (function org-capture-kill)))

(defun cofi/capture-frame-refile ()
  "Special treatment for capture frames when killing."
  (interactive)
    (cofi/capture-wrap (function org-capture-refile)))

(add-hook 'org-capture-mode-hook
          (lambda ()
            (define-key org-capture-mode-map (kbd "C-c C-c")
                        (function cofi/capture-frame-finalize))
            (define-key org-capture-mode-map (kbd "C-c C-k")
                        (function cofi/capture-frame-kill))
            (define-key org-capture-mode-map (kbd "C-c C-w")
                        (function cofi/capture-frame-refile)
              )))

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline (format "%s/todo.org" org-directory) "Tasks")
         "* TODO %?\n  %i\n  %a")
        ("x" "Note with Clipboard" entry (file (format "%s/notes.org" org-directory))
         "* %?\n  %i\n  %x")
        ("n" "Note" entry (file (format "%s/notes.org" org-directory))
         "* %?\n  %i\n  %a")
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
        ("@" "\\alert{%s}" nil)))

(setq org-export-latex-listings t)

(require 'org-protocol)

(provide 'cofi-org)
