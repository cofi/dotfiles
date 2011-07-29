;;; as-external.el --- Emacs as an external editor to other apps
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: Mon Jun 25 19:02:49 2007
(defconst as-external:version "0.6") ;;Version:
;; Last-Updated: 2009-08-04 Tue
;; URL:
;; Keywords:
;; Compatibility:
;;
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  This little library should make it easier to use Emacs as an
;;  external editor in certain cases. One such case is when want to
;;  use Emacs as the external editor with the Firefox add-on "It's All
;;  Text".
;;
;;  See variable `as-external-mode' for more information.
;;
;;
;;; A note on the implementation:
;;
;; You may wonder why this does not use `auto-mode-alist' since it
;; checks the file name in nearly the same way? It is perhaps possible
;; to use that, but there are two things to be aware of:
;;
;; 1. The choice made must override other possible choices.
;;
;; 2. Beside the file name the implementation here also checks if the
;;    buffer has clients waiting. That makes the check more reliable.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(defconst as-ext-load-time-start (float-time))

;;(message " as-ext a %.1f seconds elapsed" (- (float-time) as-ext-load-time-start))
(eval-when-compile (require 'cl))
(eval-when-compile (require 'edit-server nil t))
(eval-when-compile (require 'html-write nil t))
(eval-when-compile (require 'mlinks nil t))
(eval-when-compile (require 'mumamo nil t))
(eval-when-compile (require 'nxhtml-mode nil t))
(eval-when-compile (require 'ourcomments-util nil t))
(eval-when-compile (require 'pause nil t))
(eval-when-compile (require 'server))
(eval-when-compile (require 'mediawiki nil t))
(eval-and-compile  (require 'wrap-to-fill nil t))

;; (eval-and-compile  (require 'rng-valid nil t))
;; (declare-function mediawiki-mode "mediawiki") ;; Not in nXhtml
;; (declare-function nxhtml-validation-header-mode "nxhtml-mode")
;; (declare-function rng-clear-overlays "rng-valid")
;; (declare-function rng-process-encoding-name "rng-valid")
;; (declare-function rng-clear-conditional-region "rng-valid")
;; (declare-function rng-forward "rng-valid")
;; (declare-function rng-state-matches-current "rng-valid")
;; (declare-function rng-clear-cached-state "rng-valid")
;; (declare-function rng-mark-xmltok-errors "rng-valid")
;; (declare-function rng-mark-xmltok-dependent-regions "rng-valid")
;; (declare-function rng-cache-state "rng-valid")
;; (declare-function rng-process-end-document "rng-valid")
;;(message " as-ext b %.1f seconds elapsed" (- (float-time) as-ext-load-time-start))

;;;###autoload
(defgroup as-external nil
  "Settings related to Emacs as external editor."
  :group 'nxhtml
  :group 'external)

(defcustom as-external-its-all-text-regexp "/itsalltext/"
  "Regular expression matching It's All Text buffer's file."
  :type 'regexp
  :group 'as-external)

(defcustom as-external-alist
  '(
    (as-external-check-contents
     ((its-all-text "")
      (google-chrome "")
      ))
    (as-external-for-el-files
     ((its-all-text "/itsalltext/.*\.el'")
      (google-chrome "\.el'")
      ))
    (as-external-for-wiki
     ((its-all-text "/itsalltext/.*wiki")
      (google-chrome "wiki")
      ))
    (as-external-setup-for-mail
     ((its-all-text "/itsalltext/.*mail")
      (google-chrome "mail")
      ))
    (as-external-for-xhtml
     ((its-all-text "/itsalltext/")
      (google-chrome "")
      ))
    )
  "List to determine setup if Emacs is used as an external Editor.
Element in this list should have the form

  \(BUFFER-SETUP-FUN ((APP MATCHER) ...))

- BUFFER-SETUP-FUN is a function to be called in the buffer
  containting the text to edit for setting up things.
- APP is the external application used.
- MATCHER should be a regular expression to match something like
  `buffer-file-name' or whatever APP uses for this.

BUFFER-SETUP-FUN should return non-nil if it can setup for
editing.  If our special setup for `server-window' should not be
used the value should be 'use-default-server-window.

The list is processed by `as-external-setup'.
The matching functions are called until one returns non-nil.

The default entries in this list supports for Firefox addon It's
All Text:

- `as-external-for-xhtml'.  For text areas on web pages where you
  can enter some XHTML code, for example blog comment fields.

- `as-external-for-mail-mode', for editing web mail messages.

- `as-external-for-wiki', for mediawiki.

- `as-external-for-el-files', for Emacs lisp files.

See also `as-external-mode'.

* Tip when using Firefox's add-on It's All Text: It looks like
  the file name used will be constructed from the host url. For
  example if your are editing something on
  http://www.emacswiki.org/ the file name may be something like
  'www.emacswiki.org.283b1y212e.html'."
  :type '(repeat
          (list (choice :tag "Match temp file name"
                        (variable :tag "Regexp variable")
                        regexp)
                function))
  :group 'as-external)

(defcustom as-external-its-all-text-coding 'utf-8
  "Coding system to use for It's All Text buffers.
See also `as-external-for-xhtml'."
  :type '(choice (const :tag "No special coding system" nil)
                 coding-system)
  :group 'as-external)

(defun as-external-fall-back (msg)
  "Fallback to text-mode if necessary."
  (text-mode)
  (lwarn t :warning "%s. Using text-mode" msg))

;;;###autoload
(defun as-external-for-xhtml ()
  "Setup for Firefox addon It's All Text to edit XHTML.
It's All Text is a Firefox add-on for editing textareas with an
external editor.
See URL `https://addons.mozilla.org/en-US/firefox/addon/4125'.

In this case Emacs is used to edit textarea fields on a web page.
The text will most often be part of a web page later, like on a
blog.  Therefore turn on these:

- `nxhtml-mode' since some XHTML tags may be allowed.
- `nxhtml-validation-header-mode' since it is not a full page.
- `wrap-to-fill-column-mode' to see what you are writing.
- `html-write-mode' to see it even better.

Also bypass the question for line end conversion when using
emacsw32-eol."
  (interactive)
  (if (not (fboundp 'nxhtml-mode))
      (as-external-fall-back "Can't find nXhtml")
    (nxhtml-mode)
    (nxhtml-validation-header-mode 1)
    (set (make-local-variable 'wrap-to-fill-left-marg-modes)
         '(nxhtml-mode fundamental-mode))
    (wrap-to-fill-column-mode 1)
    ;;(visible-point-mode 1)
    (when (fboundp 'html-write-mode) (html-write-mode 1))
    (when (boundp 'emacsw32-eol-ask-before-save)
      (make-local-variable 'emacsw32-eol-ask-before-save)
      (setq emacsw32-eol-ask-before-save nil)))
  t)


(defvar as-external-mail-mode-comment-pattern "^>.*$"
  "Regular expression for a comment line.")

(defvar as-external-mail-mode-email-pattern
  (concat "[a-z0-9$%(*-=?[_][^<>\")!;:,{}]*"
          "\@"
          "\\(?:[a-z0-9\-]+\.\\)+[a-z0-9]\\{2,4\\}")
  "Regular expression for a mail address.")

(defvar as-external-mail-mode-font-lock-keywords
  (list
   (list as-external-mail-mode-comment-pattern
         '(0 font-lock-comment-face))
   ;; (list as-external-mail-mode-email-pattern
   ;;       '(0 font-lock-keyword-face))
   ))

(defun as-external-setup-for-mail ()
  (as-external-for-mail-mode)
  t)

;;(message " as-ext c %.1f seconds elapsed" (- (float-time) as-ext-load-time-start))

;;;###autoload
(define-derived-mode as-external-for-mail-mode text-mode "ExtMail "
  "Setup for Firefox addon It's All Text to edit mail.
Set normal mail comment markers in column 1 (ie >).

Set `fill-column' to 90 and enable `wrap-to-fill-column-mode' so
that it will look similar to how it will look in the sent plain
text mail.

See also `as-external-mode'."
  ;; To-do: Look at ` http://globs.org/articles.php?lng=en&pg=2 '
  ;; (set (make-local-variable 'comment-column) 0)
  ;; (set (make-local-variable 'comment-end)   "") ;; default
  ;; Try to do it similar to mail-mode:
  (let ((mail-yank-prefix "> ")
        ;;(fill-regexp "[ \t]*[-[:alnum:]]*>+[ \t]*")
        (mail-citation-prefix-regexp "\\(?:[ \t]*\\(?:\\w\\|[_.]\\)+>+\\|[ \t]*[]>|}]\\)+")
        )
    ;; (set (make-local-variable 'comment-start) yank-prefix)
    ;; (set (make-local-variable 'comment-start-skip)
    ;;      (concat "^" (regexp-quote yank-prefix) "[ \t]*"))
    ;; (make-local-variable 'adaptive-fill-regexp)
    ;; (setq adaptive-fill-regexp
    ;;       (concat
    ;;        "\\(?:"
    ;;        fill-regexp
    ;;        "\\)"
    ;;        "\\|"
    ;;        "\\(?:"
    ;;        adaptive-fill-regexp
    ;;        "\\)"
    ;;        ))
    ;; (setq adaptive-fill-regexp fill-regexp)

    ;; From mail-mode:
    (make-local-variable 'paragraph-separate)
    ;;(make-local-variable 'fill-paragraph-function)
    ;;(setq fill-paragraph-function 'mail-mode-fill-paragraph)
    ;; Allow using comment commands to add/remove quoting (this only does
    ;; anything if mail-yank-prefix is set to a non-nil value).
    (set (make-local-variable 'comment-start) mail-yank-prefix)
    (if mail-yank-prefix
        (set (make-local-variable 'comment-start-skip)
             ;;(concat "^" (regexp-quote mail-yank-prefix) "[ \t]*")
             ;;(concat "^" ">+" "[ \t]*")
             (concat "^" mail-citation-prefix-regexp "[ \t]*")
             ))
    (make-local-variable 'adaptive-fill-regexp)
    (setq adaptive-fill-regexp
          (concat "[ \t]*[-[:alnum:]]+>+[ \t]*\\|"
                  adaptive-fill-regexp))
    (make-local-variable 'adaptive-fill-first-line-regexp)
    (setq adaptive-fill-first-line-regexp
          (concat "[ \t]*[-[:alnum:]]*>+[ \t]*\\|"
                  adaptive-fill-first-line-regexp))
    ;; `-- ' precedes the signature.  `-----' appears at the start of the
    ;; lines that delimit forwarded messages.
    ;; Lines containing just >= 3 dashes, perhaps after whitespace,
    ;; are also sometimes used and should be separators.
    (setq paragraph-separate (concat (regexp-quote mail-header-separator)
                                     "$\\|\t*\\([-|#;>* ]\\|(?[0-9]+[.)]\\)+$"
                                     "\\|[ \t]*[[:alnum:]]*>+[ \t]*$\\|[ \t]*$\\|"
                                     "--\\( \\|-+\\)$\\|"
                                     page-delimiter))
    )
  ;; (require 'sendmail)
  ;; (set (make-local-variable 'fill-paragraph-function) 'mail-mode-fill-paragraph)

  ;; (make-local-variable 'adaptive-fill-first-line-regexp)
  ;; (setq adaptive-fill-first-line-regexp
  ;;       (concat "[ \t]*[-[:alnum:]]*>+[ \t]*\\|"
  ;;       	adaptive-fill-first-line-regexp))

  (set (make-local-variable 'font-lock-defaults)
       '((as-external-mail-mode-font-lock-keywords) nil))
  (setq fill-column 70)
  (when (fboundp 'mlinks-mode)
    (mlinks-mode 1))
  (when (fboundp 'wrap-to-fill-column-mode)
    (wrap-to-fill-column-mode 1))
  t)

;;;###autoload
(defun as-external-check-contents ()
  "Try to guess the file contents."
  (cond
   ((equal ";;" (and (< 2 (buffer-size))
                     (buffer-substring-no-properties 1 3)))
    (as-external-for-el-files))
   ;; We don't know about this, let others handle it.
   (t nil)))

;;;###autoload
(defun as-external-for-el-files ()
  "Setup for Firefox addon It's All Text to edit MediaWikis."
  (interactive)
  (emacs-lisp-mode)
  ;; Use default `server-window' since our own setup is for message
  ;; editing mainly.
  'use-default-server-window)

;;;###autoload
(defun as-external-for-wiki ()
  "Setup for Firefox addon It's All Text to edit MediaWikis."
  (interactive)
  ;;(require 'wikipedia-mode nil t)
  (require 'mediawiki nil t)
  (if (not (featurep 'mediawiki))
      (as-external-fall-back "Can't find file mediawiki.el")
    (mediawiki-mode))
  t)

;;(message " as-ext d %.1f seconds elapsed" (- (float-time) as-ext-load-time-start))

;;;###autoload
(define-minor-mode as-external-mode
  "If non-nil check if Emacs is called as external editor.
When Emacs is called as an external editor for example to edit
text areas on a web page viewed with Firefox this library tries
to help to setup the buffer in a useful way. It may for example
set major and minor modes for the buffer.

This can for example be useful when blogging or writing comments
on blogs.

See `as-external-alist' for more information."
  :global t
  :group 'as-external
  ;;(modify-coding-system-alist 'file "/itsalltext/" as-external-its-all-text-coding)
  (let ((coding-entry
         (cons
          as-external-its-all-text-regexp
          (cons as-external-its-all-text-coding
                as-external-its-all-text-coding))))
    ;;(message "as-external-mode=%s" as-external-mode)
    (if as-external-mode
        (progn
          (add-to-list 'file-coding-system-alist coding-entry)
          (add-hook 'server-visit-hook 'as-external-setup t)
          (add-hook 'edit-server-buffer-setup-hook 'as-external-setup t))
      (setq file-coding-system-alist
            (delq coding-entry file-coding-system-alist))
      (remove-hook 'server-visit-hook 'as-external-setup)
      (remove-hook 'edit-server-buffer-setup-hook 'as-external-setup))))

(defvar as-external-my-frame nil)
(make-variable-buffer-local 'as-external-my-frame)

(defvar as-external-last-buffer nil)

(defun as-external-server-window-fix-frames (&optional caller)
  (condition-case err
      (with-current-buffer as-external-last-buffer
        (unless (and (featurep 'pause)
                     (buffer-live-p pause-buffer)
                     (not (pause-use-topmost)))
          (remove-hook 'pause-break-exit-hook 'as-external-server-window-fix-frames)
          (setq as-external-my-frame (or as-external-my-frame
                                         ;;(make-frame)
                                         (make-frame-on-display (getenv "DISPLAY"))
                                         ))
          (when (and server-raise-frame
                     (memq caller '(its-all-text)))
            (dolist (f (frame-list))
              (unless (eq f as-external-my-frame)
                (lower-frame f))))
          (gdb-deb-print "as-external-server-window-fix-frames %s" as-external-my-frame)
          ;;(select-frame-set-input-focus (window-frame (selected-window)))
          ;;(select-frame-set-input-focus as-external-my-frame)
          ;;(run-with-idle-timer 2 nil 'raise-frame as-external-my-frame)
          ;;(as-external-start-raise-frame-timer buffer)

          ;; Fix-me: Without make-frame-visible raise-frame does not work...
          (make-frame-visible as-external-my-frame)
          (set-window-buffer (frame-first-window as-external-my-frame) (current-buffer))
          (redisplay t)
          (select-frame-set-input-focus as-external-my-frame)
          ;;(raise-frame as-external-my-frame) ;; Does not work
          (as-external-start-raise-frame-timer (current-buffer))
          ))
    (error (message "%s" (error-message-string err)))))

(defun as-external-raise-frame ()
  (condition-case err
      (progn
        (remove-hook 'window-configuration-change-hook 'as-external-raise-frame t)
        (msgtrc "as-external-raise-frame %S %S" (current-buffer) (selected-window))
        (raise-frame (window-frame (selected-window)))
        )
    (error (message "as-external-raise-frame: %s" (error-message-string err)))))

(defun as-external-start-raise-frame-timer (buffer)
  (run-with-idle-timer 2 nil 'as-external-raise-frame-in-timer buffer))

(defun as-external-raise-frame-in-timer (buffer)
  (condition-case err
      (let* ((window (get-buffer-window buffer))
             (frame (when window (window-frame window))))
        (setq frame (or frame (with-current-buffer buffer as-external-my-frame)))
        (gdb-deb-print "as-external-raise-frame-in-timer: buffer=%S window=%S frame=%S" buffer window frame)
        (if (not (and frame
                      window
                      ))
            (as-external-start-raise-frame-timer buffer)
          (select-frame-set-input-focus as-external-my-frame)
          ;;(make-frame-visible as-external-my-frame)
          ;;(redisplay t)
          (gdb-deb-print "as-external-raise-frame-in-timer: RAISE-FRAME")
          ;;(raise-frame frame)
          ))
    (error (message "as-external-start-raise-frame-timer: %s" (error-message-string err)))))

;;(message " as-ext e %.1f seconds elapsed" (- (float-time) as-ext-load-time-start))

(defvar as-external-caller nil)

(defun as-external-server-window (buffer)
  (setq server-window nil)
  (with-current-buffer buffer
    ;;(add-hook 'window-configuration-change-hook 'as-external-raise-frame nil t)
    (setq as-external-last-buffer (current-buffer))
    (run-with-idle-timer 2 nil 'as-external-server-window-fix-frames as-external-caller)
    (add-hook 'pause-break-exit-hook 'as-external-server-window-fix-frames)
    (add-hook 'kill-buffer-hook 'as-external-delete-my-frame nil t)))

(defun as-external-delete-my-frame ()
  (let ((win (and (frame-live-p as-external-my-frame)
                  (get-buffer-window nil as-external-my-frame))))
    (when (and win
               (= 1 (length (window-list as-external-my-frame 'no-mini))))
      ;; Check if we must lower before deleting frame since this is
      ;; buffer dependent and buffer will change after delete-frame.
      (let ((must-lower (and server-raise-frame
                             (memq as-external-caller '(its-all-text)))))
        (delete-frame as-external-my-frame)
        (when must-lower (lower-frame))
        ))))

(defun as-external-get-buffer-setup (caller)
  "Found buffer setup function and call it."
  (unless (memq caller '(its-all-text google-chrome))
    (gdb-deb-print "as-external: unknown caller=%s" caller)
    (msgtrc "as-external: unknown caller=%s" caller))
  (catch 'fun
    (dolist (rec as-external-alist)
      (msgtrc "as-external-setup-1 rec=%S" rec)
      (let* ((setup-fun    (nth 0 rec))
             (matchers     (nth 1 rec))
             (caller-rec (assoc caller matchers))
             (regexp     (nth 1 caller-rec)))
        (when regexp
          (when (symbolp regexp)
            (setq regexp (symbol-value regexp)))
          (when (case caller
                 (google-chrome (string-match-p regexp edit-server-url))
                 (its-all-text (string-match regexp (buffer-file-name))))
            (let ((ret (funcall setup-fun)))
              (msgtrc "as-external-setup-1: %s => %s" setup-fun ret)
              (when ret (throw 'fun ret)))))))))

(defun as-external-setup-1 ()
  ;; Fix-me: How does one know if the file names are case sensitive?
  (gdb-deb-print "as-external-setup-t start, check thread")
  (unless (when (boundp 'nowait) nowait) ;; dynamically bound in `server-visit-files'
    (msgtrc "as-external-setup-t cb=%S" (current-buffer))
    (gdb-deb-print "as-external-setup-t cb=%S" (current-buffer))
    (let* ((caller (cond
                    ((local-variable-p 'edit-server-url) 'google-chrome)
                    ((string-match-p as-external-its-all-text-regexp
                                     (buffer-file-name))
                     'its-all-text)
                    (t (error "Unknown caller"))))
           (use-server-window (as-external-get-buffer-setup caller)))
      (set (make-local-variable 'as-external-caller) caller)
      ;; Fix-me:
      (unless (or server-window
                  (eq use-server-window 'use-default-server-window))
        ;; `server-goto-toplevel' has been done here.
        ;; Setup to use a new frame
        (setq edit-server-window 'as-external-server-window)
        (setq server-window 'as-external-server-window)))))

(defun as-external-setup ()
  "Check if Emacs is used as an external editor.
If so then turn on useful major and minor modes.
This is done by checking `as-external-alist'."
  (condition-case err
      (as-external-setup-1)
    (error (message "as-external-setup error: %s" err))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Google chrome edit-server.el support

;; (defun as-external-edit-server-setup-1 ()
;;   ;; Fix-me: How does one know if the file names are case sensitive?
;;   (unless (when (boundp 'nowait) nowait) ;; dynamically bound in `server-visit-files'
;;     (msgtrc "as-external-setup-t cb=%S" (current-buffer))
;;     (let* ((use-server-window
;;             (catch 'fun
;;               (dolist (rec as-external-alist)
;;                 (msgtrc "as-external-setup-1 rec=%S" rec)
;;                 (let ((file-regexp (car rec))
;;                       (setup-fun   (cadr rec)))
;;                   (when (symbolp file-regexp)
;;                     (setq file-regexp (symbol-value file-regexp)))
;;                   (when (string-match file-regexp (buffer-file-name))
;;                     (let ((ret (funcall setup-fun)))
;;                       (msgtrc "as-external-setup-1: %s => %s" setup-fun ret)
;;                       (when ret (throw 'fun ret)))))))))
;;       (unless (or server-window
;;                  (eq use-server-window 'use-default-server-window))
;;         ;; `server-goto-toplevel' has been done here.
;;         ;; Setup to use a new frame
;;         (setq server-window 'as-external-server-window)))))

;; (defun as-external-edit-server-setup ()
;;   "Check if Emacs is used by Google Chrome edit server.
;; If so then turn on useful major and minor modes.
;; This is done by checking `as-external-edit-server-alist'."
;;   (condition-case err
;;       (as-external-setup-1)
;;     (error (message "as-external-setup error: %s" err))))

;;(message " as-ext fin %.1f seconds elapsed" (- (float-time) as-ext-load-time-start))
(provide 'as-external)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; as-external.el ends here
