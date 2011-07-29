;;; backtr.el --- Finding source code inside a function from *Backtrace* buffer

;; Copyright (C) 2005 by Lennart Borgman

;; Author:  Lennart Borgman <lennart DOT borgman DOT 073 AT student DOT lu DOT se>
;; Created: 2005-08-17
;; Version: 0.53
;; Last-Updated: Mon Mar 05 21:58:55 2007 (3600 +0100)
;; Keywords: languages
;;
;; Features that might be required by this library:
;;
;;   None
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This file adds the possibility to move inside a function from the
;; *Backtrace* buffer. Pressing the RET key while over one of the
;; backtrace lines will try to take you to the corresponding position
;; in the source code. However if point is over a symbol or a link the
;; usual action (ie without this file) in the *Backtrace* buffer is
;; used.
;;
;; Some bugs and deficiencies in debug-help-follow are corrected.

;;; Bugs:
;;
;; 1) Can not go inside varlist of (let ...). Possibly other similar
;;    bugs.  The best you can do in those situation is to choose the
;;    next outer level from the backtrace list.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; History:
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'help-mode)

(defvar backtr-current-hit nil
  "Used by `backtr-next-hit' and `backtr-prev-hit'.")

(defvar backtr-hits nil
  "Number of final hits when trying to find source code.")

(defcustom backtr-stay-in-backtrace t
  "Do not switch to the source code window."
  :type 'boolean
  :group 'debug)

(defun backtr-follow-help-or-goto-line()
  "If position is at symbol show help. Otherwise go to source line."
  (interactive)
  (or (backtr-debug-help-follow (point))
      (backtr-goto-line)
      (backtr-goto-char)
      ))

(defun backtr-goto-char()
  (let (pos)
    (save-excursion
      (beginning-of-line)
      (when (looking-at (rx (0+ blank)
                            "eval-buffer()  ; Reading at buffer position "
                            (submatch
                             (0+ digit))))
        (setq pos (string-to-number (match-string 1)))))
    (other-window -1)
    (goto-char pos)))

;; Return non-nil if following help. Stay in *Backtrace*.  Remove the
;; unnecessary push-button test. Fix the multiple *Help* buffer bug.
(defun backtr-debug-help-follow (&optional pos)
  "Follow cross-reference at POS, defaulting to point.
For the cross-reference format, see `help-make-xrefs'."
  (interactive "d")
  (require 'help-mode)
  (unless pos
    (setq pos (point)))
  ;; check if the symbol under point is a function or variable
  (let ((sym
         (intern
          (save-excursion
            (goto-char pos) (skip-syntax-backward "w_")
            (buffer-substring (point)
                              (progn (skip-syntax-forward "w_")
                                     (point)))))))
    (when (or (boundp sym) (fboundp sym) (facep sym))
      (save-excursion
        (with-output-to-temp-buffer (help-buffer)
          (set-buffer (help-buffer))
          ;; First arg of help-do-xref does not seem to be used???
          (help-do-xref pos #'help-xref-interned (list sym))
          ;; Remove the extra [back] link. FIX-ME: can the extra [back] be avoided???
          (goto-char (point-max))
          (forward-line -2)
          (let ((inhibit-read-only t))
            (delete-region (point) (point-max)))
          ;; Return the text we displayed.
          (with-current-buffer standard-output
            (buffer-string)))))))


;; Debugging tools
(defun backtr-sit-for(sec)
  ;;(sit-for sec)
  )
(defun backtr-message(&rest arg)
  ;;(apply 'message arg)
  )


(defun backtr-goto-line()
  "Goto source line."
  (let ((bn "*Backtrace*"))
    (unless (equal bn (buffer-name)) (error "Not in %s" bn)))
  (let ((started-at (point))
        found-fun
        (last-fun-call nil)
        (to-point (point))
        (button-point nil)
        (deb-lines nil)
        skip-next
        )
    (save-excursion
      (beginning-of-line)
      ;; Do not do anything on first line
      (when (equal " "  (char-to-string (following-char)))
        (while (and (not last-fun-call)
                    (setq found-fun (search-forward-regexp "^  \\([a-z-]+\\)" nil t)))
          (backward-char)
          ;;(when (eq (get-text-property (point) 'category) 'help-function-def-button)
          ;; eq does not work????
          (when (get-text-property (point) 'category)
            (setq last-fun-call (text-properties-at (point)))
            (setq button-point (copy-marker (point)))
            (while (> (progn (beginning-of-line) (point)) to-point)
              (let (line
                    (p (copy-marker (point))))
                (forward-line -1)
                (beginning-of-line)
                (skip-chars-forward " ")
                (setq line (buffer-substring-no-properties (point) (line-end-position)))
                (unless skip-next
                  (let* (
                         ;;(pos (string-match "\\(?:\\.\\.\\.\\|#\\|\\*\\)" line))
                         (pos (string-match "\\(?:\\.\\.\\.\\|#\\|\\*\\|\"\\)" line))
                         ;;(pos (string-match "\\(?:\\.\\.\\.\\|#\\|\\*\\|\=\\)" line))
                         (regexp (if pos (substring line 0 pos) line))
                         )
                    ;; Can't get the regexp for * to work???
                    ;;(setq regexp (replace-regexp-in-string "\\*" "\\*" regexp t t))
                    ;;(setq regexp (replace-regexp-in-string "\\*" "." regexp t t))
                    (setq regexp (replace-regexp-in-string "\"" "\\\"" regexp nil t))
                    (setq regexp (replace-regexp-in-string "(lambda[ ]+" "(lambda#space#" regexp nil t))
                    (setq regexp (replace-regexp-in-string "[ ]+" "[ \t\r\n]+" regexp nil t))
                    (setq regexp (replace-regexp-in-string "(lambda#space#" "(lambda[ \t\r\n]*" regexp nil t))

                    (unless (looking-at regexp) (error "Internal error, not (looking-at %s)" regexp))
                    (when (and (< 5 (length regexp)) (equal "mapc(" (substring regexp 0 5)))
                      (setq regexp (concat "(mapc[ \t\r\n]+" (substring regexp 5)))
                      )
                    (setq deb-lines (cons regexp deb-lines)))
                  )
                (if (equal "(when " (substring line 0 6)) (setq skip-next t) (setq skip-next nil))
                ))))))
    (when found-fun
      (let ((sel-win (selected-window)))
        (setq deb-lines (reverse deb-lines))
        (push-button button-point)
        ;;(delete-other-windows)
        (forward-char)
        (setq backtr-hits nil)
        (when deb-lines
          (backtr-find 0 deb-lines t)
          (setq backtr-hits (reverse backtr-hits))
          (if (not backtr-hits)
              (message "Could not find backtrack position - please use next backtrace line instead")
            (setq backtr-current-hit nil)
            (backtr-next-hit)
            (when (< 1 (length backtr-hits))
              (message "There are %s positions possible, use `backtr-next-hit' for next" (length backtr-hits)))))
        (when backtr-stay-in-backtrace
          (select-window sel-win))))))

(defun backtr-next-hit()
  "Go to next hit in source code."
  (interactive)
  (if (not backtr-hits)
      (message "No backtrace hits active")
    (let ((hit (if (not backtr-current-hit) 0 (1+ backtr-current-hit))))
      (if (<= (length backtr-hits) hit)
          (message "No next hit, use `backtr-prev-hit' to go back.")
        (setq backtr-current-hit hit)
        (let ((m (nth hit backtr-hits)))
          (switch-to-buffer (marker-buffer m))
          (goto-char m))))))

(defun backtr-prev-hit()
  "Go to prev hit in source code."
  (interactive)
  (if (not backtr-hits)
      (message "No backtrace hits active")
    (let ((hit (if (not backtr-current-hit) (1- (length backtr-hits)) (1- backtr-current-hit))))
      (if (< hit 0)
          (message "No prev hit, use `backtr-next-hit' to go forward.")
        (setq backtr-current-hit hit)
        (let ((m (nth hit backtr-hits)))
          (switch-to-buffer (marker-buffer m))
          (goto-char m))))))

(defun backtr-find(level trace-list find-all)
  "From TRACE-LIST find source code position."
  (let ((started-at (point))
        (trace-tail (cdr trace-list))
        found-this-at
        start-this
        found
        end-of-siblings
        end-of-parents
        (regexp (car trace-list)))
    (when (member (substring regexp 0 1) '("/" "+" "-" "*"))
      (setq regexp (concat "(/ " (substring regexp 2))))
    (while (not (or found-this-at end-of-parents))
      (backtr-message "backtr-find %s %s" level regexp) (backtr-sit-for 1)
      (while (not (or found-this-at end-of-siblings))
        (forward-comment 10)
        (backtr-sit-for 0.5)
        (if (looking-at regexp)
            (progn
              (backtr-message "OK looking at %s" regexp)(backtr-sit-for 1)
              (setq found-this-at (point)))
          (condition-case err
              (forward-sexp)
            (error (setq end-of-siblings t)))
          ))
      (when found-this-at
        (if (not trace-tail)
            (progn
              (setq backtr-hits (cons (point-marker) backtr-hits))
              (setq found t))
          (setq start-this (point))
          (forward-char)
          (setq found (backtr-find (1+ level) trace-tail find-all))
          (unless found
            (backtr-message "back, but not found")(backtr-sit-for 2)
            (setq found-this-at nil)
            (when start-this (goto-char start-this))
            (backtr-sit-for 2)))
        (when find-all (setq found-this-at nil))
        )
      (unless (and found (not find-all))
        (condition-case err
            (forward-sexp)
          (error (setq end-of-parents t)))
        (forward-comment 10)(backtr-sit-for 0.1))
      )
    (unless found-this-at
      (backtr-message "going back to started at=%s" started-at)
      (backtr-sit-for 2)
      (goto-char started-at)
      (backtr-sit-for 2))
    found))

(eval-after-load 'debug
  '(define-key debugger-mode-map [?\r] 'backtr-follow-help-or-goto-line))

(provide 'backtr)

;;; backtr.el ends here
