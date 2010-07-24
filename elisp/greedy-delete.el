;;; greedy-delete.el --- minor mode for a better backspace

;; Copyright (C) 1997 Free Software Foundation, Inc.

;; Author:  1997 Barry A. Warsaw
;; Created: 11-Feb-1997 (taken nearly verbatim from cc-mode.el)
;; Version: 1.7
;; Last Modified: 1997/02/11 22:00:24
;; Keywords: languages delete

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This package is called Greedy Delete-y! :-)

;; This package provides a major-mode independent mechanism for
;; different deletion preferences.  In some major-modes it turns out
;; to be less useful to map backspace or delete to just deleting one
;; character.  Instead, a `hungry' or `greedy' deletion mechanism is
;; more useful.
;;
;; The variable `gd-how-much' controls how much preceding whitespace
;; is consumed when the delete key is hit.  For non-greedy deletion
;; the `gd-delete-function' variable is consulted.
;;
;; Here's an example of how you might use greedy delete in python-mode:
;;
;; (add-hook 'py-mode-hook 'gd-add-to-mode)

;; You can get updates to this file from:
;;
;;     http://www.python.org/ftp/emacs/
;;
;; or via anonymous ftp from:
;;
;;     ftp://ftp.python.org/pub/emacs

;;; Code


;; user definable variables
;; vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv

(defvar gd-how-much 'all
  "*Controls how much preceding whitespace to consume.
The value 'all says to consume all whitespace preceding point.  The
value 'line says to only consume preceding whitespace up the the
beginning of the current line.")

(defvar gd-delete-function 'backward-delete-char-untabify
  "*Function called by `gd-electric-delete' when deleting characters.")



;; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;; NO USER DEFINABLE VARIABLES BEYOND THIS POINT

(defvar gd-enabled nil
  "Non-nil if greedy delete is enabled.
Use the command `greedy-delete-mode' to toggle, enable, or disable
this mode.")

(defvar gd-mode-check-function nil
  "Function which major-modes can use to customize `gd-electric-delete'.
This variable is buffer local.  If non-nil, `gd-electric-delete' will
`funcall' this function with no arguments.  The function should return
nil or non-nil indicating whether it is okay to do the greedy
deletion.")

(defvar gd-indicator-string " Greedy"
  "String to show in the mode line when greedy delete is enabled.")

(make-variable-buffer-local 'gd-enabled)
(make-variable-buffer-local 'gd-mode-check-function)


(defun gd-electric-delete (arg)
  "Deletes preceding character or whitespace.
If `gd-enabled' is non-nil, then preceding whitespace is consumed
according to `gd-how-much'.  However, if an ARG is supplied, or
`gd-enabled' is nil, or `gd-mode-check-function' returns non-nil then
the function in the variable `c-delete-function' is called."
  (interactive "P")
  (let ((check (if gd-mode-check-function
		   (funcall gd-mode-check-function)
		 t)))
  (if (or (not gd-enabled)
	  arg
	  (not check))
      (funcall gd-delete-function (prefix-numeric-value arg))
    (let ((here (point))
	  (skip (cond ((eq gd-how-much 'all)  " \t\n")
		      ((eq gd-how-much 'line) " \t")
		      (t (error "Illegal value for `gd-how-much': %s"
				gd-how-much))
		      )))
      (skip-chars-backward skip)
      (if (/= (point) here)
	  (delete-region (point) here)
	(funcall gd-delete-function 1))
      ))))


(defun greedy-delete-mode (&optional arg)
  "Toggle greedy delete mode.
With arg, turn greedy delete mode on if and only if arg is positive.
Turn it off if arg is negative.  Toggle the mode if arg is zero."
  (interactive "P")
  (setq gd-enabled (if (or (not arg)
			   (zerop (setq arg (prefix-numeric-value arg))))
		       (not gd-enabled)
		     (> arg 0)))
  ;; obsolete in XEmacs, but works for both XEmacs and Emacs
  (force-mode-line-update))


(defun turn-on-greedy-delete-mode ()
  "Turns on greedy delete mode.
Also updates the `minor-mode-alist'."
  (interactive)
  (greedy-delete-mode 1)
  (or (assq 'gd-enabled minor-mode-alist)
      (setq minor-mode-alist
	    (append minor-mode-alist
		    (list '(gd-enabled gd-indicator-string))))))

(defun gd-add-to-mode ()
  "Adds greedy delete to the current buffer's major mode.
This specifically does:
    1. binds `greedy-delete-mode' to C-c BS
    2. binds `gd-electric-delete' to BS
    3. calls `turn-on-greedy-delete-mode'

This function is appropriate for any major-mode hook."
  (local-set-key [(control c) backspace] 'greedy-delete-mode)
  (local-set-key [backspace] 'gd-electric-delete)
  (turn-on-greedy-delete-mode))



(provide 'greedy-delete)
;; greedy-delete.el ends here
