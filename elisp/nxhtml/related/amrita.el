;;; amrita.el --- Amrita major mode

;; Author: Roger Mason.  Based on sample.el by Stefan Monnier
;; Copyright: Roger Mason
;; Modified by Lennart Borgman
;; Keywords: extensions

;; sample.el is:
;; Copyright (C) Free Software Foundation 2009.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; See also http://www.emacswiki.org/emacs/ModeTutorial

;;; Code:

(defvar amrita-mode-hook nil)

(defvar amrita-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [foo] 'amrita-do-foo)
    map)
  "Keymap for `amrita-mode'.")

(defvar amrita-font-lock-keywords
  (list
   '("\\(fold::\\)" . font-lock-builtin-face)
   '("\\( \"= \\)"    . font-lock-builtin-face)
   )
  "Keyword highlighting specification for `amrita-mode'.")

 ;;; Indentation

(defun amrita-indent-line ()
  "Indent current line of Amrita code."
  (interactive)
  (let ((savep (> (current-column) (current-indentation)))
        (indent (condition-case nil (max (amrita-calculate-indentation) 0)
                  (error 0))))
    (if savep
        (save-excursion (indent-line-to indent))
      (indent-line-to indent))))

(defun amrita-tell-me-indentation ()
  "Indicate how the current line of Amrita code would be
    indented by amrita-mode (units are spaces)."
  (interactive)
  (message "Indent value is %s" (amrita-calculate-indentation) )
  )

(defun amrita-calculate-indentation ()
  "Return the column to which the current line should be indented."
  (interactive)
					; (beginning-of-line)
  (if (bobp)  ; Beginning of buffer
      0
    (let ((not-indented t) indent)
      (if (looking-at "^[ \t]*.*}.*")   ; Test for }
	  (progn
	    (save-excursion
	      (while not-indented
		(forward-line -1)
		(if (looking-at "^[ \t]*fold.*{.*")
		    (progn (setq indent (current-indentation))
			   (setq not-indented nil)))
		(progn
		  (setq indent (- (current-indentation) 3))
		  (setq not-indented nil)
					; Outdent 3 or 0
		  (if (< indent 0)
		      (setq indent 0))))))
	(save-excursion
	  (while not-indented
	    (forward-line -1)
	    (if (looking-at "^[ \t]*.*}.*") 	; End of block
		(progn
		  (setq not-indented nil)
		  (setq indent (current-indentation) ))
	      (if (looking-at "^[ \t]*.*{.*")	; Beginning of block
		  (progn
		    (setq not-indented nil)
		    (setq indent (+ (current-indentation) 3)))
		(if (bobp)
		    (setq not-indented nil)))))))
      indent)))

;; make double quote the same as equals sign:
;;(modify-syntax-entry ?\" (char-to-string (char-syntax ?=)))
;; change double quote to punctuation:
;;(modify-syntax-entry ?\" ".")

(defvar amrita-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?# "<" st)
    (modify-syntax-entry ?\n ">" st)
    ;;     (modify-syntax-entry ?\" (char-to-string (char-syntax ?=)) st)
    (modify-syntax-entry ?\" "." st)
    st)
  "Syntax table for `amrita-mode'.")

(defvar amrita-imenu-generic-expression
  nil)

(defvar amrita-outline-regexp
  "\\(^[ \t]*.*{.*\\)")


(defun amrita-enter-fold ()
  "Make the current fold the only visible part of the buffer"
  (interactive)
  (if (not (looking-at "^[ \t]*fold.*{.*") )
      (message "Point must be positioned on a fold in order to enter it")
    (progn
      (let ((beg (point)) (col (amrita-calculate-indentation)) (keepgoing t) )
	(message "Column = %d" col)
    	(beginning-of-line)
                                        ;    	(setq keepgoing t)
    	(while keepgoing
    	  (forward-line 1)
    	  (if (looking-at "^[ \t]*.*}.*")
    	      (progn
    		(if (eq col (amrita-calculate-indentation))
                    (setq keepgoing nil)))))
	(narrow-to-region beg (point))))))



(defun amrita-insert-fold (type comment)
  "Insert a new fold at point.
There are two arguments: 'type' is the fold type
and 'comment' is placed after the {."
  (interactive "MFold type: \nMFold comment: ")
  (let ((fold (concat "\n" "fold::" type " { " comment "\n\n}")))
    (end-of-line)
    (insert fold)
    (forward-line -3)
    (amrita-indent-line)
    (forward-line 1)
    (amrita-indent-line)
    (forward-line 1)
    (amrita-indent-line)
    (forward-line 1)
    (amrita-indent-line)
    (forward-line -1)
    (end-of-line)))

 ;;;###autoload
(define-derived-mode amrita-mode fundamental-mode "Amrita"
  "A major mode for editing Amrita files."
  :syntax-table amrita-mode-syntax-table
  (set (make-local-variable 'comment-start) "# ")
  (set (make-local-variable 'comment-start-skip) "#+\\s-*")
  (set (make-local-variable 'font-lock-defaults)
       '(amrita-font-lock-keywords))
  (set (make-local-variable 'indent-line-function) 'amrita-indent-line)
  (set (make-local-variable 'imenu-generic-expression)
       amrita-imenu-generic-expression)
  ;;   (set (make-local-variable 'outline-regexp) amrita-outline-regexp)
  (set (make-local-variable 'outline-regexp) "^[ ]*fold::")
  ;;(set (make-local-variable 'outline-minor-mode) t)
  (outline-minor-mode 1)
  (set (make-local-variable 'indent-tabs-mode) nil)
  )

(provide 'amrita)
 ;;; amrita.el ends here

;; ;;======= Code folding =======
;; (add-hook 'python-mode-hook 'my-python-outline-hook)
;; ; this gets called by outline to deteremine the level. Just use the length of the whitespace
;; (defun py-outline-level ()
;;   (let (buffer-invisibility-spec)
;;     (save-excursion
;;       (skip-chars-forward "    ")
;;       (current-column))))
;; ; this get called after python mode is enabled
;; (defun my-python-outline-hook ()
;;   ; outline uses this regexp to find headers. I match lines with no indent and indented "class"
;;   ; and "def" lines.
;;   (setq outline-regexp "[^ \t]\\|[ \t]*\\(def\\|class\\) ")
;;   ; enable our level computation
;;   (setq outline-level 'py-outline-level)
;;   ; do not use their \C-c@ prefix, too hard to type. Note this overides some bindings.
;;   (setq outline-minor-mode-prefix "\C-t")
;;   ; turn on outline mode
;;   (outline-minor-mode t)
;;   ; initially hide all but the headers
;;   ;(hide-body)
;;   ; make paren matches visible
;;   (show-paren-mode 1)
;; )
