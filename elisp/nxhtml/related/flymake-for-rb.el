;;; flymake-for-rb.el ---
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2010-12-27 Mon
;; Version:
;; Last-Updated:
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
;; Flymake for ruby files.
;;
;; Built on ideas from this:
;;
;;   Author: Steve Purcell
;;   Homepage: http://github.com/purcell/emacs.d/blob/master/site-lisp/flymake-ruby/flymake-ruby.el
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
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

(eval-when-compile (require 'cl))
(require 'flymake)

(defvar flymake-for-rb-err-line-patterns '(("^\\(.*\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 3)))
(defvar flymake-for-rb-allowed-file-name-masks '((".+\\.\\(rb\\|rake\\)$" flymake-for-rb-init)
                                               ("Rakefile$" flymake-for-rb-init)))

;; Not provided by flymake itself, curiously
(defun flymake-create-temp-in-system-tempdir (filename prefix)
  (make-temp-file (or prefix "flymake-for-rb")))

(defun flymake-for-rb-init ()
  (set (make-local-variable 'flymake-err-line-patterns) flymake-for-rb-err-line-patterns)
  ;; Invoke ruby with '-c' to get syntax checking
  (list "ruby" (list "-c" (flymake-init-create-temp-buffer-copy
                           'flymake-create-temp-in-system-tempdir))))

(defun flymake-for-rb-loader ()
  (dolist (rec flymake-for-rb-allowed-file-name-masks)
    (push 'flymake-allowed-file-name-masks rec)))

(provide 'flymake-for-rb)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; flymake-for-rb.el ends here
