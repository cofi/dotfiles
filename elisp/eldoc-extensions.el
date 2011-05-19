;;; eldoc-extensions.el -- Show eldoc when using M-:

;; Copyright (C) 2011, Thierry Volpiatto, all rights reserved.

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;; Code:
(require 'eldoc)

(defvar eldoc-in-minibuffer t)
(defvar eval-prefered-function 'pp-eval-expression)
(defvar eldoc-in-minibuffer-show-fn 'eldoc-show-in-mode-line
  "A function to display eldoc info.
Should take one arg: the string to display")
(defvar eldoc-show-in-mode-line-delay 12)

(defun eldoc-show-in-mode-line (str)
  (with-current-buffer (current-buffer)
    (let ((mode-line-format (concat " " str)))
      (force-mode-line-update)
      (sit-for eldoc-show-in-mode-line-delay))
    (force-mode-line-update)))

(defun eldoc-show-in-eval ()
  "Show eldoc for current minibuffer input."
  (interactive)
  (let* ((str-all (minibuffer-completion-contents))
         (sym     (when str-all
                    (with-temp-buffer
                      (insert str-all)
                      (goto-char (point-max))
                      (unless (looking-back ")\\|\"") (forward-char -1))
                      (eldoc-current-symbol))))
         (doc     (or (eldoc-get-var-docstring sym)
                      (eldoc-get-fnsym-args-string
                       (car (eldoc-fnsym-in-current-sexp))))))
    (when doc (funcall eldoc-in-minibuffer-show-fn doc))))

(defun eldoc-eval-expression ()
  "`eval-expression' with eldoc support."
  (interactive)
  (if eldoc-in-minibuffer
      (let ((timer (run-with-idle-timer eldoc-idle-delay
                                        'repeat 'eldoc-show-in-eval)))
        (unwind-protect
             (call-interactively eval-prefered-function)
          (cancel-timer timer)))
      (call-interactively eval-prefered-function)))

(global-set-key [remap eval-expression] 'eldoc-eval-expression)

(provide 'eldoc-extensions)

;; eldoc-extensions.el ends here
