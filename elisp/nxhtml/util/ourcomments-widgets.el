;;; ourcomments-widgets.el --- widgets for custom etc
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2009-10-13 Tue
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
;;
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

(eval-when-compile (require 'mumamo nil t))
(eval-when-compile (require 'ourcomments-util nil t))

(define-widget 'command 'restricted-sexp
  "A command function."
  :complete-function (lambda ()
                       (interactive)
                       (lisp-complete-symbol 'commandp))
  :prompt-value 'widget-field-prompt-value
  :prompt-internal 'widget-symbol-prompt-internal
  :prompt-match 'commandp
  :prompt-history 'widget-command-prompt-value-history
  :action 'widget-field-action
  :match-alternatives '(commandp)
  :validate (lambda (widget)
              (unless (commandp (widget-value widget))
                (widget-put widget :error (format "Invalid command: %S"
                                                  (widget-value widget)))
                widget))
  :value 'ignore
  :tag "Command")

;;;###autoload
(define-widget 'major-mode-function 'function
  "A major mode lisp function."
  :complete-function (lambda ()
                       (interactive)
                       (lisp-complete-symbol 'major-or-multi-majorp))
  :prompt-match 'major-or-multi-majorp
  :prompt-history 'widget-function-prompt-value-history
  :match-alternatives '(major-or-multi-majorp)
  :validate (lambda (widget)
              (unless (major-or-multi-majorp (widget-value widget))
                (widget-put widget :error (format "Invalid function: %S"
                                                  (widget-value widget)))
                widget))
  :value 'fundamental-mode
  :tag "Major mode function")

(provide 'ourcomments-widgets)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ourcomments-widgets.el ends here
