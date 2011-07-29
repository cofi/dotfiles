;;; ack.el --- Running perl ack under Emacs (on w32 especially)
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2010-12-27 Mon
;; Version:
;; Last-Updated: 2010-12-27 Mon
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
;; See command `ack'.
;;
;; This was grabbed from stackoverflow.com:
;;
;;   perl - ack does not work when run from "grep-find" in Emacs on Windows
;;   http://stackoverflow.com/questions/2322389/ack-does-not-work-when-run-from-grep-find-in-emacs-on-windows
;;
;; There is perhaps only one line in the answer from cjm that is not
;; rather trivial - but that line is very essential! It is the line
;; closing stdin.
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

(defvar ack-command "ack --nogroup --nocolor ")
(defvar ack-history nil)
(defvar ack-host-defaults-alist nil)
(defun ack ()
  "Like grep, but using ack-command as the default"
  (interactive)
  ;; Make sure grep has been initialized
  (if (>= emacs-major-version 22)
      (require 'grep)
    (require 'compile))
  ;; * Note: Close STDIN to keep ack from going into filter mode
  (let ((null-device (format "< %s" null-device))
        (grep-command ack-command)
        (grep-history ack-history)
        (grep-host-defaults-alist ack-host-defaults-alist))
    (call-interactively 'grep)
    (setq ack-history             grep-history
          ack-host-defaults-alist grep-host-defaults-alist)))

(provide 'ack)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ack.el ends here
