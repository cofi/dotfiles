;;; flymake-for-js.el --- Flymake setup for javascript files
;;
;; Author: Lennart Borgman
;; Created: Sun Dec 02 07:52:52 2007
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
;; This library provides basic setup for using `flymake-mode' with
;; javascript files.  To use this you must have a javascript
;; installed.  There are (at least) two free javascript engines (both
;; from Mozill) you can use, Rhino (implemented in Java) or
;; SpiderMonkey (implemented in C). Both are supported in this
;; library.
;;
;; I have not been able to find binaries for SpiderMonkeys to
;; download. However the Rhino engine seems fast enough and is easy to
;; install. You find them at
;;
;;    http://www.mozilla.org/rhino/
;;    http://www.mozilla.org/js/spidermonkey/
;;
;; Put this file in your Emacs `load-path' and then in .emacs
;;
;;    (require 'flymake-for-js)
;;    (flymake-for-js-loader)
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

(eval-when-compile (require 'cl))
(require 'flymake)

(defconst flymake-for-js-dir
  (file-name-directory (or load-file-name
                           (when (boundp 'bytecomp-filename) bytecomp-filename)
                           buffer-file-name))
  "Installation directory for flymake-for-js.")

;;;###autoload
(defgroup flymake-for-js nil
  "Customization group for flymake for javascript."
  :group 'flymake)

(defcustom flymake-allowed-js-file-name-masks '(("\\.json\\'" flymake-for-js-init)
                                                ("\\.js\\'" flymake-for-js-init))
  "Filename extensions that switch on js syntax checks."
  :type '(repeat (list (regexp :tag "File name regexp")
                       (function :tag "Init function")
                       (choice (const :tag "No cleanup function" nil)
                               (function :tag "Cleanup function"))))
  :group 'flymake-for-js)


(defvar flymake-for-js-err-line-pattern-re
  '(;; These pattern are probably for Rhino:
    ("^js: \"\\(.+\\)\", line \\([0-9]+\\): \\(.+\\)$" 1 2 nil 3)
    ("^js: uncaught JavaScript \\(.+\\)$" nil nil nil 1)
    ;; For Rhino with jslint.js
    ("^Lint at line \\([[:digit:]]+\\) character \\([[:digit:]]+\\): \\(.+\\)$" nil 1 2 3)
    ;; These pattern are probably for SpiderMonkey:
    ("^\\(.+\\)\:\\([0-9]+\\)\: \\(SyntaxError\:.+\\)\:$" 1 2 nil 3)
    ("^\\(.+\\)\:\\([0-9]+\\)\: \\(strict warning: trailing comma.+\\)\:$" 1 2 nil 3))
  "Regexp matching JavaScript error messages")

(defcustom flymake-for-js-rhino-jar "/path/to/js.jar"
  "Path to Rihno jar file.
Download and install Rhino JavaScript engine from

  URL `http://www.mozilla.org/rhino/'

This variable should point to the file js.jar that is in the top
directory of the Rhino dir tree. \(It was differently named
earlier and might perhaps be renamed again.)"
  :type '(file :must-match t)
  :group 'flymake-for-js)

;;(setq flymake-log-level 3)
;;(setq flymake-for-js-rhino-use-jslint nil)
(defcustom flymake-for-js-rhino-use-jslint nil
  "Use jslint.js if this is non-nil.
jslint.js will give you warnings about style things like indentation too."
  :type 'boolean
  :group 'flymake-for-js)

(defcustom flymake-for-js-rhino-js (expand-file-name "rhino.js" flymake-for-js-dir)
  "Path to rhino.js.
Only used if `flymake-for-js-rhino-use-jslint' is nil.

This file and env.js must be placed in the same directory. Default
is this directory.

Those files comes with Rhino, see `flymake-for-js-rhino-jar'."
  :type '(file :must-match t)
  :group 'flymake-for-js)

(defcustom flymake-for-js-rhino-jslint (expand-file-name "jslint.js" flymake-for-js-dir)
  "Path to jslint.js.
Only used if `flymake-for-js-rhino-use-jslint' is t.

If you do not have this file you can download it from URL
`http://www.jslint.com/rhino/jslint.js'. I had to change quit(2)
to quit(0) in it \(which seems like a bug in `flymake-mode' to
me)."
  :type '(file :must-match t)
  :group 'flymake-for-js)

;;(flymake-for-js-check-rhino-js)
(defun flymake-for-js-check-rhino-js ()
  "Checks that the path to env.js is ok."
  (with-current-buffer (find-file-noselect flymake-for-js-rhino-js)
    (let* ((proj-folder (file-name-as-directory (file-name-directory (buffer-file-name))))
           (proj-line (concat "var project_folder = 'file:///" proj-folder "';"))
           (proj-line-re "^\\W*var\\W+project_folder\\W*=\\W*"))
      (save-restriction
        (widen)
        (goto-char (point-max))
        (if (re-search-backward proj-line-re nil t)
            (let ((beg (line-beginning-position))
                  (end (line-end-position)))
              (unless (string= (buffer-substring-no-properties beg end)
                               proj-line)
                (delete-region beg end)
                (insert proj-line)
                (basic-save-buffer)))
          (goto-char (point-min))
          (insert proj-line "\n")
          (basic-save-buffer))))))

(defcustom flymake-for-js-engine 'rhino
  "Javascript engine to use.
You may have to restart Emacs after changing this - if you can
not figure out what buffers and processes to kill.

I have only been able to test Rhino since I do not have
SpiderMonkey."
  :type '(choice (const :tag "Rhino" rhino)
                 (const :tag "SpiderMonkey" spidermonkey))
  :group 'flymake-for-js)

(defun flymake-for-js-init ()
  (set (make-local-variable 'flymake-err-line-patterns) flymake-for-js-err-line-pattern-re)
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (flymake-for-js-check-has-engine)
    (cond
     ((eq flymake-for-js-engine 'rhino)
      (list "java" (list "-jar" flymake-for-js-rhino-jar
                         (if flymake-for-js-rhino-use-jslint
                             flymake-for-js-rhino-jslint
                           flymake-for-js-rhino-js)
                         local-file)))
     ((eq flymake-for-js-engine 'spidermonkey)
      (list "js" (list "-s" local-file)))
     (t
      (error "Bad value: %s" flymake-for-js-engine)))))

(defvar flymake-for-js-has-engine nil)

(defun flymake-for-js-check-has-engine ()
  "Check for the needed files."
  (if flymake-for-js-has-engine
      t
    (cond
     ;; Rhino
     ((eq flymake-for-js-engine 'rhino)
      (unless (executable-find "java")
        (error "Could not find java executable"))
      (unless (file-exists-p flymake-for-js-rhino-jar)
        (error "Could not find file %s\n\nPlease customize flymake-for-js-rhino-jar\n"
               flymake-for-js-rhino-jar))
      (if flymake-for-js-rhino-use-jslint
          (unless (file-exists-p flymake-for-js-rhino-jslint)
            (error "Could not find file %s" flymake-for-js-rhino-jslint))
        (unless (file-exists-p flymake-for-js-rhino-js)
          (error "Could not find file %s" flymake-for-js-rhino-js))
        (flymake-for-js-check-rhino-js)))
     ;; SpiderMonkey
     ((eq flymake-for-js-engine 'spidermonkey)
      (unless (executable-find "js")
        (error "Could not find js program")))
     (t
      (error "Bad value: %s" flymake-for-js-engine)))
    (setq flymake-for-js-has-engine t)))

(defun flymake-for-js-loader ()
  (dolist (rec flymake-allowed-js-file-name-masks)
    (push 'flymake-allowed-file-name-masks rec)))

(provide 'flymake-for-js)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; flymake-for-js.el ends here
