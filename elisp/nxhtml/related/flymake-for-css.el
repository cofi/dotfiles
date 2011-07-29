;;; flymake-for-css.el --- Flymake setup for css files
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2009-11-21 Sat
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
;;  See variable `flymake-for-css-validator-jar' for instructions for how
;;  to set this up.
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


(eval-when-compile (require 'cl))
(require 'flymake)
(require 'xml)


(defcustom flymake-allowed-css-file-name-masks '(("\\.css\\'" flymake-for-css-init))
  "Filename extensions that switch on js syntax checks."
  :type '(repeat (list (regexp :tag "File name regexp")
                       (function :tag "Init function")
                       (choice (const :tag "No cleanup function" nil)
                               (function :tag "Cleanup function"))))
  :group 'flymake-files)


(defvar flymake-for-css-err-line-pattern-re '(("^file:\\([^:]+\\):\\([^:]+\\):\\(.*\\)" 1 2 nil 3))
  "Regexp matching CSS error messages")

(defcustom flymake-for-css-validator-jar "~/.emacs.d/css-validator/css-validator.jar"
  "Full path to css-validor.jar file.
You need the css-validator.jar and some other files for flymake
for CSS to work. The instructions below tell you how to get and
install it. The instructions are copied from url

  `http://www.emacswiki.org/emacs/FlymakeCSS'

Get url `http://www.w3.org/QA/Tools/css-validator/css-validator.jar'
and create a directory named ‘lib’ in the same directory. Copy to
the ‘lib’ dir the following jars:

 * commons-collections-3.2.1.jar
 * jigsaw.jar
 * velocity-1.6.1.jar
 * xml-apis.jar
 * commons-lang-2.4.jar
 * tagsoup-1.2.jar
 * xercesImpl.jar

From:

 URL `http://jigsaw.w3.org/Distrib/jigsaw_2.2.6.tar.gz'
 URL `http://www.apache.org/dist/commons/collections/binaries/commons-collections-3.2.1-bin.tar.gz'
 URL `http://www.apache.org/dist/commons/lang/binaries/commons-lang-2.4-bin.tar.gz'
 URL `http://archive.apache.org/dist/velocity/engine/1.6.1/velocity-1.6.1.tar.gz'
 URL `http://www.apache.org/dist/xerces/j/Xerces-J-bin.2.9.1.tar.gz'
 URL `http://home.ccil.org/~cowan/XML/tagsoup/tagsoup-1.2.jar'

Test validating some CSS file by running:

  java -jar css-validator.jar file:somecssfile.css"
  :type 'file
  :group 'flymake)
;;(setq flymake-for-css-validator-jar "c:/dl/programs/css-valid/css-validator.jar")

(defun flymake-for-css-init ()
  (set (make-local-variable 'flymake-err-line-patterns) flymake-for-css-err-line-pattern-re)
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (unless (file-exists-p flymake-for-css-validator-jar)
      (error "Can't find css-validator.jar: %s\n\nPlease customize option flymake-for-css-validator-jar\n"
             flymake-for-css-validator-jar))
    (list "java"
          (list "-jar" flymake-for-css-validator-jar
                "-output" "gnu"
                (concat "file:" local-file)))))

(defun flymake-for-css-loader ()
  (dolist (rec flymake-allowed-css-file-name-masks)
    (push 'flymake-allowed-file-name-masks rec)))


;;(defun flymake-make-overlay (beg end tooltip-text face mouse-face)
;; (defadvice flymake-make-overlay (before
;;                                  flymake-for-css-ad-flymake-make-overlay
;;                                  activate
;;                                  compile)
;;   (ad-set-arg 2 (xml-substitute-numeric-entities (ad-get-arg 2))))

;; Fix-me: remove when this has been giving its proper place in Emacs.
(eval-when-compile
  (unless (fboundp 'xml-substitute-numeric-entities)
    (message "Use Emacs 22 workaround for newsticker--decode-numeric-entities")
    (defun xml-substitute-numeric-entities (string)
    "Decode SGML numeric entities by their respective utf characters.
This is just a copy of the function in newst-backen.el for Emacs
22 users.

This function replaces numeric entities in the input STRING and
returns the modified string.  For example \"&#42;\" gets replaced
by \"*\"."
    (if (and string (stringp string))
        (let ((start 0))
          (while (string-match "&#\\([0-9]+\\);" string start)
            (condition-case nil
                (setq string (replace-match
                              (string (read (substring string
                                                       (match-beginning 1)
                                                       (match-end 1))))
                              nil nil string))
              (error nil))
            (setq start (1+ (match-beginning 0))))
          string)
      nil))
    ))


(provide 'flymake-for-css)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; flymake-for-css.el ends here
