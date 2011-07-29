;;; idxdoc.el --- Support docindexer in idxsearch.el
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2011-01-03 Mon
;; Version:
;; Last-Updated: 2011-03-18 Fri
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
;; Support for DocIndexer (http://www.methods.co.nz/docindexer/) in
;; `idxsearch'.
;;
;;
;; To change which files types are searched during the indexing
;; process follow this recipe from Eli Zaretski:
;;
;; - Find config.py in the docindexer installation directory and edit
;;   it to add a line for *.el files.  (If it is not there then get it
;;   from the source distribution.)
;;
;; - Find a file named library.zip in the docindexer installation
;;   directory.  This is the class library used by docindexer.
;;
;; - Replace the file docindexer/config.pyc in library.zip with the
;;   edited docindexer/config.py.  Note: the .pyc extension means that
;;   the file was compiled by Python; the corresponding .py file is
;;   not compiled, but it will be used anyway -- this is similar to
;;   what Emacs does with *.el and *.elc files.
;;
;; - Run "docindexer --config" and make sure you see the *.el line in
;;   the output.
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

;; (eval-when-compile (require 'idxsearch))


(defgroup idxdoc nil
  "Customization group for idxdoc."
  :group 'idxsearch)

;; (defcustom idxdoc-dirs nil
;;   "Index roots."
;;   :type '(repeat directory)
;;   :group 'idxdoc)

;;(setq idxdoc-install-dir "C:/Program Files/docindexer/")
(defcustom idxdoc-install-dir ""
  "Installation directory for docindex."
  :type 'directory
  :group 'idxdoc)

(defcustom idxdoc-include-files "*.pdf|*.org"
  "File types to index.
This will be the value for docindex -i switch when indexing."
  :type 'string
  :group 'idxdoc)

;;;###autoload
(defun idxdoc-index-files ()
  "Index files.
Look for a docindexer index file and offer to reindex if found.
Otherwise offer to index current directory."
  ;; docindex.exe" -r root -i "*.pdf|*.org|*.html|*.htm"
  (interactive)
  (cond
   ((= 0 (length idxdoc-install-dir))
    (when (y-or-n-p "You must tell where DocIndexer is installed first. Do that now? ")
      (customize-group 'idxdoc)))
   (t
    (let* ((dom-root (locate-dominating-file default-directory ".docindexer"))
           (idx-root (if dom-root
                         (read-directory-name "Reindex files in: " dom-root)
                       (read-directory-name "Index files in: " default-directory)))
           (idxprog (expand-file-name "docindex" idxdoc-install-dir)))
    (let ((outbuf (get-buffer-create "*DocIndexer indexing output*")))
      (display-buffer outbuf)
      (with-current-buffer outbuf
        (unless (= 0 (buffer-size))
          (goto-char (point-max))
          (insert "\n======================================\n\n")))
      (message "Starting indexing")
      ;; Fix-me
      (start-process "DocIndexer" outbuf
                     idxprog "-r" idx-root "-i" idxdoc-include-files)
      )))))

;;;###autoload
(defun idxdoc-search (search-patt file-patts root)
  "Search with DocIndexer.
SEARCH-PATT, FILE-PATTS and ROOT are the same as for `idxsearch',
except that FILE-PATTS is a list.

Indexing is not incremental and not automatic with DocIndexwer, but it is rather fast.
You can start the indexing with the command `idxdoc-index-files'.

There is a customization group you can access with:

  M-x customize-group RET idxdoc"
  (let* ((words-or-phrases (idxsearch-ggl-split search-patt))
         (or+and (idxsearch-mk-and-grep words-or-phrases))
         (grep-or-patt   (nth 0 or+and))
         (grep-and-patts (nth 1 or+and))
         (index-patts (mapcar (lambda (w-or-p)
                                (if (or t (string-match "[^a-z0-9]" w-or-p))
                                    (concat "\"" w-or-p "\"")
                                  w-or-p))
                              words-or-phrases))
         (index-patt (mapconcat 'identity index-patts " "))
         (dom-root (locate-dominating-file root ".docindexer"))
         (docsearch "docsearch")
         (buffer (current-buffer))
         (cnt-hits 0)
         win maxw
         exts not-exts)
    (dolist (fp file-patts)
      (if (and (string-match (rx "*." (submatch (+ (not (any ".*")))) eos)
                             fp)
               (> 6 (length (match-string 1 fp)))
               nil ;; fix-me: bug in docsearch, can only handle one ext:?
               )
          ;; We have probably got an extension
          (setq exts (cons (match-string-no-properties 1 fp) exts))
        ;; This was something else, just convert it to a regexp:
        (let* ((fp1 (replace-regexp-in-string "\\." "\\." fp t t))
               (fp2 (replace-regexp-in-string "*" ".*" fp1 t t)))
          (setq not-exts (cons fp2 not-exts)))))
    (when exts
      (setq exts (mapconcat 'identity exts " ")))
    (when not-exts
      (setq not-exts (mapconcat 'identity not-exts "\\|")))
    ;;(setq win (display-buffer (current-buffer)))
    (setq win (get-buffer-window (current-buffer)))
    (setq maxw (window-width win))
    (when (and idxdoc-install-dir
               (< 0 (length idxdoc-install-dir)))
      (setq docsearch (expand-file-name "docsearch" idxdoc-install-dir)))

    (cond
     (  (not (executable-find docsearch))
        (insert "\n"
                (propertize "Error:" 'font-lock-face 'font-lock-warning-face)
                "\nPlease ")
        (insert-text-button "customize"
                            'action
                            (lambda (btn)
                              (customize-option 'idxdoc-install-dir)))
        (insert " idxdoc-install-dir to\ntell where docindexer is installed."))

     (  (not dom-root)
        (insert "\n"
                (propertize "Error:" 'font-lock-face 'font-lock-warning-face)
                "\nThe directory "
                (propertize root 'font-lock-face 'font-lock-string-face)
                " is not known to be indexed.\n\n"
                "You can use "
                (propertize "M-x idxdoc-index-files" 'font-lock-face font-lock-builtin-face)
                " to index it."
                ))

     (t
      ;; docsearch "ext:(org pdf)" "something wrong" AND good
      (setq exts nil) ;; fix-me
      (let* ((ds (let ((exec-path (list idxdoc-install-dir)))(executable-find "docsearch")))
             (args
              (let ((patts nil))
                (dolist (patt index-patts)
                  (when patts (setq patts (cons "AND" patts)))
                  (setq patts (cons patt patts)))
                (setq patts (cons "." patts))
                (reverse patts)))
             sts
             (buf (get-buffer-create "docsearch-out"))
             (num-hits 0)
             (debug nil))
        (insert "Search started at " (format-time-string "%Y-%m-%d %T\n\n"))
        (with-current-buffer buf
          (erase-buffer)
          (when debug (display-buffer buf))
          ;;(call-process-shell-command cmd nil t t)
          (setq sts (apply 'call-process ds nil t nil args))
          (unless (eq sts 0) (display-buffer buf))
          (goto-char (point-min))
          (when (looking-at "WARNING: could not properly read security provider files:\n")
            (forward-line 4)
            (delete-region (point-min) (point)))
          (goto-char (point-min))
          (while (not (eobp))
            (let ((file (buffer-substring (point-at-bol) (point-at-eol))))
              (when (or (not not-exts)
                        (string-match not-exts file))
                (with-current-buffer buffer
                  (setq num-hits (1+ num-hits))
                  (setq file (file-relative-name file))
                  (insert "* File " file " matches\n")
                  (when (and idxsearch-grep-in-text-files
                             (idxsearch-text-p file))
                    (idxsearch-grep file grep-or-patt nil maxw))
                  (sit-for 0)
                  )))
            (forward-line)))
        (unless debug (kill-buffer buf))
        (insert (format "\nMatched %d files\n" num-hits))
        (insert "Search finished at " (format-time-string "%Y-%m-%d %T\n\n"))
        )))))

(provide 'idxdoc)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; idxdoc.el ends here
