;;; idxsql.el --- For access to search engines via SQL
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2011-01-05 Wed
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
;; Support in `idxsearch' for indexing search engines that you can ask
;; though SQL.
;;
;; The index of some search engines can be accessed through SQL queries.
;; This file provide generic and specific implementation for such access.
;;
;; Currently this depends on an externa ruby script that does the
;; SQL-querying and send backs the result to Emacs.
;;
;; At the moment there are support for these search engines:
;; - Windows Desktop Search, see `idxwds-search'.
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

;;;###autoload
(defun idxwds-search (search-patt file-patts root)
  (idxsql-search search-patt file-patts root))

(defun idxsql-search (search-patt file-patts root)
  (let* ((search-patts (idxsearch-ggl-split search-patt))
         (file-patt (mapconcat (lambda (fp) (replace-regexp-in-string "*" "%" fp t t))
                               file-patts
                               ","))
         (options (let ((opts (list "--root" root
                                    "--filepatt" file-patt
                                    "--query" (mapconcat 'identity search-patts ","))))
                    (when idxsearch-show-details       (setq opts (cons "--details" opts)))
                    (when idxsearch-grep-in-text-files (setq opts (cons "--greptext" opts)))
                    opts))
         (cmds (idxsearch-make-command options))
         (cmd (car cmds))
         (script-type (cadr cmds))
         (default-directory (car (split-string (cadr (member "--root" options)) ",")))
         ;; Coding systems. To my surprise this seems to work for ruby
         ;; at least!
         (process-coding-system-alist
          '((".*idxsearch.ps1.*" . utf-8)
            (".*powershell.exe.*" . utf-8)
            (".*ruby.exe" . utf-8)
            )))
    (unless (and (get 'compilation-start 'command-can-be-list)
                 (not (eq script-type 'powershell)))
      ;; Fix-me: Or rather hope that my patch to compilation-start is
      ;; accepted soon...
      (setq cmd (mapconcat 'identity cmd " ")))
    (message "cmd=%S" cmd)
    (with-current-buffer (compilation-start cmd 'idxsearch-mode)
      ;;(run-with-idle-timer 3 nil 'idxsearch-insert-search-info-header root search-patt (mapconcat 'identity file-patts "; "))
      (visual-line-mode 1)
      (setq wrap-prefix "           ")
      (orgstruct-mode)
      )))


(provide 'idxsql)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; idxsql.el ends here
