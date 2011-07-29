;;; idxsearch.el --- Integration with indexed search engines
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2010-12-21 Tue
;; Version:
;; Last-Updated:
;; URL:
;; Keywords:  matching
;; Compatibility:
;;
;; Features that might be required by this library:
;;
;;   `backquote', `bytecomp', `comint', `compile', `flymake',
;;   `flymake-css', `flymake-java-1', `flymake-js', `nxhtml-base',
;;   `powershell-mode', `ring', `tool-bar', `warnings'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Integration with indexed search engines.
;; For more information see `idxsearch'.
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

;; Fix-me: option for matching long lines with all patterns, instead
;; of any.

;; Fix-me: Extract this and use as a loop for new queries. Allow
;; things like TAB cycle visibility in the output buffer. Add a
;; "reenter query command".

(eval-when-compile (require 'compile))
(eval-when-compile (require 'cl))
(eval-when-compile (require 'idxgds))
(eval-when-compile (require 'idxsql))
(eval-when-compile (require 'idxdoc))
(require 'grep)
(require 'org)
(require 'nxhtml-base)

;; Fix-me: The byte compiler should not complain about these:
(declare-function orgstruct-mode "orgstruct-mode")


(defvar idxsearch-search-patt-hist nil)
(defvar idxsearch-file-patt-hist nil)

(defvar idxsearch-search-script (expand-file-name "etc/wds/idxsearch.rb" nxhtml-install-dir))
;; Fix-me: maybe. I am unable to get the ps1 version to work nicely
;; from within Emacs. It works but you can't have spaces in the script
;; file name, it is slower than the ruby version and there are more
;; chars not displayed correctly (since it works through cmd.exe
;; perhaps, but that is the only way to get it working currently,
;; i.e. Powershell 2.0, WinXP).
;;
;; Therefor I did not finish all details in the ps1 script and it is
;; currently not usable with idxsearch.el. (Though it should be easy
;; to fix, it is just output formatting that differs.)
;;
;;(setq idxsearch-search-script (expand-file-name "etc/wds/idxsearch.ps1" nxhtml-install-dir))

(defgroup idxsearch nil
  "Customization group for `idxsearch'."
  :group 'matching)

(defvar idxsearch-engines
  '((idxdoc-search "DocIndexer")
    (idxgds-search "Google Desktop Search")
    (idxwds-search "Windows Desktop Search"))
  "Search engines.")


;; (idxsearch-funp idxsearch-engine)
(defun idxsearch-funp (fun)
  (assoc fun idxsearch-engines))

(define-widget 'idxsearch-function 'function
  "A index search function known by `idxsearch."
  :complete-function (lambda ()
                       (interactive)
                       (lisp-complete-symbol 'idxsearch-funp))
  :prompt-match 'idxsearch-funp
  :prompt-history 'widget-function-prompt-value-history
  :match-alternatives '(idxsearch-funp)
  :validate (lambda (widget)
              (unless (idxsearch-funp (widget-value widget))
                (widget-put widget :error (format "Unknown index search function: %S"
                                                  (widget-value widget)))
                widget))
  :value 'fundamental-mode
  :tag "Index search specific function")

;;(setq idxsearch-engine 'idxgds-search)
;;(setq idxsearch-engine 'idxwds-search)
;;(setq idxsearch-engine 'idxdoc-search)
(defcustom idxsearch-engine (cond
                             ((idxgds-query-url-p) 'idxgds-search)
                             (t (if (eq system-type 'windows-nt)
                                    'idxwds-search
                                  'idxdoc-search)))
  "Desktop search engine for `idxsearch' to use.
The currently supported search engines are:

* DocIndexer, see `idxdoc-search'.
* Google Desktop Search
  You need to set `idxgds-query-url' to use it.
* Windows Desktop Search
"
  :type 'idxsearch-function
  :group 'idxsearch)

(defcustom idxsearch-dflt-file-pattern "*.org, *.pdf"
  "Default file pattern for `idxsearch'.
Comma-separated list.  Each part corresponds to the end of a file
name.  '*' may be used as a wildcard."
  :type 'string
  :group 'idxsearch)

(defcustom idxsearch-show-details nil
  "Show details in search result if they are available."
  :type 'boolean
  :group 'idxsearch)

(defcustom idxsearch-grep-in-text-files nil
  "If the hit file is a text file grep inside it."
  :type 'boolean
  :group 'idxsearch)

;;;###autoload
(defun idxsearch (search-patt file-patt root)
  "Search using an indexed search engine on your pc.
This searches all the content you have indexed there.

The string SEARCH-PATT may consist of single words or phrases
\"enclosed like this\".  All words and phrases must match for a
file to match.

If the file is a text file it will be searched for all words and
phrases so you get direct links into it.

FILE-PATT is a comma-separated list of filenames with '*' as a
wildcard.  It defaults to `idxsearch-dflt-file-pattern'.

ROOT is the root directory containing files to search.


To do the search an indexed search engine is used.  You choose
which one by customizing `idxsearch-engine'."
  (interactive
   ;; Fix-me: Different search engines have different pattern
   ;; styles. Use different hist vars? Different prompts?
   (let* ((def-str (grep-tag-default))
          (str (read-string "Search pattern: " def-str 'idxsearch-search-patt-hist))
          (def-fil idxsearch-dflt-file-pattern)
          (fil (read-string "File name pattern: " def-fil 'idxsearch-file-patt-hist))
          (dir (read-directory-name "Indexed search in directory tree: ")))
     (list str fil dir)))
  (let ((item-patt (rx (or (and "\""
                                (submatch (* (not (any "\""))))
                                "\"")
                           (submatch (and word-start
                                          (+ (not space))
                                          word-end)))))
        (start 0)
        strs
        (file-patts (split-string file-patt (rx (* whitespace) "," (* whitespace))))
        (outbuf (get-buffer-create "*idxsearch*")))
    (while (setq start (string-match item-patt search-patt start))
      (let ((y (or (match-string 1 search-patt)
                   (match-string 2 search-patt))))
        (setq start (+ start (length y)))
        (setq strs (cons y strs))))
    (display-buffer outbuf)
    (with-current-buffer outbuf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (unless (derived-mode-p 'idxsearch-mode) (idxsearch-mode))
        (unless orgstruct-mode (orgstruct-mode))
        (visual-line-mode 1)
        (setq wrap-prefix "           ")
        (idxsearch-insert-search-info-header root search-patt file-patt)
        (setq default-directory root)
        (funcall idxsearch-engine search-patt file-patts root)))))

(defun idxsearch-insert-search-info-header (root search-patt file-patt)
  (let ((here (point-marker))
        (engine-name (nth 1 (assoc idxsearch-engine idxsearch-engines))))
    (set-marker-insertion-type here t)
    (goto-char (point-min))
    (if (looking-at "-\*- mode: ")
        (goto-char (point-at-eol))
      (insert "-*- mode: idxsearch; default-directory: \"" root "\" -*-\n"))
    (insert "Using " engine-name "\n")
    (insert "    Search: " search-patt "\n")
    (insert "  In files: " file-patt "\n")
    (goto-char here)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Pattern building helpers

;; (idxsearch-ggl-split "\"hi there\" some other where")
(defun idxsearch-ggl-split (search-patt)
  (let ((item-patt (rx (or (and "\""
                                (submatch (* (not (any "\""))))
                                "\"")
                           (submatch (and word-start
                                          (+ (not space))
                                          word-end)))))
        (start 0)
        strs)
    (while (setq start (string-match item-patt search-patt start))
      (let ((y (or (match-string 1 search-patt)
                   (match-string 2 search-patt))))
        (setq start (+ start (length y)))
        (setq strs (cons y strs))))
    strs))

(defun idxsearch-mk-and-grep (grep-patts)
  (let ((patterns (mapcar (lambda (pat)
                            (concat "\\<" pat "\\>"))
                          grep-patts))
        (or-pattern (regexp-opt grep-patts)))
    (list or-pattern patterns)))




;; (setq locate-make-command-line 'idxsearch-locate-make-command-line)
;; (idxsearch-locate-make-command-line "some.fil")
(defun idxsearch-locate-make-command-line (search)
  (let* ((cmd (car (idxsearch-make-command
                    (list
                     "--root"   default-directory
                     "--locate" "locate"
                     "--query"  search)))))
    cmd))

(defun idxsearch-make-command (options)
  (let* ((script-ext (file-name-extension idxsearch-search-script))
         (script-type (cond
                       ((string= "ps1" script-ext) 'powershell)
                       ((string= "rb"  script-ext) 'ruby)
                       (t 'unknown)))
         (command-list (append `(,(convert-standard-filename idxsearch-search-script))
                               options)))
    (when (eq script-type 'ruby)
      (setq command-list (append '("ruby.exe") command-list)))
    (list command-list script-type)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Output buffer font lock etc

(defvar idxsearch-link-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [(control ?m)] 'idxsearch-org-open-at-point)
    map))

(defun idxsearch-hit-marker (bound)
  (let ((here (point)))
    (while (and (< (point) bound)
                (re-search-forward "\\[\\[\\(.*?\\)\\]\\[\\(.*?\\)\\]" bound t))
      (let ((b0 (match-beginning 0))
            (e0 (match-end 0))
            (m1 (match-string 1))
            (b2 (match-beginning 2))
            (e2 (match-end 2)))
        (with-silent-modifications
          (put-text-property b0 b2 'invisible t)
          (put-text-property (- e0 1) (+ e0 1) 'invisible t)
          (put-text-property b2 e2 'help-echo m1)
          (put-text-property b2 e2 'keymap idxsearch-link-keymap)
          (put-text-property b2 e2 'mouse-face 'highlight)
          (put-text-property b2 e2 'font-lock-face 'font-lock-function-name-face)
          )))
    (goto-char here)
    (while (and (< (point) bound)
                (re-search-forward "{{{\\(.*?\\)}}}" bound t))
      (let ((b0 (match-beginning 0))
            (e0 (match-end 0))
            (b1 (match-beginning 1))
            (e1 (match-end 1)))
        (with-silent-modifications
          (put-text-property b0 (- b1 0) 'invisible t)
          (put-text-property e1 (- e0 0) 'invisible t)
          (put-text-property b1 e1 'font-lock-face 'font-lock-keyword-face)
          )))
    nil))

(defun idxsearch-org-open-at-point ()
  (interactive)
  (let* ((file (idxsearch-find-filename))
         (full (expand-file-name file))
         (default-directory (file-name-directory full)))
    (org-open-at-point)))

(defvar idxsearch-hit-face compilation-info-face
  "Face name to use for search hits.")

(defun idxsearch-find-filename ()
  (save-match-data
    (let ((here (point))
          (file-loc-patt "^\\* File \\(.*\\) matches$"))
      (unless (re-search-backward file-loc-patt nil t)
        (error "Expected to find line matching %S above" file-loc-patt))
      (let* ((file-name (match-string-no-properties 1))
             (full-file (expand-file-name file-name)))
        (goto-char here)
        ;;(caar (nth 2 (nth 0 file-msg)))
        `(,full-file)
        ))))

;; (REGEXP FILE [LINE COLUMN TYPE HYPERLINK HIGHLIGHT...])
(defconst idxsearch-error-regexp-alist
  `(
    ;; For ruby errors etc (this must be first here):
    ,(cdr (assoc 'gnu compilation-error-regexp-alist-alist))
    ;; Fix-me: Remove debugging [abc]
    ("^[abc]\\([0-9]+\\):\\([0-9]+\\):" idxsearch-find-filename 1 2)
    ("^\\* File \\(.+\\) matches$" 1 nil nil 0 1)
    )
  "Regexp used to match search hits.  See `compilation-error-regexp-alist'.")

;; fix-me: Maybe add some highlighting to show that there headerlines
;; are handled by org?
(defvar idxsearch-mode-font-lock-keywords
  '(;; configure output lines.
    ("^\\(Search \\(?:started\\|finished\\)\\).*"
     (0 '(face nil message nil help-echo nil mouse-face nil) t)
     (1 compilation-info-face))
    ("^Compilation \\(exited abnormally\\|interrupt\\|killed\\|terminated\\|segmentation fault\\)\\(?:.*with code \\([0-9]+\\)\\)?.*"
     (0 '(face nil message nil help-echo nil mouse-face nil) t)
     (1 compilation-error-face)
     (2 compilation-error-face nil t))
    ("^  Snippet:\\|Title:\\|Authors:"
     ;;(0 font-lock-type-face))
     (0 'shadow))
    (idxsearch-hit-marker)
    )
  "Additional things to highlight in idxsearch mode.
This gets tacked on the end of the generated expressions.")

(define-compilation-mode idxsearch-mode "Search"
  "Mode for `idxsearch' output."
  (set (make-local-variable 'compilation-error-face) idxsearch-hit-face))

;; Fix-me: ruby instead
;; (defun idxsearch-add-powershell-kw ()
;;   (let ((kw `((,(cadr powershell-compilation-error-regexp-alist)
;;               (1 'compilation-error)
;;               (2 compilation-line-face nil t)
;;               (0
;;                (compilation-error-properties '1 2 nil nil nil '2 'nil)
;;                append))))
;;         )
;;     (font-lock-add-keywords 'idxsearch-mode kw)))
;; (add-hook 'idxsearch-mode-hook 'idxsearch-add-powershell-kw)

;;(idxsearch-text-p "a.org")
;;(idxsearch-text-p "a.el")
;;(idxsearch-text-p "a.rb")
;;(idxsearch-text-p "a.png")
;;(idxsearch-text-p "a.pdf")
;;(idxsearch-text-p "a.cmd")
;;(idxsearch-text-p "a")
(defvar idxsearch-non-text "\.\\(pdf\\|doc\\)\\'")
(defun idxsearch-text-p (file)
  (catch 'result
    ;; Look at extensions first because it is quicker!
    (let ((case-fold-search (memq system-type '(windows-nt cygwin)))
          (file (file-name-sans-versions file)))
      ;; Known non-text?
      (when (string-match idxsearch-non-text file)
        (throw 'result nil))
      ;; Image file?
      (when (assoc-default file image-type-file-name-regexps 'string-match)
        (throw 'result nil))
      ;; Auto mode?
      (let ((name file)
            mode)
        ;; fix-me: stolen from set-auto-mode, but there must be
        ;; something wrong there! The "(when mode" is inside the loop.
        (while name
          (setq mode (assoc-default file auto-mode-alist 'string-match))
          (if (and mode
                   (consp mode)
                   (cadr mode))
              (setq mode (car mode)
                    name (substring name 0 (match-beginning 0)))
            (setq name)))
        (when mode
          (throw 'result t))))
    ;; Content specific?
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file nil  200)
        (goto-char (point-min))
        (let ((done (or
                     (when (looking-at auto-mode-interpreter-regexp)
                       (let ((mode (match-string 2)))
                         (assoc (file-name-nondirectory mode) interpreter-mode-alist)))
                     (progn
                       (narrow-to-region (point-min) (min (point-max)
                                                          (+ (point-min)
                                                             magic-mode-regexp-match-limit)))
                       (assoc-default nil magic-mode-alist
                                      (lambda (re dummy)
                                        (if (functionp re)
                                            (funcall re)
                                          (looking-at re))))))))
          (when done (throw 'result done)))))
    nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Searching in text files

;; Fix-me: split pattern, match ALL
;; each pattern a word
(defun idxsearch-grep (file or-pattern and-patterns maxw)
  (let* ((old-buf (find-buffer-visiting file))
         (buf (or old-buf
                  ;;(find-file-noselect file)
                  (with-current-buffer (generate-new-buffer "idxsearch-grep")
                    (insert-file-contents file)
                    (current-buffer))
                  ))
         (curbuf (current-buffer))
         here
         (format-w 10)
         (row-format (format "%%-%ds" format-w))
         (num-lines 0))
    (setq maxw (or maxw 80))
    (setq maxw (- maxw format-w 2))
    (with-current-buffer buf
      (setq here (point))
      (save-restriction
        (widen)
        (goto-char (point-min))
        (while (re-search-forward or-pattern nil t)
          (setq num-lines (1+ num-lines))
          (let* ((beg (match-beginning 0))
                 (end (match-end       0))
                 (row (line-number-at-pos beg))
                 (col (- beg (point-at-bol)))
                 (cnd (- end (point-at-bol)))
                 (len (- cnd col))
                 (line (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
                 show
                 (line-matches t)
                 (part "e"))
            (when (< 1 (length and-patterns))
              (dolist (pat and-patterns)
                (when line-matches
                  (unless (string-match pat line)
                    (setq line-matches nil)))))
            (goto-char (point-at-eol))
            (when line-matches
              (setq line
                    (with-temp-buffer
                      (insert line)
                      (untabify (point-min) (point-max))
                      (buffer-substring (point-min) (point-max))))
              ;; (with-current-buffer curbuf (insert line "\n"))
              (if (< (length line) maxw)
                  (progn
                    (setq part "a")
                    (setq show line))
                (if (< len maxw)
                    (let* ((pad (/ (- maxw len) 2))
                           (start (max 0 (- col pad)))
                           (stop (+ start (- maxw 3)))
                           (over (- stop (length line))))
                      (setq part "b")
                      ;; (with-current-buffer curbuf (insert (format "%d %d %d\n" start stop over)))
                      (when (< 0 over)
                        (setq part "d")
                        (setq start (- start over))
                        (setq stop  (- stop  over)))
                      (setq show (concat "_" (substring line start stop) "_")))
                  (setq part "c")
                  (setq show (concat "_" (substring line col (+ col (- maxw 3))) "_"))))
              (setq show (replace-regexp-in-string or-pattern
                                                   (lambda (rep)
                                                     (concat "{{{" rep "}}}"))
                                                   show))
              (with-current-buffer curbuf
                (insert part (format row-format
                                     (format "%d:%d:" row col))
                        show "\n"))))))
      (goto-char here))
    (when (< 0 num-lines) (insert "\n"))
    (unless old-buf (kill-buffer buf))))

(provide 'idxsearch)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; idxsearch.el ends here
