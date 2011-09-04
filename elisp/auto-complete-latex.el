;;; auto-complete-latex.el --- A LaTeX extention for auto-complete-mode

;; Copyright (C) 2010 tequilasunset

;; Author: tequilasunset <tequilasunset.mac@gmail.com>
;; Repository: http://bitbucket.org/tequilasunset/auto-complete-latex
;; Keywords: completion, LaTeX
(defconst ac-l-version "0.2.4")

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Requirements:

;;  auto-complete-mode:  http://cx4a.org/software/auto-complete/

;;; Installation:

;;  - Put files into load-path, and add the following into init file.
;;
;;      (require 'auto-complete-latex)
;;      (setq ac-l-dict-directory "/path/to/ac-l-dict/")
;;      (add-to-list 'ac-modes 'foo-mode)
;;      (add-hook 'foo-mode-hook 'ac-l-setup)
;;
;;  - If you get the error below
;;
;;      `variable binding depth exceeds max-specpdl-size',
;;
;;    add the following into init file.
;;
;;      (setq max-specpdl-size (+ 500 max-specpdl-size))

;;; Commentary:

;;  - Customize group:
;;
;;      M-x customize-group RET auto-complete-latex RET
;;
;;  - Don't use ac-sources:
;;
;;    Use `ac-l-sources' instead.
;;
;;  - Examples of configuration:
;;
;;      * Setup for AUCTeX
;;
;;          (when (require 'auto-complete-latex nil t)
;;            (setq ac-l-dict-directory "~/.emacs.d/ac-l-dict/")
;;            (add-to-list 'ac-modes 'latex-mode)
;;            (add-hook 'LaTeX-mode-hook 'ac-l-setup))
;;
;;      * Setup for YaTeX
;;
;;          (when (require 'auto-complete-latex nil t)
;;            (setq ac-l-dict-directory "~/.emacs.d/ac-l-dict/")
;;            (add-to-list 'ac-modes 'yatex-mode)
;;            (add-hook 'yatex-mode-hook 'ac-l-setup))
;;
;;        If you want to use command help in Japanese, put
;;        YATEXHLP.jp into ac-l-dict.
;;
;;  - ac-l-dict:
;;
;;    Files in it become sources, etc. Files are classified like below.
;;    If there are unnecessary files, remove them.
;;
;;      1. Basic files
;;
;;           basic-commands, basic-arguments, macro, latex-dot-ltx,
;;           platex-commands, platex-arguments, primitives,
;;           ptex-primitives
;;
;;         Keywords in these files become candidates for basic sources.
;;
;;      2. User files
;;
;;           user-commands, user-arguments
;;
;;         These files become user sources.
;;
;;      3. Help file
;;
;;           latex-help
;;
;;         This file become a LaTeX command help.
;;
;;      4. External package files
;;
;;         Files other than above become package sources. the
;;         form is NAME-TYPE-SYMBOL-REQUIRES.
;;
;;         NAME      Package or class file name. You can set the
;;                   dependence with using `ac-l-package-dependences'.
;;         TYPE      `c' (command) or `a' (argument).
;;         SYMBOL    Symbol property. `*' => `p'.
;;         REQUIRES  Requires property. `*' => not set.
;;
;;  - Commands that argument completion will work:
;;
;;      `ac-l-argument-regexps'
;;      `ac-l-file-regexps'
;;      `ac-l-label-regexps'
;;      `ac-l-bib-regexps'
;;
;;    Above are related variables. If you want to complete label
;;    names in argument of `\foo', write the following into init file.
;;
;;      (add-to-list 'ac-l-label-regexps "foo")
;;
;;  - Completion at point:
;;
;;    Two commands `ac-l-complete-labels' and `ac-l-complete-bibs'
;;    are provided to complete at point.
;;
;;  - A table of symbol properties:
;;
;;       SYMBOL |           MEANING
;;      --------+----------------------------------
;;         l    | LaTeX or pLaTeX
;;         a    | AMS packages
;;         b    | beamer
;;         h    | hyperlinks
;;         g    | graphics
;;         m    | math sign or equations
;;         c    | colors
;;         t    | tables
;;         f    | fonts
;;         p    | unclassified external packages
;;         F    | file names in a current directory
;;         L    | label names
;;         B    | bib keys
;;         u    | user-commands or user-arguments
;;
;;  - Startup improvement:
;;
;;    In case you use `ac-l-master-file', `ac-l-package-files' or
;;    `ac-l-bib-files', startup will be slower. If you are using
;;    ac-l-package-files, you can improve it with using the command
;;    `ac-l-write-package-files'.

;;; Code:

(require 'cl)
(require 'auto-complete)

(defgroup auto-complete-latex nil
  "Auto completion of LaTeX keywords."
  :group 'auto-complete
  :group 'tex
  :prefix "ac-l-")


;;;; variables
(defcustom ac-l-update-delay 0.8
  "Delay to update candidates."
  :type 'float
  :group 'auto-complete-latex)

(defcustom ac-l-master-file nil
  "Specify LaTeX master file path as string.
Parse master file's \\input and \\include(only).
Then create candidates from master file and parsed files."
  :type 'string
  :group 'auto-complete-latex)
(defvaralias 'ac-l-target 'ac-l-master-file)

(defcustom ac-l-sources nil
  "A list of user sources.
This is similar to `ac-sources', but you don't have to add
`ac-l-source-*' and below sources.

   ac-source-dictionary
   ac-source-files-in-current-dir
   ac-source-filename
   ac-source-words-in-*"
  :type '(repeat symbol)
  :group 'auto-complete-latex)

(defcustom ac-l-package-files nil
  "A list of package files (valid suffixes are .sty and .cls).
Parse LaTeX command definitions in them, and create candidates."
  :type '(repeat string)
  :group 'auto-complete-latex)

(defcustom ac-l-bib-files nil
  "A list of bib files (valid suffix is .bib).
Parse bibliography keys in them, and create candidates."
  :type '(repeat string)
  :group 'auto-complete-latex)

(defcustom ac-l-use-word-completion nil
  "If non-nil, use sources for normal word (text) completion."
  :type 'boolean
  :group 'auto-complete-latex)

;;; internal
(defvar ac-l-major-mode nil)
(defvar ac-l-master-p nil)
(defconst ac-l-command-prefix "\\\\\\([a-zA-Z@]+\\)")
(defvar ac-l-update-timer nil)


;;;; functions

;;; DB
;; package-cmds, package-args, cur-bib-tables, all-bib-tables, latex-cmds,
;; latex-args, package-sources, user-noprefix-sources, user-prefix-sources,
;; label-cands, bibitem-cands, bib-cands, filenames, label-tables, sources,
;; bibitem-tables, file-cmds, file-words, children,
(defconst ac-l-db (make-hash-table :test 'eq))

(defsubst ac-l-db-get (sym)
  (gethash sym ac-l-db))

(defsubst ac-l-db-set (sym value)
  (puthash sym value ac-l-db))

(defsubst ac-l-db-push (value sym)
  (puthash sym (cons value (gethash sym ac-l-db)) ac-l-db))

(defsubst ac-l-db-append (sym lst)
  (puthash sym (append (gethash sym ac-l-db) lst) ac-l-db))

;;; prefixes for arguments
(defcustom ac-l-argument-regexps
  '("\\(?:usep\\|RequireP\\)ackage" "documentclass" "begin" "end" "fnsymbol"
    "\\(?:this\\)?pagestyle" "bibliography\\(?:style\\)?" "pagenumbering"
    "\\(?:new\\|addto\\|set\\)counter" "[aA]lph" "arabic" "[rR]oman"
    "@\\(?:addtoreset\\|startsection\\|namedef\\|definecounter\\)"
    "addcontentsline" "numberwithin" "\\(?:text\\|page\\|f\\|define\\)color"
    "colorbox" "\\(?:column\\|row\\|cell\\|arrayrule\\|doublerulesep\\)color"
    "hypersetup" "include\\(?:graphics\\|slide\\)" "insert[a-z]+" "frame"
    "lst[a-zDIMS]+" "resetcount\\(?:er\\)?onoverlays" "tableofcontents"
    "movie" "hyperlink\\(?:movie\\|sound\\)" "multiinclude" "sound" "note"
    "trans[a-z]+" "use[a-z]*theme" "[a-z]+beamertemplate[a-z]*"
    "\\(?:use\\|set\\)beamer\\(?:color\\|font\\|covered\\)")
  "A list of regexps to match commands which take arguments."
  :type '(repeat regexp)
  :group 'auto-complete-latex)

(defcustom ac-l-file-regexps
  '("include\\(?:only\\|graphics\\)?" "input" "hypersetup")
  "A list of regexps to match commands which take file name argument."
  :type '(repeat regexp)
  :group 'auto-complete-latex)

(defcustom ac-l-label-regexps
  '("\\(?:page\\|auto\\|eq\\)?ref" "label")
  "A list of regexps to match commands which take label name argument."
  :type '(repeat regexp)
  :group 'auto-complete-latex)

(defcustom ac-l-bib-regexps
  '("\\(?:no\\|short\\)?cite[a-zA-Z]*" "bibitem")
  "A list of regexps to match commands which take bibliography argument."
  :type '(repeat regexp)
  :group 'auto-complete-latex)

(defun ac-l-prefix-in-paren (regexps)
  ;; This doesn't work as omni completion because the return is ac-point.
  (if (save-excursion
        (re-search-backward
         (concat "\\\\\\("
                 (mapconcat 'identity regexps "\\|")
                 "\\)\\*?\\(\\s([^]>}]*\\s)\\)*\\(\\s([^]>}]*\\)\\=") nil t))
      ac-point))

;;; read file data
(defsubst ac-l-convert-filename-to-file (filename)
  ;; faster than file-name-sans-extension
  (let ((nodir (file-name-nondirectory filename)))
    (if (string-match "\\(.+\\)\\.[^.]*$" nodir)
        (match-string 1 nodir)
      nodir)))

(defun* ac-l-read-bibs
    (&key (files ac-l-bib-files)
          (regexp "^@[^{@]+{\\([^ =,\t\n]*\\),\n[^@]+\\(^}\\)"))
  "Convert each bib file listed in FILES to a hash table."
  (dolist (filename files)
    (let* ((file (ac-l-convert-filename-to-file filename))
           (table (make-hash-table :test 'equal)))
      (ignore-errors
        (with-temp-buffer
          (insert-file-contents filename)
          (while (re-search-forward regexp nil t)
            (puthash (match-string-no-properties 1)
                     (match-string-no-properties 0)
                     table))))
      (ac-l-db-push (cons file table) 'all-bib-tables)
      (ac-l-db-push file 'filenames))))

;; k -> package name in `ac-l-package-files', v -> [cmds args]
(defconst ac-l-packages (make-hash-table :test 'equal))

(defun* ac-l-read-packages
    (&key (files ac-l-package-files)
          (cmd-re "\\\\\\(?:[a-z@]*def\\|let\\|new[a-z]+\\|providecommand\\|Declare[a-zA-Z@]+\\)\\*?[ \t]*{?\\\\\\([a-zA-Z]+\\)}?[ =\\#[{]")
          (arg-re  "\\\\\\(?:DeclareOption[a-zA-Z]*\\|new[a-z]+\\|@definecounter\\)\\*?[ \t]*{\\([a-zA-Z]+\\)}"))
  "Convert each package listed in FILES to an element of `ac-l-packages'."
  (dolist (filename files)
    (let ((file (ac-l-convert-filename-to-file filename))
          cand cmds args)
      (ignore-errors
        (with-temp-buffer
          (insert-file-contents filename)
          (while (re-search-forward cmd-re nil t)
            (setq cand (match-string-no-properties 1))
            (unless (member cand cmds)
              (push cand cmds)))
          (goto-char (point-min))
          (while (re-search-forward arg-re nil t)
            (setq cand (match-string-no-properties 1))
            (unless (member cand args)
              (push cand args)))))
      (puthash file (vector cmds args) ac-l-packages)
      (ac-l-db-push file 'filenames))))

(defcustom ac-l-dict-directory "~/.emacs.d/ac-l-dict/"
  "Path of the ac-l-dict."
  :type 'string
  :group 'auto-complete-latex)

(defun ac-l-write-package-files (dir)
  "Output candidates collected from files listed in `ac-l-package-files'.
You can use them in the ac-l-dict."
  (interactive (list (read-directory-name "Dir: " ac-l-dict-directory nil t)))
  (maphash (lambda (k v)
             (loop for (type cands) in `((c ,(aref v 0)) (a ,(aref v 1)))
                   when cands do
                   (with-temp-buffer
                     (insert (mapconcat 'identity (sort cands #'string<) "\n"))
                     (write-region (point-min) (point-max)
                                   (format "%s%s-%s-*-*" dir k type)))))
           ac-l-packages))

(defcustom ac-l-package-dependences
  '(("hyperref" . "beamer")
    ("color" . "colortbl\\|beamer")
    ("array" . "tabularx\\|colortbl"))
  "Alist of external package dependences.
Each element is the form (REQUIRED PACKAGE . PACKAGES). Package and
class files are treated equivalently. This is effective only for
files read from the ac-l-dict."
  :type '(repeat (cons string regexp))
  :group 'auto-complete-latex)

(defun* ac-l-set-help-doc
    (&optional (sources '(ac-l-source-latex-commands
                          ac-l-source-latex-arguments)))
  "Set document property to each source listed in SOURCES."
  (let* ((files (directory-files ac-l-dict-directory))
         (help-fn (cond
                   ((member "YATEXHLP.jp" files)
                    ;; `Warning: defvar ignored because kinsoku-limit is let-bound'.
                    (load "international/kinsoku")
                    'ac-l-yatex-jp-documentation)
                   ((member "latex-help" files)
                    'ac-l-latex2e-documentation))))
    (when help-fn
      (dolist (source sources)
        (push (cons 'document help-fn) (symbol-value source))))))

(defun ac-l-make-source-from-dir ()
  (dolist (file (directory-files ac-l-dict-directory nil "^[^.]"))
    (let ((sym "p")
          (prx ac-l-command-prefix)
          source package req)
      ;; parse file name
      (cond
       ((string-match "^\\([^-]+\\)-\\([^-]\\)-\\([^-]\\)-\\([^-]\\)$" file)
        (let* ((n (match-string 1 file))
               (T (match-string 2 file))
               (s (match-string 3 file))
               (r (match-string 4 file))
               (d (assoc-default n ac-l-package-dependences 'string=))
               (filenames (ac-l-db-get 'filenames)))
          (unless (member n filenames)
            (ac-l-db-set 'filenames (cons n filenames)))
          (if d (setq package (concat n "\\|" d)) (setq package n))
          (unless (string= s "*") (setq sym s))
          (unless (string= r "*") (setq req (string-to-number r)))
          (if (string= T "a")
              (setq prx 'ac-l-argument
                    source (intern (format "ac-l-source-%s-arguments" n)))
            (setq source (intern (format "ac-l-source-%s-commands" n))))))
       ((or (string= "macro" file)
            (string= "latex-dot-ltx" file)
            (string-match "^\\(ptex-\\)?primitives$" file)
            (string-match "^\\(basic\\|platex\\)-commands$" file))
        (setq source 'latex-cmd))
       ((string-match "^\\(basic\\|platex\\)-arguments$" file)
        (setq source 'latex-arg))
       ((cond ((string= "user-commands" file) t)
              ((string= "user-arguments" file) (setq prx 'ac-l-argument)))
        (setq sym "u"
              source (intern (concat "ac-l-source-" file)))))
      ;; read file contents
      (when source
        (let ((cands (with-temp-buffer
                       (insert-file-contents (concat ac-l-dict-directory file))
                       (split-string (buffer-string) "\n"))))
          (case source
            (latex-cmd (ac-l-db-append 'latex-cmds cands))
            (latex-arg (ac-l-db-append 'latex-args cands))
            (otherwise
             (set source (delq nil `(,(if package `(ac-l-package . ,package))
                                     ,(if (integerp req) `(requires . ,req))
                                     (symbol . ,sym)
                                     (prefix . ,prx)
                                     (candidates . ',cands))))
             (cond (package
                    (ac-l-db-push source 'package-sources))
                   ((string= sym "u")
                    (ac-l-db-push source 'user-prefix-sources))))))))))

;;; update file's info
(defstruct ac-l-info
  "Information about each tex file."
  modification words commands packages labels bibitems bibs)

;; k -> filename (full path), v -> struct
(defconst ac-l-structs (make-hash-table :test 'equal))
(defconst ac-l-children (make-hash-table :test 'equal))

(defsubst ac-l-split-string (str)
  (split-string str "\\([ \t\n]\\|%.*\n\\|,\\)+" t))

(defsubst ac-l-candidates-hash (regexp table beg end)
  (goto-char beg)
  (while (re-search-forward regexp end t)
    (puthash (match-string-no-properties 1)
             (match-string-no-properties 0)
             table)
    (goto-char (1+ (match-beginning 0)))))

(defun ac-l-make-info (struct filename &optional master)
  (let* ((word-re "[^\\,]\\(\\<[-'a-zA-Z]+\\>\\)")
         (package-re "^[^%\n]*\\\\\\(?:\\(?:usep\\|RequireP\\)ackage\\|documentclass\\)\\(?:\\[[^]]*\\]\\)?{\\([^}]+\\)")
         (lines ".*\n.*\n.*\n")
         (label-re "\\\\label{\\(\\(?:[^ }\t\n]\\)+\\)}")
         (label-re1 (concat "^[^%\n]*" label-re ".*$"))
         (label-re2 (concat "^" lines "[^%\n]*" label-re lines ".*$"))
         (bibitem-re "^[^%\n]*\\\\bibitem\\(?:\\[[^]]*\\]\\)?{\\(\\(?:[^ }\t\n]\\)+\\)}[^\\]*")
         (bib-re "^[^%\n]*\\\\bibliography{\\([^}]+\\)")
         (collect-p (not (or master (buffer-file-name))))
         (beg (point-min))
         (label-beg (save-excursion
                      (goto-char beg)
                      (forward-line 3)
                      (point)))
         (label-end (save-excursion
                      (goto-char (point-max))
                      (forward-line -3)
                      (point)))
         (label-ht (or (ignore-errors (clrhash (ac-l-info-labels struct)))
                       (make-hash-table :test 'equal)))
         (bibitem-ht (or (ignore-errors (clrhash (ac-l-info-bibitems struct)))
                         (make-hash-table :test 'equal)))
         (i 0)
         words commands packages bibs cand)
    (save-excursion
      (when (and ac-l-use-word-completion collect-p)
        (goto-char beg)
        (while (and (re-search-forward word-re nil t) (<= i 100))
          (setq cand (match-string-no-properties 1))
          (when (and (not (member cand words))
                     (>= (length cand) 3))
            (push cand words)
            (incf i))))
      (when collect-p
        (let ((latex-cmds (ac-l-db-get 'latex-cmds)))
          (goto-char beg)
          (while (re-search-forward ac-l-command-prefix nil t)
            (setq cand (match-string-no-properties 1))
            (unless (or (member cand commands)
                        (member cand latex-cmds))
              (push cand commands)))))
      (when master
        (goto-char beg)
        (while (re-search-forward package-re nil t)
          (dolist (name (ac-l-split-string (match-string-no-properties 1)))
            (unless (member name packages)
              (push name packages)))))
      (ac-l-candidates-hash label-re1 label-ht beg label-beg)
      (ac-l-candidates-hash label-re2 label-ht beg nil)
      (ac-l-candidates-hash label-re1 label-ht label-end nil)
      (ac-l-candidates-hash bibitem-re bibitem-ht beg nil)
      (when ac-l-bib-files
        (goto-char beg)
        (while (re-search-forward bib-re nil t)
          (dolist (name (ac-l-split-string (match-string-no-properties 1)))
            (unless (member name bibs)
              (push name bibs))))))
    (puthash filename
             (make-ac-l-info
              :modification (ignore-errors (nth 5 (file-attributes filename)))
              :words words
              :commands commands
              :packages packages
              :labels label-ht
              :bibitems bibitem-ht
              :bibs bibs)
             ac-l-structs)))

(defsubst ac-l-struct-master ()
  (gethash ac-l-master-file ac-l-structs))

(defsubst ac-l-all-structs ()
  (delq t (append (loop for v being the hash-values in ac-l-children collect v)
                  (list (ac-l-struct-master)))))

(defsubst ac-l-append-info (info-fn)
  (apply 'append (mapcar info-fn (ac-l-all-structs))))

(defsubst ac-l-convert-file-to-filename (file base-dir suffix)
  ;; FILE -> BASE-DIR/FILE.SUFFIX
  (let ((path (expand-file-name file base-dir)))
    (concat (if (string-match "^\\(.+\\)\\.[^./]+$" path)
                (match-string 1 path)
              path)
            "." suffix)))

(defun ac-l-update-children-names ()
  ;; parse file names in master and push them into DB
  (let* ((beg-re "^[^%\n]*\\\\\\(?:")
         (end-re "\\)[ {\t]+\\([^ }%\n]+\\)")
         (regexp (concat beg-re "in\\(?:put\\|clude\\)" end-re))
         (beg (point-min))
         (dir (if (string-match "^\\(.+/\\).+$" ac-l-master-file)
                  (match-string 1 ac-l-master-file)
                "/"))
         names)
    (save-excursion
      (goto-char beg)
      (when (re-search-forward (concat beg-re "includeonly" end-re) nil t)
        (setq names (ac-l-split-string (match-string-no-properties 1))
              regexp (concat beg-re "input" end-re)))
      (goto-char beg)
      (setq names (append names
                          (loop while (re-search-forward regexp nil t)
                                collect (match-string-no-properties 1)))))
    (ac-l-db-set 'children
                 (loop for name in names
                       for filename = (ac-l-convert-file-to-filename name dir "tex")
                       if (file-exists-p filename)
                       collect filename))))

(defun ac-l-update-children (filenames)
  (clrhash ac-l-children)
  (dolist (filename filenames)
    ;; If struct is undefined, put t.
    (puthash filename (or (gethash filename ac-l-structs) t) ac-l-children))
  ac-l-children)

(defsubst ac-l-file-mod-p (struct filename)
  (not (equal (ac-l-info-modification struct)
              (nth 5 (file-attributes filename)))))

(defsubst ac-l-update-master-info ()
  (ac-l-make-info (ac-l-struct-master) ac-l-master-file t))

(defun ac-l-update-info (&optional force)
  "If necessary, update file's info."
  (if ac-l-master-p
      (let ((master-mod-p (or force (ac-l-file-mod-p (ac-l-struct-master) ac-l-master-file)))
            (buf-list (buffer-list)))
        ;; master
        (or (loop with master = (expand-file-name ac-l-master-file)
                  for buf in buf-list
                  if (string= master (buffer-file-name buf))
                  do (when (or (buffer-modified-p buf) master-mod-p)
                       (with-current-buffer buf
                         (ac-l-update-children-names)
                         (ac-l-update-master-info)))
                  and return t)
            (when master-mod-p
              (with-temp-buffer
                (insert-file-contents ac-l-master-file)
                (ac-l-update-children-names)
                (ac-l-update-master-info))))
        ;; children
        (let* ((filenames (ac-l-db-get 'children))
               (table (ac-l-update-children filenames)))
          (dolist (buf buf-list)
            (let* ((filename (buffer-file-name buf))
                   (struct (gethash filename table)))
              (when (and struct
                         (or (not (ac-l-info-p struct))
                             (buffer-modified-p buf)
                             (ac-l-file-mod-p struct filename)))
                (with-current-buffer buf
                  (ac-l-make-info struct filename))
                (remhash filename table))))
          (maphash (lambda (filename struct)
                     (when (or (not (ac-l-info-p struct))
                               (ac-l-file-mod-p struct filename))
                       (with-temp-buffer
                         (insert-file-contents filename)
                         (ac-l-make-info struct filename))))
                   table)
          (ac-l-update-children filenames)))
    (when (or force (buffer-modified-p))
      (ac-l-update-master-info))))

(defun ac-l-update ()
  "Update `ac-sources' according to packages."
  (ac-l-db-set 'package-cmds nil)
  (ac-l-db-set 'package-args nil)
  (let ((sources (ac-l-db-get 'sources))
        cmd-sources arg-sources)
    (dolist (name (ac-l-info-packages (ac-l-struct-master)))
      ;; sources
      (dolist (source (ac-l-db-get 'package-sources))
        (let* ((alist (symbol-value source))
               (package (cdr (assq 'ac-l-package alist)))
               (prefix (cdr (assq 'prefix alist))))
          (when (string-match package name)
            (cond
             ((string= prefix ac-l-command-prefix)
              (unless (memq source cmd-sources)
                (push source cmd-sources)))
             ((eq prefix 'ac-l-argument)
              (unless (memq source arg-sources)
                (push source arg-sources)))))))
      ;; candidates
      (let* ((vec (or (gethash name ac-l-packages) '[nil nil]))
             (cmd (aref vec 0))
             (arg (aref vec 1)))
        (if cmd (ac-l-db-append 'package-cmds cmd))
        (if arg (ac-l-db-append 'package-args arg))))
    (setq ac-sources (append (ac-l-db-get 'user-prefix-sources)
                             (nth 0 sources)
                             cmd-sources
                             (nth 1 sources)
                             arg-sources
                             (nth 2 sources)
                             (ac-l-db-get 'user-noprefix-sources)
                             (nth 3 sources)))))

(defun ac-l-set-sources ()
  (let ((s0 `(ac-source-filename
              ac-l-source-labels
              ac-l-source-bibitems
              ,(if ac-l-bib-files 'ac-l-source-bibliographies)
              ,(if (ac-l-db-get 'latex-cmds) 'ac-l-source-latex-commands)))
        (s1 `(,(if ac-l-package-files 'ac-l-source-package-commands)
              ac-l-source-commands
              ac-source-files-in-current-dir
              ,(if (ac-l-db-get 'latex-args) 'ac-l-source-latex-arguments)))
        (s2 `(,(if ac-l-package-files 'ac-l-source-package-arguments)
              ,(if (ac-l-db-get 'filenames) 'ac-l-source-filenames)))
        (s3 `(,(if ac-l-use-word-completion 'ac-l-source-words)
              ac-source-dictionary)))
    (ac-l-db-set 'sources
                 (mapcar (lambda (s) (delq nil s)) (list s0 s1 s2 s3)))))

;;; candidate
;; copied from auto-complete.el and added arguments
(defun ac-l-candidate (beg-re end-re)
  (let ((i 0)
        (regexp (concat beg-re (regexp-quote ac-prefix) end-re))
        cand cands)
    (save-excursion
      ;; Search backward
      (goto-char ac-point)
      (while (and (or (not (integerp ac-limit)) (< i ac-limit))
                  (re-search-backward regexp nil t))
        (setq cand (match-string-no-properties 1))
        (unless (member cand cands)
          (push cand cands)
          (incf i)))
      ;; Search backward
      (goto-char (+ ac-point (length ac-prefix)))
      (while (and (or (not (integerp ac-limit)) (< i ac-limit))
                  (re-search-forward regexp nil t))
        (setq cand (match-string-no-properties 1))
        (unless (member cand cands)
          (push cand cands)
          (incf i))))
    (nreverse cands)))

(defun ac-l-incremental-update-index (idx cand-fn)
  (let ((pair (symbol-value idx))
        (ac-limit (or (and (integerp ac-limit) ac-limit) 10)))
    (when (null pair)
      (set idx (cons nil nil)))
    ;; Mark incomplete
    (when (car pair)
      (setcar pair nil))
    (let ((list (cdr pair))
          (words (funcall cand-fn)))
      (dolist (word words)
        (unless (member word list)
          (push word list)
          (setcdr pair list))))))

(defun ac-l-update-index (idx cand-fn)
  (dolist (buf (buffer-list))
    (when (and (eq ac-l-major-mode (buffer-local-value 'major-mode buf))
               (or ac-fuzzy-enable
                   (not (eq buf (current-buffer)))))
      (with-current-buffer buf
        (when (and (not (car (symbol-value idx)))
                   (< (buffer-size) 1048576))
          ;; Complete index
          (set idx (cons t (let ((ac-point (point-min))
                                 (ac-prefix "")
                                 ac-limit)
                             (funcall cand-fn)))))))))

(defun ac-l-candidates (idx cand-fn)
  (loop initially (unless ac-fuzzy-enable
                    (ac-l-incremental-update-index idx cand-fn))
        for buf in (buffer-list)
        if (and (or (not (integerp ac-limit)) (< (length cands) ac-limit))
                (derived-mode-p (buffer-local-value 'major-mode buf)))
        append (funcall ac-match-function ac-prefix
                        (cdr (buffer-local-value idx buf)))
        into cands
        finally return cands))


;;;; sources
(defvar ac-l-word-index nil)
(make-variable-buffer-local 'ac-l-word-index)

(defun ac-l-smart-capitalize ()
  ;; Meadow/Emacs memo: http://www.bookshelf.jp/soft/meadow_34.html#SEC495
  (when (and (looking-back "[[:space:][:cntrl:]]+[a-z']+")
             (= (point) (save-excursion
                          (backward-sentence)
                          (forward-word)
                          (point))))
    (capitalize-word -1)))

(defun ac-l-candidate-words-in-buffer ()
  (ac-l-candidate "[^\\,]\\(\\<" "[-'a-zA-Z]+\\>\\)"))

(defvar ac-l-source-words
  '((action . ac-l-smart-capitalize)
    (requires . 3)))

(defvar ac-l-command-index nil)
(make-variable-buffer-local 'ac-l-command-index)

(defun ac-l-candidate-commands-in-buffer ()
  (ac-l-candidate "\\\\\\(" "[a-zA-Z@]+\\)"))

(defvar ac-l-source-commands
  `((prefix . ,ac-l-command-prefix)))

(defun ac-l-basic-sources-setup ()
  ;; Add properties into basic sources.
  ;; The sources work like ac-source-words-in-same-mode-buffers.
  (let* ((cw-fn 'ac-l-candidate-words-in-buffer)
         (cc-fn 'ac-l-candidate-commands-in-buffer)
         (wc `(ac-l-candidates 'ac-l-word-index ',cw-fn))
         (cc `(ac-l-candidates 'ac-l-command-index ',cc-fn))
         (wi `(ac-l-update-index 'ac-l-word-index ',cw-fn))
         (ci `(ac-l-update-index 'ac-l-command-index ',cc-fn)))
    (labels ((pushprops (p1 p2 p3 p4)
                        (push `(candidates . ,p1) ac-l-source-words)
                        (push `(candidates . ,p2) ac-l-source-commands)
                        (push `(init . ,p3) ac-l-source-words)
                        (push `(init . ,p4) ac-l-source-commands)))
      (if ac-l-master-p
          ;; add functions for file's candidates
          (pushprops `(append ,wc (ac-l-db-get 'file-words))
                     `(append ,cc (ac-l-db-get 'file-cmds))
                     `(lambda ()
                        ,wi
                        (ac-l-db-set 'file-words
                                     (ac-l-append-info 'ac-l-info-words)))
                     `(lambda ()
                        ,ci
                        (ac-l-db-set 'file-cmds
                                     (ac-l-append-info 'ac-l-info-commands))))
        (pushprops wc cc wi ci)))))

(defvar ac-l-source-latex-commands
  `((candidates . (ac-l-db-get 'latex-cmds))
    (prefix . ,ac-l-command-prefix)
    (symbol . "l")))

(defvar ac-l-source-latex-arguments
  `((candidates . (ac-l-db-get 'latex-args))
    (prefix . ac-l-argument)
    (symbol . "l")))

(defvar ac-l-source-filenames
  '((candidates . (ac-l-db-get 'filenames))
    (prefix . ac-l-argument))
  "Source for package and bib file names.")

(defvar ac-l-source-package-commands
  `((candidates . (ac-l-db-get 'package-cmds))
    (prefix . ,ac-l-command-prefix)
    (symbol . "p")))

(defvar ac-l-source-package-arguments
  '((candidates . (ac-l-db-get 'package-args))
    (prefix . ac-l-argument)
    (symbol . "p")))

(defsubst ac-l-gethash (key tables)
  (loop for table in tables thereis (gethash key table)))

(defsubst ac-l-append-keys (tables)
  (apply 'append (mapcar (lambda (table)
                           (loop for k being the hash-keys in table collect k))
                         tables)))

(defun ac-l-update-labels ()
  (let ((it (ac-l-db-set 'label-tables
                         (mapcar 'ac-l-info-labels (ac-l-all-structs)))))
    (ac-l-db-set 'label-cands (ac-l-append-keys it))))

(defvar ac-l-source-labels
  '((init . ac-l-update-labels)
    (candidates . (ac-l-db-get 'label-cands))
    (prefix . ac-l-label)
    (document . (lambda (k) (ac-l-gethash k (ac-l-db-get 'label-tables))))
    (symbol . "L")))

(defun ac-l-complete-labels ()
  "Start label name completion at point."
  (interactive)
  (auto-complete (list (remove '(prefix . ac-l-label) ac-l-source-labels))))

(defun ac-l-update-bibitems ()
  (let ((it (ac-l-db-set 'bibitem-tables
                         (mapcar 'ac-l-info-bibitems (ac-l-all-structs)))))
    (ac-l-db-set 'bibitem-cands (ac-l-append-keys it))))

(defvar ac-l-source-bibitems
  '((init . ac-l-update-bibitems)
    (candidates . (ac-l-db-get 'bibitem-cands))
    (prefix . ac-l-bib)
    (document . (lambda (k) (ac-l-gethash k (ac-l-db-get 'bibitem-tables))))
    (symbol . "B")))

(defun ac-l-update-bib ()
  (let ((it (ac-l-db-set 'cur-bib-tables
                         (loop with tables = (ac-l-db-get 'all-bib-tables)
                               for name in (ac-l-append-info 'ac-l-info-bibs)
                               if (assoc-default name tables 'string=)
                               collect it))))
    (ac-l-db-set 'bib-cands (ac-l-append-keys it))))

(defvar ac-l-source-bibliographies
  '((init . ac-l-update-bib)
    (candidates . (ac-l-db-get 'bib-cands))
    (prefix . ac-l-bib)
    (document . (lambda (k) (ac-l-gethash k (ac-l-db-get 'cur-bib-tables))))
    (symbol . "B")))

(defun ac-l-complete-bibs ()
  "Start bibliography completion at point."
  (interactive)
  (auto-complete `(,(remove '(prefix . ac-l-bib) ac-l-source-bibitems)
                   ,(remove '(prefix . ac-l-bib) ac-l-source-bibliographies))))

;;; help
(defconst ac-l-help (make-hash-table :test 'equal))

(defmacro ac-l-define-help-doc (name file beg-re end-re)
  (declare (indent 1))
  `(defun ,(intern (format "ac-l-%s-documentation" name)) (str)
     (or (gethash str ac-l-help)
         (unless (string-match "@" str)
           (with-temp-buffer
             (insert-file-contents (concat ac-l-dict-directory ,file))
             (if (re-search-forward (concat ,beg-re str ,end-re) nil t)
                 (puthash str (match-string-no-properties 1) ac-l-help)
               (puthash str t ac-l-help)))))))

(ac-l-define-help-doc latex2e
  "latex-help"
  "\\(?:\f\n\\)\\([^\f]*\\(?:^[`\\]"
  "\\(?:\\s(\\|[ '\n]\\)[^\f]+\\)\\)")

(ac-l-define-help-doc yatex-jp
  "YATEXHLP.jp"
  "^\\(\\\\?"
  "\n[^]+\\)")


;;;; clear
(defvar ac-l-clear-timer nil)

(defun ac-l-clear ()
  (clrhash ac-l-help))

(defun* ac-l-cancel-timers
    (&optional (timers '(ac-l-update-timer
                         ac-l-clear-timer)))
  (interactive)
  (dolist (timer timers)
    (let ((val (symbol-value timer)))
      (when (timerp val)
        (cancel-timer val)
        (set timer nil)))))


;;;; setup
(defun ac-l-update-all (&optional force)
  (when (eq ac-l-major-mode major-mode)
    (ac-l-update-info force)
    (ac-l-update)))

(defun ac-l-master-p ()
  (setq ac-l-master-p (and (stringp ac-l-master-file)
                           (file-exists-p ac-l-master-file))))

(defmacro ac-l-set-local-variable (var val)
  (declare (indent 1))
  `(unless (local-variable-p ',var)
     (set (make-local-variable ',var) ,val)))

(defun ac-l-set-local-variables ()
  (ac-l-set-local-variable ac-prefix-definitions
    (append '((ac-l-argument . (ac-l-prefix-in-paren ac-l-argument-regexps))
              (ac-l-file . (ac-l-prefix-in-paren ac-l-file-regexps))
              (ac-l-label . (ac-l-prefix-in-paren ac-l-label-regexps))
              (ac-l-bib . (ac-l-prefix-in-paren ac-l-bib-regexps)))
            ac-prefix-definitions))
  (ac-l-set-local-variable ac-source-files-in-current-dir
    (append '((prefix . ac-l-file)
              (symbol . "F"))
            ac-source-files-in-current-dir)))

(defun* ac-l-user-sources-setup (&optional (sources ac-l-sources))
  (dolist (source sources)
    (ac-l-db-push source (if (assq 'prefix (symbol-value source))
                             'user-prefix-sources
                           'user-noprefix-sources))))

(defun ac-l-set-timers ()
  (setq ac-l-update-timer (run-with-idle-timer ac-l-update-delay t 'ac-l-update-all)
        ac-l-clear-timer (run-with-timer 600 600 'ac-l-clear)))

(defun ac-l-setup ()
  "Set up Auto Complete LaTeX."
  (let ((msg "Loading auto-complete-latex...")
        (initial-p (not (ac-l-struct-master))))
    (message "%s" msg)
    (setq ac-l-major-mode major-mode)
    (ac-l-set-local-variables)
    (when initial-p
      (ac-l-master-p)
      (ac-l-basic-sources-setup)
      (ac-l-user-sources-setup)
      (ac-l-read-packages)
      (ac-l-read-bibs)
      (ac-l-make-source-from-dir)
      (ac-l-set-help-doc)
      (ac-l-set-sources)
      (ac-l-set-timers))
    (when (or initial-p (not ac-l-master-p))
      (ac-l-update-all t))
    (message "%sdone" msg)))

(provide 'auto-complete-latex)
;;; auto-complete-latex.el ends here