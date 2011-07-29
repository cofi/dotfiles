;;; bibhlp.el --- Routines for article references handling
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2010-12-04 Sat
;; Version: 0.2
;; Last-Updated: 2010-12-09 Thu
;; URL:
;; Keywords: bib
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
;; Help routines for handling of bibl ref, looking up doi, searching
;; library, simple parsing, some formatting, add to/lookup in your web
;; bibl ref manager.
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
(eval-when-compile (require 'http-post-simple))
(eval-when-compile (require 'mm-url))
(eval-when-compile (require 'org-find-links))
(eval-when-compile (require 'thingatpt))
(eval-when-compile (require 'web-vcs))
;;(eval-when-compile (require 'web-vcs)) ;; autoloaded

(require 'browse-url)

(defgroup bibhlp nil
  "Customization group for bibhlp."
  :group 'external
  :group 'communications)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Zotero lives currently in Firefox

(defun bibhlp-open-in-firefox (url)
  "Open URL in Firefox (for Zotero)."
  (if (eq system-type 'windows-nt)
      (w32-shell-execute nil "C:/Program Files/Mozilla Firefox/firefox.exe" url)
    (start-process "Firefox" nil "firefox.exe" url)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Problem downloading PDF

;; Fix-me: Using Opera at the moment due to Chrome and Firefox bug when displaying pdf:
(defvar bibhlp-browse-url-for-pdf-exe
  (if (eq system-type 'windows-nt)
      ;;"C:/Program Files/Opera/opera.exe"
      (let ((firefoxes '( "C:/Program Files/Mozilla Firefox/firefox.exe"
			 "C:/Program Files (x86)/Mozilla Firefox/firefox.exe")))
	(catch 'found
	  (dolist (ff firefoxes)
	    (when (file-exists-p ff)
	      (throw 'found ff)))))
    ;; Fix-me:
    "C:/Program Files/Opera/opera.exe"))

(defun bibhlp-browse-url-for-pdf (url)
  "Show URL in a web browser, capable of downloading PDF."
  (if t
      (browse-url url)
    (if (eq system-type 'windows-nt)
        (w32-shell-execute nil
                           bibhlp-browse-url-for-pdf-exe
                           url)
      (start-process "PDF-browser" nil
                     bibhlp-browse-url-for-pdf-exe
                     url))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Heuristic parsing of bibliographic entries

;; .bib = BibTex
;; .env = EndNote
;; .ris = Reference Manager

;;; RIS
;; TY  - JOUR
;; T1  - Training and plasticity of working memory
;; JO  - Trends in Cognitive Sciences
;; VL  - 14
;; IS  - 7
;; SP  - 317
;; EP  - 324
;; PY  - 2010/7//
;; T2  -
;; AU  - Klingberg, Torkel
;; SN  - 1364-6613
;; M3  - doi: DOI: 10.1016/j.tics.2010.05.002
;; M3  - 10.1001/archpsyc.62.4.417 (zotero)
;; UR  - http://www.sciencedirect.com/science/article/B6VH9-50B0K3D-1/2/962202c56e58dd3c877f2d485c1a0900
;; ER  -

(defun bibhlp-make-ris (rec)
  "Make a RIS record."
  (if (not rec)
      (message "No data to show")
    (with-current-buffer (get-buffer-create "*BIBHLP*")
      (erase-buffer)
      (fundamental-mode)
      (let ((val (plist-get rec :authors)))
        (when val
          (dolist (v val)
            (let ((ln (nth 0 v))
                  (fn (nth 1 v))
                  (in (nth 2 v)))
              (insert "AU  - " ln ", ")
              (when fn (insert fn " "))
              (when in (insert in))
              (insert "\n")))))
      (let ((val (plist-get rec :mail)))
        (when val (insert "AD  - " val "\n")))
      (let ((val (plist-get rec :year)))
        (when val (insert "PY  - " val "\n")))
      (let ((val (plist-get rec :title)))
        (when val (insert "TI  - " val "\n")))
      (let ((val (plist-get rec :journal)))
        (when val (insert "JO  - " val "\n")))
      (let ((val (plist-get rec :volume)))
        (when val (insert "VL  - " val "\n")))
      (let ((val (plist-get rec :issue)))
        (when val (insert "IS  - " val "\n")))
      (let ((val (plist-get rec :firstpage)))
        (when val (insert "SP  - " val "\n")))
      (let ((val (plist-get rec :lastpage)))
        (when val (insert "EP  - " val "\n")))
      (let ((val (plist-get rec :doi)))
        (when val (insert "M3  - doi: DOI: " val "\n")))
      ;; :pmid
      (display-buffer (current-buffer))
      )))

(defun bibhlp-parse-entry (beg mid end)
  "Try to parse a bibiographic entry between BEG and END.
These defaults to (point-min) and (point-max).
If MID is given this is the position where links begin.

Return as plist with keys
fix-me
     :authors
     :mail
     :year
     :title
     :journal
     :volume
     :issue
     :firstpage
     :lastpage
     :doi
     :pmid
     :pmcid

This is an adhoc routine to be used for convenience from
interactive functions.  Be aware that it may fail or give wrong
result.  Though I have had quite good success with it.

However what it does is first trying formats that reminds of RIS
etc.

If that fails it tries to parse it as a html code with meta tags
and other simple formats found for download on the web.

If that fails tro try to parse it like something similar to an
APA reference."
  (setq beg (or beg (point-min)))
  (setq end (or end (point-max)))
  (let ((here (point))
        (ref-end (or mid end))
        type authors raw-authors et-al mail year title
        journal volume issue firstpage lastpage doi isbn url pmid
        pmcid section keywords abstract location publisher
        return-value
        links)
    (save-restriction
      (narrow-to-region beg end)
      ;; Find out format
      (goto-char (point-min))
      (setq return-value
            (or
             (bibhlp-parse-endnote   beg end) ;; .enw, citmgr - EndNote
             (bibhlp-parse-ris-like  beg end) ;; .ris - Reference Manager, MedLine, Zotero etc
             (bibhlp-parse-from-html beg end)
             (bibhlp-parse-apa-like  beg ref-end)
             (bibhlp-parse-ama-like beg ref-end)
             (progn
               (message "%s" (propertize "bibhlp: Unrecognized reference format, can't parse it"
                                    'face 'secondary-selection))
               (throw 'top-level nil)
               nil)))
      (when mid
        (goto-char mid)
        (setq links (bibhlp-parse-link-text mid end))
        (setq return-value (append return-value links nil))
        )
      (goto-char here)
      ;; thing-at-point pattern may catch <> around mail:
      (when (and mail (eq ?< (string-to-char mail)))
        (setq mail (substring mail 1 (- (length mail) 2))))
      ;; Lastname etc
      ;; fix-me: more to do here, initials
      (setq type        (plist-get return-value :type))
      (setq raw-authors (plist-get return-value :authors))
      (setq et-al       (plist-get return-value :et-al))
      (setq mail        (plist-get return-value :mail))
      (setq keywords    (plist-get return-value :keywords))
      (setq year        (plist-get return-value :year))
      (setq publisher   (plist-get return-value :publisher))
      (setq title       (plist-get return-value :title))
      (setq location    (plist-get return-value :location))
      (setq publisher   (plist-get return-value :publisher))
      (setq journal     (plist-get return-value :journal))
      (setq volume      (plist-get return-value :volume))
      (setq issue       (plist-get return-value :issue))
      (setq firstpage   (plist-get return-value :firstpage))
      (setq lastpage    (plist-get return-value :lastpage))
      (setq doi         (plist-get return-value :doi))
      (setq isbn        (plist-get return-value :isbn))
      (setq url         (plist-get return-value :url))
      (setq pmid        (plist-get return-value :pmid))
      (setq pmcid       (plist-get return-value :pmcid))
      (if (not (stringp (car raw-authors)))
          (setq authors raw-authors)
        (dolist (val raw-authors)
          (let (last first inits auth)
            (if (not (string-match "\\(.+\\), +\\(.+\\)" val))
                ;; Medline format perhaps:
                (if (not (string-match "\\(.+?\\) +\\(.+\\)" val))
                    (setq auth (list val))
                  (setq last  (match-string-no-properties 1 val))
                  (let ((val2 (match-string-no-properties 2 val)))
                    (if (string= (upcase val2) val2)
                        (progn
                          (setq inits val2)
                          (setq auth (list last nil inits)))
                      ;; Fix-me: initials, split
                      (setq first val2)
                      (setq auth (list last first))
                      )))
              (setq last (match-string-no-properties 1 val))
              (setq first (match-string-no-properties 2 val))
              (setq auth (list last first)))
            (setq authors (cons auth authors))))
        (setq authors (reverse authors)))
      (when (and journal
                 (null type))
        (setq type 'journal-article))
      (setq url (replace-regexp-in-string " " "+" url t t))
      (or ;;return-value
       (list
        :type type
        :authors authors
        :et-al et-al
        :mail mail
        :keywords keywords
        :year year
        :location location
        :publisher publisher
        :title title
        :journal journal
        :volume volume
        :issue issue
        :firstpage firstpage
        :lastpage lastpage
        :doi doi
        :isbn isbn
        :url url
        :pmid pmid
        :pmcid pmcid)))))

(defun bibhlp-parse-ama-like (beg end)
  "Unknown. Catch formats used by NCBI etc.
This should cover AMA.

Example:

Binswanger IA, Kral AH, Bluthenthal RN, Rybold
DJ, Edlin BR. High prevalence of abscesses and
cellulitis among community-recruited injection drug
users in San Francisco. Clin Infect Dis. 2000;
30:579–91"
  (let ((auths nil)
        beg-yy end-yy yy ti jo is pf pl vo doi isbn pmid pmcid pos
        (yy-patt (rx whitespace
                     (submatch (or "19" "20")
                               (any "0-9")
                               (any "0-9"))
                     (? whitespace (or "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec") ";"))))
    (goto-char beg)
    (when (re-search-forward yy-patt end t)
      (setq yy (match-string-no-properties 1))
      (setq beg-yy (match-beginning 0))
      (setq end-yy (match-end 0))
      (goto-char beg)
      (when (search-forward "." beg-yy t)
        (let ((auth-str (buffer-substring-no-properties beg (1- (point))))
              )
          (dolist (au (split-string auth-str ",[ \n]*" t))
            (let* ((li (split-string au "[ \n]+" t))
                  (lastname (nth 0 li))
                  (initials (nth 1 li)))
              (setq auths (cons (list lastname nil initials) auths)))))
        (setq auths (reverse auths))
        (skip-chars-forward " \t\n")
        (setq pos (point))
        (when (re-search-forward "[.?!]" (1+ beg-yy) t)
          (setq ti (buffer-substring-no-properties pos (1- (point))))
          (setq ti (replace-regexp-in-string "[ \t\n]+" " " ti))
          (skip-chars-forward " \t\n")
          (setq pos (point))
          (when (search-forward "." beg-yy t)
            (setq jo (buffer-substring-no-properties pos (1- (point))))
            (setq jo (replace-regexp-in-string "[ \t\n]+" " " jo))
            (goto-char end-yy)
            (when (re-search-forward (concat "\\(?1:[0-9]+\\)"
                                             "\\(?:(\\(?2:[0-9]+\\))\\)?"
                                             ":\\(?3:[0-9]+\\)"
                                             "\\(?:[-–]\\(?4:[0-9]+\\)\\)?"
                                             )
                                     end t)
              (setq vo (match-string-no-properties 1))
              (setq is (match-string-no-properties 2))
              (setq pf (match-string-no-properties 3))
              (setq pl (match-string-no-properties 4))
              (when (re-search-forward "\bdoi:\\(.*\\)\." end t)
                (setq doi (match-string-no-properties 1)))
              (list
               :type 'journal-article ;; fix-me
               :authors auths
               :year yy
               :title ti
               :journal jo
               :volume vo
               :issue is
               :firstpage pf
               :lastpage pl
               :doi doi
               :isbn isbn
               :pmid pmid
               :pmcid pmcid
               ))))))))

(defun bibhlp-parse-apa-like (beg end)
  "Parse reference similar to APA style between BEG and END."
  (let ((re-yy (rx "("
                   (submatch (or (repeat 4 (any digit))
                                 "in press"))
                   ")"
                   (*? anything)
                   (? (any ".,:")) ;; Should be "."
                   ))
        auths et-al beg-yy end-yy yy ti doi isbn pmid pmcid
        type location publisher journal volume issue pf pe)
    ;; Find year in slightly different formats.
    (goto-char beg)
    ;;(if (re-search-forward "(\\([0-9]\\{4\\}\\).*?)[.:]?" end t)
    (if (re-search-forward re-yy end t)
        (progn
          (setq beg-yy (match-beginning 0))
          (setq end-yy (match-end 0))
          (setq yy (match-string-no-properties 1)))
      (setq beg-yy end)
      (setq end-yy end))
    (when yy
      ;; Get authors. Formats we try to cover are:
      ;;   Saylam, C., Ucerler, H., Kitis, O., Ozand, E. and Gonul, A.S. (2006)
      ;;   Cooper P, Murray L, Wilson A, Romaniuk H (2003).
      ;;   Glezer I, Simard AR, Rivest S (2007):
      ;;   Glezer, Simard, Rivest (2007):
      ;;
      ;; fix-me: rewrite author part as a state machine. The model below fails on "& karlsson, hans (2000)" etc.
      (let (;;(re-author "\\([^,]+\\),?[\w]+\\([^,\w]+\\),")
            ;;(re-author "[ \t\n]*\\([^,]*\\),[ \t\n]*\\([^,]*\\),")
            (re-author (rx (* whitespace)
                           (or (and (submatch (+? (not (any ",&"))))
                                    (+ whitespace)
                                    (? (and "et al"
                                            (? ".")
                                            (+ whitespace)))
                                    "(")
                               (and (submatch (+ (not (any ",&"))))
                                    (? (and ","
                                            (* whitespace)
                                            (submatch (+ (and (any "A-Z")
                                                              (? ".")
                                                              (* whitespace))))))
                                    (or (and (or ","
                                                 (and (+ whitespace)
                                                      "&"))
                                             (* whitespace)
                                             (? (and "&" (+ whitespace)))
                                             (* whitespace))
                                        (and
                                         (* whitespace)
                                         "("))))))
            (case-fold-search nil)
            )
        (goto-char beg)
        (goto-char beg)
        (while ( < (point) beg-yy)
          (let ((b1 (point))
                e1
                who
                lastname initials)
            (if (not (re-search-forward re-author (1+ beg-yy) t))
                (goto-char beg-yy)
              (if (match-string-no-properties 3)
                  (progn
                    (setq lastname (match-string-no-properties 2))
                    (setq initials (match-string-no-properties 3)))
                (setq lastname (or (match-string-no-properties 2)
                                   (match-string-no-properties 1)))
                (when (match-string 1)
                  (let ((ms0 (match-string-no-properties 0)))
                    (setq et-al (string-match (rx (+ whitespace)
                                                  "et al"
                                                  (? ".")
                                                  (+ whitespace)
                                                  "(")
                                              ms0)
                          )
                    (when et-al (message "ET AL FOUND"))
                    ))
                (setq initials nil))
              (setq initials (delete ?\  (delete ?. (append initials nil))))
              (setq initials (concat initials))
              (push (list lastname nil initials) auths)))))
      ;; Get title, journal, volume, issue, pages.
      ;; Formast we try to cover are:
      ;;   Controlled trial. I. Impact on maternal mood. British Journal Psychiatry 182, 412–419
      ;;   Focal gray: A follow-up study. Neuropsychopharmacology 32:2057–2066.
      ;;   Reduced hippocampal. Surg. Radiolog. Anat., 28: 82–87.
      ;;   Microglia act: a (R)-[11C]PK11195 study. Biol Psych, 64(9), 820-822.
      ;;
      ;; All possibly followed by doi:, pmcid:, pmid: etc.
      )
    ;; Sanity check
    (unless auths (setq yy nil))
    (when yy
      (goto-char end-yy)
      (skip-chars-forward " \t\n" end)
      (let ((ti-beg (point)))
        (skip-chars-forward "^.?!" end)
        (unless (>= (point) end) (forward-char 1))
        (setq ti (buffer-substring-no-properties ti-beg (point))))
      (unless (eobp) (forward-char))

      (if (re-search-forward (rx (* whitespace)
                                 (submatch (*? anything))
                                 (* whitespace)
                                 ","
                                 (* whitespace)
                                 (submatch (+ digit))
                                 )
                             end t)
          (progn
            (setq type 'journal-article)
            (setq journal (match-string-no-properties 1))
            (setq volume  (match-string-no-properties 2))
            (when (re-search-forward (rx (* whitespace)
                                         (? "(" (submatch (+ (any digit))) ")")
                                         (* whitespace)
                                         ",")
                                     end t)
              (setq issue  (match-string-no-properties 1))
              (when (re-search-forward (rx (* whitespace)
                                           (submatch (+ (any digit)))
                                           (* whitespace)
                                           (? "-" (* whitespace)
                                              (submatch (+ (any digit))))))
                (setq pf (match-string-no-properties 1))
                (setq pe (match-string-no-properties 2)))))
        (when (re-search-forward (rx (* whitespace)
                                     (submatch (* (not (any ":"))))
                                     (* whitespace)
                                     ":")
                                 end t)
          (setq location (match-string-no-properties 1))
          (when (member location '("doi" "isbn" "pmid" "pmcid"))
            (setq location nil))
          (when location
            (setq type 'book)
            (when (re-search-forward (rx (* whitespace)
                                         (submatch (* (not (any "."))))
                                         (* whitespace)
                                         ".")
                                     end t)
              (setq publisher (match-string-no-properties 1))))))
      ;; doi etc
      (goto-char beg)
      (when (re-search-forward "\\bdoi:\\([^ \t\n]*\\)" end t)
        (setq doi (match-string-no-properties 1)))
      (goto-char beg)
      (when (re-search-forward "\\bisbn:\\([^ \t\n]*\\)" end t)
        (setq isbn (match-string-no-properties 1)))
      (goto-char beg)
      (when (re-search-forward "\\bpmid:\\([^ \t\n]*\\)" end t)
        (setq pmid (match-string-no-properties 1)))
      (goto-char beg)
      (when (re-search-forward "\\bpmcid:\\([^ \t\n]*\\)" end t)
        (setq pmcid (match-string-no-properties 1)))
      (setq auths (reverse auths))
      (list
       :year yy
       :authors auths
       :et-al et-al
       :title ti
       :type type
       :location location
       :publisher publisher
       :journal journal
       :volume volume
       :issue issue
       :firstpage pf
       :lastpage pe
       :doi doi
       :isbn isbn
       :pmid pmid
       :pmcid pmcid))))

(defun bibhlp-parse-endnote (beg end)
  "Parse EndNote record in buffer between BEG and END.
BEG and END defaults to current min and max.

.enw and citmgr files have this format.

Return a plist with found info, see `bibhlp-parse-entry'."
  (setq beg (or beg (point-min)))
  (setq end (or end (point-max)))
  (let (type authors et-al year title location publisher journal volume
        issue firstpage lastpage doi pmid section url abstract
        mail keywords isbn)
    (goto-char beg)
    (when (re-search-forward "^%\\(.\\) " end t)
      (goto-char beg)
      (while (re-search-forward "^%\\(.\\) \\(.+?\\) *$" end t)
        (let ((mark (match-string-no-properties 1))
              (val  (match-string-no-properties 2)))
          (cond
           ((string= mark "0")
            (cond
             ((string= val "Journal Article") (setq type 'journal-article))
             ((string= val "Book")            (setq type 'book))
             ((string= val "Thesis")          (setq type 'thesis))
             (t (error "Unknown type: %S" val))))
           ((string= mark "T") (setq title val))
           ((string= mark "A") (setq authors (cons val authors)))
           ((string= mark "K") (setq keywords (cons val keywords)))
           ((string= mark "J") (setq journal val))
           ((string= mark "V") (setq volume val))
           ((string= mark "N") (setq issue val))
           ((string= mark "P")
            ;; Fix me: not sure of format
            (string-match "\\([0-9]+\\)\\(?:-\\([0-9]+\\)\\)?" val)
            (setq firstpage (match-string-no-properties 1 val))
            (setq lastpage (match-string-no-properties 2 val))
            )
           ((string= mark "D") (setq year val))
           ((string= mark "I") (setq publisher val))
           ((string= mark "K") (setq keywords (cons val keywords)))
           ((string= mark "X") (setq abstract val))
           ((string= mark "U") (setq url val))
           ((string= mark "R")
            (cond ((string-match "doi:.*?\\(10\..*\\)" val)
                   (setq doi (match-string-no-properties 1 val)))
                  ((string-match "^10\\..*" val)
                   (setq doi (match-string-no-properties 0 val)))
                  ;;(t (setq doi val))
                  ))
           ((string= mark "@")
            ;; ISBN/ISSN, take it only if it is ISBN
            (let ((num-digits (length (replace-regexp-in-string "-" "" val))))
              (when (or (= 13 num-digits)
                        (= 10 num-digits))
                (setq isbn val))))
           ((string= mark "+")
            (setq location val))
           )))
      (goto-char (point-min))
      (when (re-search-forward "^ \\([^/].*/.*\\)" nil t)
        (setq doi (match-string-no-properties 1)))
      (setq authors (reverse authors))
      (when (or authors title doi pmid)
        (list
         :type type
         :authors authors
         :et-al et-al
         :year year
         :title title
         :journal journal
         :volume volume
         :issue issue
         :firstpage firstpage
         :lastpage lastpage
         :doi doi
         :isbn isbn
         :pmid pmid
         :pmcid pmid
         :location location
         :publisher publisher
         :mail mail
         :url url
         :doi doi
         :keywords keywords
         :abstract abstract
         )))))

(defun bibhlp-parse-ris-like (beg end)
  "Parse reference manager record in buffer between BEG and END.
BEG and END defaults to current min and max.

See URL `http://www.refman.com/support/risformat_intro.asp'.

This also covers RefWorks, Zotero, MedLine etc.

.ris files have this format and it is used by MedLine, Zotero etc
with some variations.

Return a plist with found info, see `bibhlp-parse-entry'."
  (setq beg (or beg (point-min)))
  (setq end (or end (point-max)))
  (let (type authors et-al year title publisher journal volume
        issue firstpage lastpage doi pmid pmcid section url
        abstract mail isbn location)
    (goto-char beg)
    (when (re-search-forward "^\\(?:AU\\|A1\\|TI\\)\\(?: +-\\)? " end t)
      (goto-char beg)
      ;; RefWorks: RT Journal
      (while (re-search-forward "^\\([A-Z0-9]+\\)\\(?: *-  *\\| \\)\\(.*?\\) *$" end t)
        (let ((mark (match-string-no-properties 1))
              (val  (match-string-no-properties 2)))
          (cond
           ((string= mark "RT") (setq type 'journal-article))
           ((string= mark "TY")
            (cond
             ((string= val "JOUR") (setq type 'journal-article))
             ((string= val "ABST") (setq type 'journal-article))
             ((string= val "BOOK") (setq type 'book))
             ((string= val "CHAP") (setq type 'journal-article)) ;; fix-me
             ((string= val "THES") (setq type 'thesis))
             (t (error "Unknown type: %S" val))))
           ((member mark '("T1" "TI"))
            ;; Title may span several lines, but probably just 2.
            (setq title val)
            (let ((after-title (point)))
              (forward-line 1)
              (back-to-indentation)
              (if (= 0 (current-column))
                  (goto-char after-title)
                (setq title (concat title " "
                                    (buffer-substring-no-properties (point) (point-at-eol))))
                (goto-char (point-at-eol)))))
           ((string= mark "JO") (setq journal val))
           ((string= mark "JF") (setq journal val)) ;; Zotero
           ((string= mark "JT") (unless journal (setq journal val))) ;; Seems to be a long variant sometimes
           ((string= mark "JA") (setq journal val))
           ((string= mark "TA") (setq journal val)) ;; Medline
           ((string= mark "VL") (setq volume val))
           ((string= mark "VI") (setq volume val))
           ((string= mark "VO") (setq volume val))
           ((string= mark "IS") (setq issue val))
           ((string= mark "IP")
            ;; Sometimes it is something like "4 Pt 1" whatever that means.
            (string-match "^\\([0-9]+\\)" val)
            (setq issue (match-string-no-properties 1 val)))
           ((string= mark "SP") (setq firstpage val))
           ((string= mark "EP") (setq lastpage val))
           ((string= mark "PG")
            (if (string-match "\\([0-9]+\\)-\\([0-9]+\\)" val)
                (progn
                  (setq firstpage (match-string-no-properties 1 val))
                  (setq lastpage
                        (number-to-string (+
                                           ;;(string-to-number firstpage)
                                           (string-to-number (match-string-no-properties 2 val))))))
              (when (string-match "\\([0-9]+\\)" val)
                (setq firstpage (match-string-no-properties 1 val)))))
           ((string= mark "PY") (setq year val))
           ((string= mark "DP") (setq year val))
           ((string= mark "DEP") (setq year val))
           ((string= mark "Y1") (setq year val)) ;; zotero
           ((string= mark "AU") (setq authors (cons val authors)))
           ((string= mark "A1") (setq authors (cons val authors))) ;; zotero
           ((string= mark "PB") (setq publisher val))
           ((string= mark "SN")
            ;; ISBN/ISSN, take it only if it is ISBN
            (let ((num-digits (length (replace-regexp-in-string "-" "" val))))
              (when (or (= 13 num-digits)
                        (= 10 num-digits))
                (setq isbn val))))
           ((string= mark "DO") (setq doi val))
           ((string= mark "AID")
            (cond ((string-match " ?\\([^ ]+\\) +\\[doi] *$" val)
                   (setq doi (match-string-no-properties 1 val)))))
           ((string= mark "UR") ;; Some journals
            (cond ((string-match "http://dx.doi.org/\\(10\..*\\)" val)
                   (setq doi (match-string-no-properties 1 val)))
                  (t (setq url val))
                  ))
           ((string= mark "M3")
            ;; M3  - doi: DOI: 10.1016/j.tics.2010.05.002
            (cond ((string-match "doi:.*?\\(10\..*\\)" val)
                   (setq doi (match-string-no-properties 1 val)))
                  (t (setq doi val))))
           ((string= mark "AB") (setq abstract val))

           ;; Used by pubmed at least:
           ((string= mark "AID")
            (when (string-match "^10\..* [doi]" val)
              (setq doi (match-string-no-properties 1 val))))
           ((string= mark "PMID") (setq pmid val))
           ((string= mark "AD")
            (require 'thingatpt)
            (cond ((string-match thing-at-point-email-regexp val)
                   (setq mail (match-string-no-properties 1 val)))
                  (t (setq location val))
                  ))

           ;; There are a lot additions in for example pubmed so
           ;; just continue if we do not want it.
           (t nil))))
      (setq authors (reverse authors))
      (when year
        (setq year (substring year 0 4)))
      ;;(unless publisher (setq publisher address))
      (when (or authors title doi pmid)
        (list
         :type type
         :authors authors
         :et-al et-al
         :year year
         :title title
         :journal journal
         :volume volume
         :issue issue
         :firstpage firstpage
         :lastpage lastpage
         :doi doi
         :pmid pmid
         :pmcid pmcid
         :publisher publisher
         :mail mail
         :url url
         :doi doi
         :isbn isbn
         :location location
         )))))

(defun bibhlp-parse-from-html (beg end)
  "Parse html in buffer between BEG and END.
They default to current min and max.

Return a plist with found info, see `bibhlp-parse-entry'."
  (setq beg (or beg (point-min)))
  (setq end (or end (point-max)))
  (let (authors et-al year title journal volume issue firstpage lastpage doi pmid section)
    (unless (and authors title journal volume issue firstpage lastpage doi pmid section)
      (goto-char beg)
      (when (looking-at "[ \t\n]*<")
        ;; RDFa
        ;; Mediterranean Ceramics: RDFa Document Metadata: Authors in PLOS One
        ;; http://mediterraneanceramics.blogspot.com/2010/05/rdfa-document-metadata-authors-in-plos.html
        (let ((no-authors (unless authors t))
              ;; Fix-me: This pattern can't handle nested tags to get
              ;; the value. That case needs some kind of a parser. It
              ;; can probably be handled by splitting the regexp and
              ;; the search for start/end tags.
              (rdfa-re (concat "<[^<>]* property *= *\"\\(?1:[^\"]*\\)\"[^<>]*?"
                               "\\(?: content *= *\"\\(?2:[^\"]*\\)\""
                               "\\|"
                               ">\\(?3:[^<]*\\)<\\)"))
              )
          (while (re-search-forward rdfa-re end t)
            (let ((pro (match-string-no-properties 1))
                  (val (or (match-string-no-properties 2)
                           (match-string-no-properties 3)))
                  (dummy (match-string-no-properties 0)))
              (cond
               ;; fix-me: mostly guesses below:
               ((when no-authors (string= pro "foaf:name"))
                (let* ((mclist (split-string val " *" t))
                       (first  (nth 0 mclist))
                       (last   (nth 1 mclist)))
                  (setq authors (cons (list first last) authors))))
               ((string= pro "dc:date") (setq year (substring-no-properties val 0 4)))
               ((string= pro "dc:title") (setq title val))
               ((string= pro "dc:source")
                ;; (string-match "\\(.*\\) \\([0-9]\\{4\\}\\) \\([0-9]+\\):\\([0-9]+\\)$" "where 2010 5:87")
                (when (string-match "\\(.*\\) \\([0-9]\\{4\\}\\) \\([0-9]+\\):\\([0-9]+\\)$" val)
                  (setq journal   (match-string-no-properties 1 val))
                  (setq year      (match-string-no-properties 2 val))
                  (setq volume    (match-string-no-properties 3 val))
                  (setq firstpage (match-string-no-properties 4 val))))
               ((string= pro "dc:identifier") (setq doi val))
               )))))
      (unless (and authors title journal volume issue firstpage lastpage doi pmid section)
        (goto-char beg)
        (let ((no-authors (unless authors t)))
          (while (re-search-forward
                  ;; "<meta +name *= *\"\\(dc\.[^\"]*\\)\" +content *= *\"\\([^\"]*\\)\""
                  "<meta .*?>"
                  end t)
            (let ((str (match-string-no-properties 0)))
              (save-match-data
                ;; "<meta +name *= *\"\\(dc\.[^\"]*\\)\" +content *= *\"\\([^\"]*\\)\""
                (let ((mn
                       (when (string-match " +name *= *\"\\(dc\.[^\"]*\\)\"" str)
                         (match-string-no-properties 1)))
                      (mc (when (string-match " +content *= *\"\\([^\"]*\\)\"" str)
                            (match-string-no-properties 1))))
                  (when (and mn mc)
                    (cond
                     ((when no-authors (string= mn "dc.creator"))
                      (let* ((mclist (split-string mc " *" t))
                             (first  (nth 0 mclist))
                             (last   (nth 1 mclist)))
                        (setq authors (cons (list first last) authors))))
                     ((string= mn "dc.date") (setq year (substring-no-properties mc 0 4)))
                     ((string= mn "dc.title") (setq title mc))
                     ((string= mn "dc.source")
                      ;; (string-match "\\(.*\\) \\([0-9]\\{4\\}\\) \\([0-9]+\\):\\([0-9]+\\)$" "where 2010 5:87")
                      (when (string-match "\\(.*\\) \\([0-9]\\{4\\}\\) \\([0-9]+\\):\\([0-9]+\\)$" mc)
                        (setq journal   (match-string-no-properties 1 mc))
                        (setq year      (match-string-no-properties 2 mc))
                        (setq volume    (match-string-no-properties 3 mc))
                        (setq firstpage (match-string-no-properties 4 mc))))
                     ((string= mn "dc.identifier") (setq doi mc))
                     ))))))
          (when no-authors (setq authors (reverse authors)))))
      (unless (and authors title journal volume issue firstpage lastpage doi pmid section)
        (goto-char beg)
        (while (re-search-forward "<meta +name *= *\"\\([^\"]*\\)\" +content *= *\"\\([^\"]*\\)\"" end t)
          (let ((mn (match-string-no-properties 1))
                (mc (match-string-no-properties 2)))
            (cond
             ((string= mn "citation_authors")
              (let ((mclist (split-string mc ", *" t)))
                (setq authors (mapcar (lambda (a)
                                        (split-string a " +" t))
                                      mclist))))
             ((string= mn "citation_year") (setq year mc))
             ((string= mn "citation_title") (setq title mc))
             ((string= mn "citation_journal_title") (setq journal mc))
             ((string= mn "citation_volume") (setq volume mc))
             ((string= mn "citation_issue") (setq issue mc))
             ((string= mn "citation_firstpage") (setq firstpage mc))
             ((string= mn "citation_lastpage") (setq lastpage mc))
             ((string= mn "citation_doi") (setq doi mc))
             ))))
      (unless (and authors title journal volume issue firstpage lastpage doi pmid section)
        (goto-char beg)
        (setq authors nil) ;; fix-me
        (when (search-forward "<rdf:RDF " end t)
          (let ((beg-rdf (point))
                (end-rdf (search-forward "</rdf:RDF>"))
                (no-authors (unless authors t))
                )
            (goto-char beg-rdf)
            (while (re-search-forward "<\\([^/>]+\\)>\\([^<]+\\)<" end-rdf t)
              (let ((rf (match-string-no-properties 1))
                    (rv (match-string-no-properties 2)))
                ;;(message "rf=%S, rv=%S" rf rv)
                (cond
                 ((string= rf "dc:title") (setq title rv))
                 ((when no-authors (string= rf "dc:creator"))
                  (let* ((names (split-string rv ", *"))
                         (firstname (nth 1 names))
                         (lastname  (nth 0 names)))
                    (setq authors (cons (list firstname lastname) authors))))
                 ((string= rf "dc:identifier")
                  (cond
                   ((string= "info:doi/" (substring-no-properties rv 0 9))
                    (setq doi (substring-no-properties rv 9)))
                   ((string= "info:pmid/" (substring-no-properties rv 0 10))
                    (setq pmid (substring-no-properties rv 10)))
                   (t (error "Unknown dc:identifier=%S" rv))))
                 ((string= rf "dc:date") (setq year (substring-no-properties rv 0 4)))
                 ((string= rf "prism:publicationName") (setq journal rv))
                 ((string= rf "prism:publicationDate") (setq year (substring-no-properties rv 0 4)))
                 ((string= rf "prism:volume") (setq volume rv))
                 ((string= rf "prism:number") (setq issue rv)) ;; fix-me: Is this correct?
                 ((string= rf "prism:startingPage") (setq firstpage rv))
                 ((string= rf "prism:endingPage") (setq lastpage rv)) ;; Fix-me: Is this correct?
                 )))
            (when no-authors (setq authors (reverse authors)))
            )))
      (setq authors (reverse authors))
      (when (or authors title doi pmid)
        (list
         :authors authors
         :et-al et-al
         :year year
         :title title
         :journal journal
         :volume volume
         :issue issue
         :firstpage firstpage
         :lastpage lastpage
         :doi doi
         :pmid pmid)
        ))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Call the web for help


(defun bibhlp-doi-to-url (doi)
  (when (string-match "^doi:" doi)
    (setq doi (substring doi 4)))
  (concat "http://dx.doi.org/" doi))

;; (bibhlp-get-page "http://www.ncbi.nlm.nih.gov/pubmed/17501969")
;; (bibhlp-get-page "doi:10.1186/1744-859X-8-2")
(defun bibhlp-get-page (url)
  (when (string= (substring url 0 4)
                 "doi:")
    (setq url (concat "http://dx.doi.org/" url)))
  (let* ((buf-res (web-vcs-url-retrieve-synch url))
         (buf (car buf-res))
         (res (cdr buf-res)))
    (unless buf
      (error "status=%S" res))
    ;;(switch-to-buffer-other-window buf)
    buf))

;; (bibhlp-get-data-from-url "http://www.ncbi.nlm.nih.gov/pubmed/17501969")
;; (bibhlp-get-data-from-url "doi:10.1186/1744-859X-8-2")
(defun bibhlp-get-data-from-url (url)
  (when (string-match "^doi:" url)
    (setq url (bibhlp-doi-to-url url)))
  (setq url (bibhlp-resolve-url url)) ;; fix-me: should this be done or not?
  (let ((buf (bibhlp-get-page url)))
    (with-current-buffer buf
      ;;(message "buf=\n\n\n%s\n\n" (buffer-string))
      (bibhlp-parse-from-html nil nil))))

;;; CrossRef

(defvar bibhlp-crossref-post-url "http://www.crossref.org/SimpleTextQuery/")
(defvar bibhlp-crossref-form-url "http://www.crossref.org/SimpleTextQuery")
(defcustom bibhlp-crossref-user-email
  ;;nil
  "lennart.borgman@gmail.com"
  "Mail address to use for access of CrossRef.org simple query form.
This is used by `bibhlp-get-ids-from-crossref'."
  :type 'string
  :group 'bibhlp)

(defcustom bibhlp-my-ip nil
  "Your pc:s ip as seen from outside.
Used by `bibhlp-get-ids-from-crossref'.

Get it from for example URL `http://www.showmyip.com/'.

If you do not set this then it will be fetched each time needed
\(i.e. once per session) by `bibhlp-get-my-ip'."
  :type 'string
  :group 'bibhlp)

;; (bibhlp-get-my-ip)
(defun bibhlp-get-my-ip ()
  (or
   ;;; Needs js:
   ;; (bibhelp-get-ip-from-showmyip.com "http://www.ip-address.com/"
   ;; 				     "<h2>My IP address is: \\([0-9]+\.[0-9]+\.[0-9]+\.[0-9]+\\)")
   (bibhelp-get-ip-from-showmyip.com "http://ip.iptelnow.com/"
    				     "\\([0-9]+\.[0-9]+\.[0-9]+\.[0-9]+\\)")
   (bibhelp-get-ip-from-showmyip.com "http://showmyip.com/"
				     "displaycopy('\\([0-9]+\.[0-9]+\.[0-9]+\.[0-9]+\\)')")
   (error "Could not find external IP address, please set `bibhlp-my-ip'")))

(defun bibhelp-get-ip-from-showmyip.com (site search)
  (declare (special url-http-response-status))
  (message "Getting your external IP address from %s ..." site)
  (let ((outbuf
	 (condition-case err
	     (url-retrieve-synchronously site)
	   (error (message "%s" (error-message-string err))
		  nil))))
    (when outbuf
      (with-current-buffer outbuf
	(let ((status url-http-response-status))
	  (if (/= 200 status)
	      (progn
		(message "Error trying to get IP: status=%S" status)
		nil)
	    (goto-char (point-min))
	    (if (re-search-forward search nil t)
		(match-string 1)
	      (message "Error: Could not find IP-address in output from %s" site)
	      (message "%s" (buffer-substring-no-properties (point-min) (point-max)))
	      nil)))))))

(defvar bibhlp-ip2 "172.20.1.78"
  "Used by bibhlp-get-ids-from-crossref.
No idea what this is for address, DomainTools says it is private??")

;; (setq y (bibhlp-get-ids-from-crossref crossref-1))
;; (setq y (bibhlp-get-ids-from-crossref crossref-2))
(defun bibhlp-get-ids-from-crossref (apa-ref)
  "Return an assoc list with DOI, PMID and PMCID for APA-REF.
REF should be an item in a reference lists for something that
might have an DOI id.

This routine calls crossref.org \(URL `http://crossref.org') to
get those identifiers.  You must get an account there \(free for
non-commercial use) to be able to use it.  Enter the mail address
you use there in `bibhlp-crossref-user-email'.

You may also need set `bibhlp-my-ip' correctly for this to work,
but it should be fetched automatically.

Note: crossref.org is currently mainly for looking up DOI, but
will give you PMID and PMCID too if they are available."
  ;; Fix-me: ":" and bottom border, just do as in other cases
  (declare (special url-http-response-status))
  ;; (when (string-match "\\b[a-zA-Z]+:" apa-ref)
  ;;   (setq apa-ref (substring apa-ref 0 (1- (match-beginning 0)))))
  ;;(message "apa-ref=%S" apa-ref)
  (setq bibhlp-my-ip (or bibhlp-my-ip (bibhlp-get-my-ip)))
  (unless bibhlp-my-ip
    (error "Please set `bibhlp-my-ip' first"))
  (let (status key1 key doi pmid pmcid other-result)
    (message "Getting CrossRef.org fake input form...")
    (with-current-buffer
        (url-retrieve-synchronously bibhlp-crossref-post-url)
      (setq status url-http-response-status)
      (unless (= 200 status) (error "Error: status=%S" status))
      (goto-char (point-min))
      (unless (re-search-forward "\.key\.value = \"\\([^\"]*\\)\"" nil t)
        (message "\n\nCrossref error form was:\n%s" (buffer-substring-no-properties (point-min) (point-max)))
        (error "Can't find crossref first key, see *Messages*"))
      (setq key1 (match-string-no-properties 1)))
    (message "Getting CrossRef.org real input form...")
    (let* ((time (concat (format-time-string "%Y%m%d!%s") (format "%0d" (/ (nth 2 (current-time)) 1000))))
           (ip1 bibhlp-my-ip)
           (ip2 bibhlp-ip2)
           (cookie (catch 'session-id
                     (let ((cs (url-cookie-retrieve "www.crossref.org" "/SimpleTextQuery")))
                       (dolist (c cs)
                         (when (string= (aref c 1) "JSESSIONID")
                           (throw 'session-id (aref c 2)))))))
           (ret (http-post-simple bibhlp-crossref-post-url
                                  (list (cons 'key (concat
                                                    ;;cookie "_" ip2 "_" ip1 "_"
                                                    key1
                                                    time)))))
           (page (nth 0 ret))
           (res  (nth 2 ret))
           )
      (unless (= 200 res)
        (error "Crossref returned %S" res))
      (if (not (string-match "<input [^>]*id=\"key\" [^>]*value=\"\\([^\"]*\\)\"" page))
          (progn
            (message "\n\nCrossref second form was:\n%s" page)
            (error "Can't find crossref auth key, see *Messages*"))
        (setq key (match-string-no-properties 1 page)))
      ;; (message "\n\n\nCalling for translation, key=%S\n\n\n" key)
      ;; (message "Form page:\n%s\n\n\n" page)
      )
    (message "crossref.apa-ref=%S" apa-ref)
    (message "Getting CrossRef.org DOI output ...")
    (let* ((ret (http-post-simple bibhlp-crossref-post-url
                                  (list (cons 'command "Submit")
                                        (cons 'doiField "")
                                        (cons 'email bibhlp-crossref-user-email)
                                        (cons 'emailField "")
                                        (cons 'freetext apa-ref)
                                        (cons 'key key)
                                        (cons 'password "")
                                        (cons 'username "")
                                        )
                                  nil
                                  ))
           (page (nth 0 ret))
           (res  (nth 2 ret))
           rec)
      ;;(message "\n \n\n\n\nPage=\n%s" page)
      (if (= 200 res)
          (progn
            ;;(when (string-match "href=\"http://dx.doi.org/\\([^\"]*\\)\"" page)
            (when (string-match ">doi:\\([^<]*\\)<" page)
              ;;(message "\n\n\nPage=\n%s\n\n\n" page)
              (setq doi (match-string-no-properties 1 page)))
            (when (string-match "\\bPMid:\\([0-9]+\\)" page)
              (setq pmid (match-string-no-properties 1 page)))
            (when (string-match "\\bPMCid:\\([0-9]+\\)" page)
              (setq pmcid (match-string-no-properties 1 page)))
            (unless (or doi pmid pmcid)
              ;;<td class=\\\"resultA\\\" colspan=\\\"4\\\" height=\\\"100%\\\" width=\\\"100%\\\" valign=\\\"top\\\">Crossref Query servlet failed to respond to your query
              ;; </td>
              (if (string-match "<td class=\"result[AB]\"[^>]*>\\(\\(?:.\\|\n\\)+?\\)</td>" page)
                  (setq other-result (list 'org-mode
                                           (concat (propertize "Crossref returned no ids, just this:\n\n" 'font-lock-face 'font-lock-warning-face)
                                                   (match-string-no-properties 1 page))))
                (if (string-match "\\(eXtyles parser failed\\)" page)
                    (setq other-result (list 'org-mode
                                             (concat (propertize "Crossref failed to parse the input:\n\n" 'font-lock-face 'font-lock-warning-face)
                                                     (match-string-no-properties 1 page))))
                  (setq other-result (list 'html-mode
                                           (concat "<!-- Status 200 but Crossref final output was: -->\n\n" page)))
                  )))
            )
        (setq other-result
              (list 'html-mode
                    (format "<!-- Crossref return status=%S and final output was: -->\n\n%s" status page)))))
    (let (ret)
      (when pmcid
        (push (cons 'pmcid pmcid) ret))
      (when pmid
        (push (cons 'pmid pmid) ret))
      (when doi
        (push (cons 'doi doi) ret))
      (when other-result
        (push (cons 'other-result other-result) ret))
      ret)))


;;; Resolving urls

;; &an=00019616-200903000-00013
;; (bibhlp-get-ids-from-crossref "http://dx.doi.org/10.1097/TEN.0b013e318198b9b8") ;; err
;; (bibhlp-goto-citeulike "http://dx.doi.org/10.1097/TEN.0b013e318198b9b8") ;; ok
;; (http-head-simple-internal "http://dx.doi.org/10.1097/TEN.0b013e318198b9b8") ;; ok
;; (bibhlp-resolve-url "http://dx.doi.org/10.1097/TEN.0b013e318198b9b8") ;; err
;; (browse-url "http://dx.doi.org/10.1097/TEN.0b013e318198b9b8") ;; ok - but not to actual page
;; (bibhlp-resolve-url "http://www.sunet.se/")
;; (bibhlp-resolve-url "http://www.google.com/")
;; (bibhlp-resolve-url "http://www.google.se/")
;; (bibhlp-resolve-url "http://dx.doi.org/10.1016/j.biopsych.2008.04.025")
;; (browse-url "http://dx.doi.org/10.1016/j.biopsych.2008.04.025")
(defun http-head-simple-internal (url)
  "Make a http head call for URL.
Return a list with status and returned url."
  (let ((url-request-method        "HEAD")
	(url-request-data          nil)
	(url-request-extra-headers nil)
        ;;(url-mime-charset-string   (http-post-charset-name charset))
        )
    (let (header
	  data
	  status
          current-url)
      (with-current-buffer (url-retrieve-synchronously url)
        ;; fix-me: I have no idea of what this `declare' does... -
        ;; there is no doc string for it. It is of course a cl
        ;; thing...
        (declare (special url-http-response-status))
        ;;(setq xbuf (current-buffer))
        ;;(setq xbuf-name (buffer-name))
        (setq current-url (url-recreate-url url-current-object))
	;; status
	(setq status url-http-response-status)
	;; ;; return the header and the data separately
	;; (goto-char (point-min))
	;; (if (search-forward-regexp "^$" nil t)
	;;     (setq header (buffer-substring (point-min) (point))
	;; 	  data   (buffer-substring (1+ (point)) (point-max)))
        ;;   ;; unexpected situation, return the whole buffer
        ;;   (setq data (buffer-string)))
        )
      (message "A:status=%S, current-url=%S" status current-url)
      (unless (memq status '(200))
        (setq url-request-method "GET")
        (with-current-buffer (url-retrieve-synchronously url)
          (declare (special url-http-response-status))
          (setq current-url (url-recreate-url url-current-object))
          (setq status url-http-response-status))
        (message "B:status=%S, current-url=%S" status current-url))
      (values status current-url))))

(defun bibhlp-resolve-url (url)
  "Return url after redirections from URL.
Does not take care of browser reload functions."
  (let ((new-url url)
        old-url
        res
        status
        done)
    (while (not (equal new-url old-url))
      (setq old-url new-url)
      (setq res (http-head-simple-internal old-url))
      (message "res=%S" res)
      (setq status (nth 0 res))
      (unless (memq status '(200 302))
        (error "Returned status=%S" status))
      (setq new-url (nth 1 res)))
    new-url
    ))


;;; ParsCit
;; fix-me: stopped working

(defvar bibhlp-parscit-post-url "http://aye.comp.nus.edu.sg/parsCit/parsCit.cgi")

(defvar bibhlp-parscit-bib-re (rx buffer-start
                                  (*? anything)
                                  (submatch
                                   "&lt;algorithm"
                                   (*? anything)
                                   "/algorithm&gt;"
                                   )
                                  (* anything)
                                  buffer-end)
  "Get the <algorithm> rec from the output.
This is html encoded, but looks like this after decoding:

<algorithm name=\"ParsCit\" version=\"10111111111111111111111101\">
  <citationList>
    <citation>
      <authors>
        <author>G P Amminger</author>
        <author>M R Schafer</author>
        <author>K Papageorgiou</author>
        <author>C M Klier</author>
        <author>S M Cotton</author>
        <author>S M Harrigan</author>
        <author>A Mackinnon</author>
      </authors>
      <volume>67</volume>
      <date>2010</date>
      <title>Long-Chain {omega}-3 Fatty Acids: A Randomized, Placebo-Controlled Trial</title>
      <journal>Arch Gen Psychiatry</journal>
      <pages>146--154</pages>
    </citation>
  </citationList>
</algorithm>")

;; (setq x (parscit-post-reference parscit-post-1))
(defun parscit-post-reference (ref)
  (message "\n\n\nparsit.ref=%S\n\n\n" ref)
  (let* ((ret (http-post-simple-multipart bibhlp-parscit-post-url
                                          (list (cons 'demo "3")
                                                (cons 'textlines ref)
                                                ;;(cons 'end3 "on")
                                                (cons 'ris3 "on")
                                                (cons 'SUBMIT "Parse these lines!")
                                                )
                                          nil
                                          ))
         (page (nth 0 ret))
         (res  (nth 2 ret))
         str-rec
         authors et-al title date journal volume issue pages)
    (if (/= 200 res)
        (message "Could not get rec")
      (setq str-rec
            (mm-url-decode-entities-string
             (replace-regexp-in-string bibhlp-parscit-bib-re "\\1" page)))
      (message "\n\n\nstr-rec:\n%s\n\n\n" str-rec)
      (setq str-rec (split-string str-rec "[\n]" t))
      (dolist (str str-rec)
        (when (string-match "<author>\\([^<]*\\)<" str)  (setq authors (cons (match-string-no-properties 1 str) authors)))
        (when (string-match "<title>\\([^<]*\\)<" str)   (setq title (match-string-no-properties 1 str)))
        (when (string-match "<date>\\([^<]*\\)<" str)    (setq date (match-string-no-properties 1 str)))
        (when (string-match "<journal>\\([^<]*\\)<" str) (setq journal (match-string-no-properties 1 str)))
        (when (string-match "<volume>\\([^<]*\\)<" str)  (setq volume (match-string-no-properties 1 str)))
        (when (string-match "<issue>\\([^<]*\\)<" str)   (setq issue (match-string-no-properties 1 str)))
        (when (string-match "<number>\\([^<]*\\)<" str)   (setq issue (match-string-no-properties 1 str)))
        (when (string-match "<pages>\\([^<]*\\)<" str)   (setq pages (match-string-no-properties 1 str)))
        ))
    ;; Fix-me: try to normalize authors, making it three components: last, first, initials.
    (let ((norm-authors nil))
      (dolist (author authors)
        ;; (dolist (author
        ;;          '(
        ;;            "cullberg, johan"
        ;;            "cullberg, j. n."
        ;;            "cullberg, jon n."
        ;;            "cullberg allan"
        ;;            ))
        (let (last first inits temp)
          (cond
           ((string-match "^\\([^\w]+?\\),? +\\([^\w]+?\\)\\(?:[.]? +\\(.*\\)\\)?$" author)
            (setq last  (match-string-no-properties 1 author))
            (setq first (match-string-no-properties 2 author))
            (setq inits (match-string-no-properties 3 author))
            (when inits
              (setq inits (replace-regexp-in-string "[.\w]" "" inits)))
            (setq last  (capitalize last))
            (setq first (capitalize first))
            (message "res=%S" (list last first inits))
            (setq norm-authors (cons (list last first inits) norm-authors))
            )
           (t (error "Could not match author=%S" author)))))
      (setq authors norm-authors))

    ;;(setq authors (reverse authors))
    (list
     :authors  authors
     :et-al et-al
     :title  title
     :date  date
     :journal  journal
     :volume  volume
     :issue  issue
     :pages  pages
     )))

;;; PMID, PMC, Pubmed, PubmedCentral

(defun bibhlp-make-pubmed-search-string (rec)
  "Make a search string for PubMed from REC."
  (let ((txt nil))
    (dolist (auth (plist-get rec :authors))
      (let ((lastname (car auth)))
        (when txt (setq txt (concat txt " AND ")))
        (when (string-match-p " " lastname)
          (setq lastname (concat "\"" lastname "\"")))
        (setq txt (concat txt lastname "[Author]"))))
    (let ((ti (plist-get rec :title)))
      (when ti
        (dolist (tw (split-string ti "[][ \f\t\n\r\v!.:,()-]" t))
          (when (< 7 (length tw))
            (when txt (setq txt (concat txt " AND ")))
            (setq txt (concat txt tw "[Title]"))))))
    (kill-new txt)
    txt))

(defun bibhlp-search-in-pubmed (rec)
  "Go to PubMed and look for REC.
REC should be a bibliographic record in the format returned from
`bibhlp-parse-entry'."
  (let ((txt (bibhlp-make-pubmed-search-string rec)))
    (message "pubmed search: %S" txt)
    (let ((url (concat
                "http://www.ncbi.nlm.nih.gov/pubmed?term="
                ;; http://www.ncbi.nlm.nih.gov/pubmed?term=Biological[title]%20AND%20clinical[title]%20AND%20ethical[title]%20AND%20advances[title]
                (browse-url-encode-url txt))))
      (bibhlp-browse-url-for-pdf url))))

;; (bibhlp-pmid2pmcid "19877500" "pubmed")
;; (bibhlp-pmid2pmcid "2804881" "pmc")
(defun bibhlp-pmid2pmcid (id from)
  "Convert ID format from FROM to alternate format.
Pubmed and Pubmed Central use different id:s.  This routine
converts between them by calling the conversion routines at NCBI,
see URL `http://www.ncbi.nlm.nih.gov/'.

FROM should be either \"pubmed\" or \"pmc\"."
  (interactive (let* ((lnk (org-link-at-point))
                      (typ (nth 0 lnk))
                      (val (nth 1 lnk))
                      (frm (cond
                            ((string= typ "pmid") "pubmed")
                            ((string= typ "pmcid") "pmc"))))
                 (when frm (list val frm))))
  (let ((known-from '("pubmed" "pmc")))
    (unless (member from known-from) (error "Value from=%S must be in %S" from known-from)))
  (unless (stringp id) (error "Parameter id=%S should be a string" id))
  (let* ((post-res (http-post-simple "http://www.ncbi.nlm.nih.gov/sites/pmctopmid"
                                     (list (cons 'PMCRALayout.PMCIDS.PMCIDS_Portlet.Db from)
                                           (cons 'PMCRALayout.PMCIDS.PMCIDS_Portlet.Ids id)
                                           (cons 'p$a "PMCRALayout.PMCIDS.PMCIDS_Portlet.Convert")
                                           (cons 'p$l "PMCRALayout")
                                           (cons 'p$st "pmctopmid")
                                           )))
         (page    (nth 0 post-res))
         (status  (nth 2 post-res))
         decoded-page)
    (unless (= 200 status) "Error: status=%S" status)
    (setq decoded-page
          (mm-url-decode-entities-string
           (replace-regexp-in-string bibhlp-parscit-bib-re "\\1" page)))
    (message "\n\n\npage:\n%s\n\n\n" decoded-page)
    (when (cond
           ((string= from "pubmed") (string-match ">PMC\\([0-9]+\\)</td>" decoded-page))
           ((string= from "pmc")    (string-match ">\\([0-9]+\\)</td>" decoded-page))
           (t (error "Bad from=%S" from)))
      (let ((result (match-string-no-properties 1 decoded-page)))
        (when (and result (called-interactively-p 'interactive))
          (let ((rtyp (cond
                       ((string= from "pubmed") "pmcid")
                       ((string= from "pmc") "pmid")
                       (t (error "Uh?")))))
            (goto-char (point-at-eol))
            (insert "\n" rtyp ":" result)))
        result))))


;; Fix-me: maybe finish these converters? The info in pubmed seems incomplete so I do not know if it is worth it.
(defun bibhlp-pmid2doi (pmid)
  (let ((url (concat "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?"
                     "db=PubMed"
                     ""
                     ))
        )
    ))

;; (bibhlp-doi2pmid "19042775")
(defun bibhlp-doi2pmid (doi)
  ;; http://lists.ccs.neu.edu/pipermail/bionlp/2010-May/001950.html
  (let* ((url (concat "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?term="
                     doi
                     ;; "&email=maximilianh@gmail.com"
                     ))
         (buf (generate-new-buffer "bibhlp-doi2pmid"))
         )
    (with-current-buffer buf
      (url-insert-file-contents url))
    (display-buffer buf)
    ))

;;; LibHub at lu.se
;; Useful of something does not work with Google Scholar

(defun bibhlp-make-libhub-search-string (rec)
  "Make a search string for LibHub from REC."
  (let ((txt nil))
    (dolist (auth (plist-get rec :authors))
      (let ((lastname (car auth)))
        (when txt (setq txt (concat txt " AND ")))
        (when (string-match-p " " lastname)
          (setq lastname (concat "\"" lastname "\"")))
        (setq txt (concat txt "au:" lastname))))
    (let ((ti (plist-get rec :title)))
      (when ti
        (dolist (tw (split-string ti "[][ \f\t\n\r\v!.:,()-]" t))
          (when (< 7 (length tw))
            (when txt (setq txt (concat txt " AND ")))
            (setq txt (concat txt "ti:" tw))))))
    (kill-new txt)
    txt))

(defcustom bibhlp-libhub-search-url
  "http://libhub.sempertool.dk.ludwig.lub.lu.se/libhub?func=search&libhubSearch=1&query="
  "Base url for searching LibHub.
Used by `bibhlp-search-in-libhub'.  The query in LibHub
format is added at the end of this, url-encoded.

The default value is for Lund University."
  :type 'string
  :group 'bibhlp)

(defun bibhlp-search-in-libhub (rec)
  "Go to LibHub and look for REC.
REC should be a bibliographic record in the format returned from
`bibhlp-parse-entry'.

You must customize `bibhlp-libhub-search-url' to use this
\(unless you are at Lund University)."
  (let ((txt (bibhlp-make-libhub-search-string rec)))
    (message "LibHub search: %S" txt)
    (let ((url (concat
                ;; "http://elin.lub.lu.se.ludwig.lub.lu.se/elin?func=advancedSearch&lang=se&query="
                ;; "http://libhub.sempertool.dk.ludwig.lub.lu.se/libhub?func=search&libhubSearch=1&query="
                bibhlp-libhub-search-url
                (browse-url-encode-url txt))))
      (bibhlp-browse-url-for-pdf url))))


;;; Google Scholar

;; http://scholar.google.se/scholar?as_q=mindfulness+training&num=10&as_epq=&as_oq=&as_eq=&as_occt=any&as_sauthors=Chiesa+Calati+Serretti&as_publication=&as_ylo=&as_yhi=&as_sdt=1&as_subj=bio&as_subj=med&as_subj=soc&as_sdtf=&as_sdts=5&btnG=Search+Scholar&hl=en

;; ?as_q=mindfulness+training
;; &num=10
;; &as_epq=
;; &as_oq=
;; &as_eq=
;; &as_occt=any
;; &as_sauthors=Chiesa+Calati+Serretti
;; &as_publication=
;; &as_ylo=
;; &as_yhi=
;; &as_sdt=1
;; &as_subj=bio
;; &as_subj=med
;; &as_subj=soc
;; &as_sdtf=
;; &as_sdts=5
;; &btnG=Search+Scholar
;; &hl=en
(defcustom bibhlp-google-scholar-extra "&as_subj=bio&as_subj=med&as_subj=soc"
  "Additions to search string.
Default is to include only articles etc covering bio, med and
soc. \(This reflects my personal choice ... ;-) - just set it to
\"\" if it disturbs.)"
  :type 'string
  :group 'bibhlp)

(defvar bibhlp-google-scholar-pre "http://scholar.google.se/scholar?as_q=")
(defun bibhlp-make-google-scholar-search-string (rec)
  "Make a search string for Google Scholar from REC."
  (let ((txt nil)
        (add-len (+ (length bibhlp-google-scholar-pre)
                    (length bibhlp-google-scholar-extra))))
    (let ((ti (plist-get rec :title)))
      (when ti
        ;; (dolist (tw (split-string ti "[][ \f\t\n\r\v!.:,()-]" t))
        ;;   (when (< 7 (length tw))
        ;;     (when txt (setq txt (concat txt " ")))
        ;;     (setq txt (concat txt tw))))
        (when txt (setq txt (concat txt " ")))
        (setq txt (concat txt "\"" ti "\""))
        ))
    (dolist (auth (plist-get rec :authors))
      (let ((lastname (car auth))
            (add ""))
        (when txt (setq add " AND "))
        (when (string-match-p " " lastname)
          (setq lastname (concat "\"" lastname "\"")))
        (setq add (concat add "author:" lastname))
        ;; There seem to be some kind of cut of a bit above 290 at
        ;; least for Opera.
        (when (> 290 (+ add-len (length txt) (length add)))
          (setq txt (concat txt add)))
        ))
    ;; Characters like åäö needs to be changed to some ascii char,
    ;; otherwise google (at least in Opera) will see them as blanks.
    ;; Google will then handle it correctly.
    (when (fboundp 'ourcomments-tr)
      (setq txt (ourcomments-tr txt "éüåäö" "euaao")))
    (kill-new txt)
    txt))

(defun bibhlp-search-in-google-scholar (rec)
  "Go to Google Scholar and look for REC.
REC should be a bibliographic record in the format returned from
`bibhlp-parse-entry'."
  (let ((txt (bibhlp-make-google-scholar-search-string rec)))
    (message "G Scholar search: %S" txt)
    (let ((url (concat
                bibhlp-google-scholar-pre
                (browse-url-encode-url (concat txt
                                               bibhlp-google-scholar-extra)))))
      (message "G url(%d)=%S" (length url) url)
      (bibhlp-browse-url-for-pdf url))))

(defun bibhlp-parse-link-text (beg end)
  ;; Fix-me
  (let ((here (point))
        (re-link (rx whitespace
                     (submatch
                      (or "doi:" "pmid:" "pmcid:")
                      (+ (not (any whitespace))))))
        links doi pmid pmcid)
    (goto-char beg)
    (while (re-search-forward re-link end t)
      (push (match-string-no-properties 1) links))
    (goto-char here)
    (dolist (lnk links)
      (cond
       ((string-match "^doi:" lnk) (setq doi lnk))
       ((string-match "^pmid:" lnk) (setq pmid lnk))
       ((string-match "^pmcid:" lnk) (setq pmcid lnk))
       ))
    (list :doi doi
          :pmid pmid
          :pmcid pmcid)))

(defun bibhlp-make-link-text (rec)
  ;; Fix-me
  )

(defun bibhlp-num-authors (rec)
  "Return number of authors.
In case :et-al is set then return -1."
  (let ((authors (plist-get rec :authors))
        (et-al   (plist-get rec :et-al)))
    (if et-al
        (- (length authors))
      (length authors))))

(defun bibhlp-make-ama (rec no-empty links)
  "Make an AMA style ref from REC."
  ;; http://www.ncbi.nlm.nih.gov/books/NBK7265/
  ;; http://www.wsulibs.wsu.edu/electric/quickguides/docs/nlm.html
  (let ((str nil)
        (type (plist-get rec :type)))
    (let ((authors (plist-get rec :authors))
          (et-al   (plist-get rec :et-al)))
      (unless et-al
        (when (< 6 (length authors))
          (setq authors (copy-sequence authors))
          (setcdr (nthcdr 4 authors) nil)
          (setq et-al t)))
      (setq str (concat str
                        ;; Fix-me:
                        (let ((auth-strs (mapcar (lambda (a)
                                                   (let* ((l (nth 0 a))
                                                          (f (nth 1 a))
                                                          (i (nth 2 a))
                                                          (inits (split-string (concat
                                                                                (when f (substring f 0 1))
                                                                                i)
                                                                               "" t)))
                                                     (concat l
                                                             (when inits " ")
                                                             (mapconcat 'identity inits "")
                                                             )))
                                                 authors)))
                          (if (= 1 (length auth-strs))
                              (concat
                               (car auth-strs)
                               (when et-al "et al."))
                            (if et-al
                                (concat (mapconcat 'identity auth-strs ", ")
                                        " et al.")
                              (concat (mapconcat 'identity (butlast auth-strs) ", ")
                                      (when (cdr authors) (concat " & "
                                                                  (car (last auth-strs)))))))))))
    (let ((ti   (plist-get rec :title)))
      (when (or ti (not no-empty))
        (setq str (concat str " " (or ti "NO-TI")))
        (unless (memq (aref str (1- (length str)))
                      (append ".!?" nil))
          (setq str (concat str ".")))
        ))
    (when (eq type 'journal-article)
      (let ((jo (plist-get rec :journal)))
        (when (or jo (not no-empty))
          (setq str (concat str
                            ;;" /"
                            " "
                            (or jo "NO-JO")
                            ;;"/"
                            "."))))
      (setq str (concat str
                        " " (or (plist-get rec :year) "n.d.") ";"))
      (let ((vl (plist-get rec :volume)))
        (when (or vl (not no-empty))
          (setq str (concat str (or vl "NO-VL")))
          (let ((is (plist-get rec :issue)))
            (when (or is (not no-empty))
              (setq str (concat str "(" (or is "NO-IS") ")"))))
          (setq str (concat str ":")))))
    (unless (eq type 'book)
      (let ((pf (plist-get rec :firstpage)))
        (when (or pf (not no-empty))
          (setq str (concat str (or pf "NO-PF")))
          (let ((pe (plist-get rec :lastpage)))
            (when pe
              (setq str (concat str "-" pe)))))
        (setq str (concat str "."))))
    str)
  )

(defun bibhlp-make-apa (rec no-empty links)
  "Make an APA style ref from REC."
  (let ((str nil)
        (type (plist-get rec :type))
        (no-author nil))
    (let ((authors (plist-get rec :authors))
          (et-al   (plist-get rec :et-al)))
      (unless et-al
        (when (< 6 (length authors))
          (setq authors (copy-sequence authors))
          (setcdr (nthcdr 4 authors) nil)
          (setq et-al t)))
      (setq str (concat str
                        ;; Fix-me:
                        (let ((auth-strs (mapcar (lambda (a)
                                                   (let* ((l (nth 0 a))
                                                          (f (nth 1 a))
                                                          (i (nth 2 a))
                                                          (inits (split-string (concat
                                                                                (when f (substring f 0 1))
                                                                                i)
                                                                               "" t)))
                                                     (concat l
                                                             (when inits ", ")
                                                             (mapconcat 'identity inits ".")
                                                             (when inits (concat ".")))))
                                                 authors)))
                          (if (not auth-strs)
                              (let ((ti   (or (plist-get rec :title)
                                              "NO TITLE")))
                                (setq no-author t)
                                ti)
                            (if (= 1 (length auth-strs))
                                (concat
                                 (car auth-strs)
                                 (when et-al "et al."))
                              (if et-al
                                  (concat (mapconcat 'identity auth-strs ", ")
                                          " et al.")
                                (concat (mapconcat 'identity (butlast auth-strs) ", ")
                                        (when (cdr authors) (concat " & "
                                                                    (car (last auth-strs))))))))))))
    (setq str (concat str
                      " (" (or (plist-get rec :year) "n.d.") ")."))
    (unless no-author
      (let ((ti   (plist-get rec :title)))
        (when (or ti (not no-empty))
          (setq str (concat str " " (or ti "NO-TI")))
          (unless (memq (aref str (1- (length str)))
                        (append ".!?" nil))
            (setq str (concat str "."))))))
    (when (eq type 'journal-article)
      (let ((jo (plist-get rec :journal)))
        (when (or jo (not no-empty))
          (setq str (concat str
                            ;;" /"
                            " "
                            (or jo "NO-JO")
                            ;;"/"
                            ","))))
      (let ((vl (plist-get rec :volume)))
        (when (or vl (not no-empty))
          (setq str (concat str " " (or vl "NO-VL")))
          (let ((is (plist-get rec :issue)))
            (when (or is (not no-empty))
              (setq str (concat str "(" (or is "NO-IS") ")"))))
          )))
    (unless (eq type 'book)
      (let ((pf (plist-get rec :firstpage)))
        (when (or pf (not no-empty))
          (setq str (concat str ","))
          (setq str (concat str " " (or pf "NO-PF")))
          (let ((pe (plist-get rec :lastpage)))
            (when (and pe (< 0 (length pe)))
              (setq str (concat str "-" pe)))))
        (setq str (concat str "."))))
    (when links
      ;; fix-me: use function above
      (when (memq type '(book thesis))
        (let ((location  (or (plist-get rec :location)  "NO-LOCATION"))
              (publisher (or (plist-get rec :publisher) "NO-PUBLISHER"))
              (isbn (or (plist-get rec :isbn)
                        (plist-get rec :isbn13)
                        (plist-get rec :isbn10))))
          (setq str (concat str " " location ":" publisher "."))
          (when isbn
            (setq str (concat str "\nisbn:" isbn)))))
      (let ((doi (plist-get rec :doi)))
        (when doi
          (setq str (concat str "\ndoi:" doi))))
      ;; (let ((isbn (plist-get rec :isbn)))
      ;;   (when isbn
      ;;     (setq str (concat str "\nisbn:" isbn))))
      (let ((pmid (plist-get rec :pmid)))
        (when pmid
          (setq str (concat str "\npmid:" pmid))))
      (let ((pmcid (plist-get rec :pmcid)))
        (when pmcid
          (setq str (concat str "\npmcid:" pmcid))))
      (let ((url (plist-get rec :url)))
        (when url
          (setq str (concat str "\n" url))))
      )
    str))

(defvar bibhlp-marking-ovl nil)
(make-variable-buffer-local 'bibhlp-marking-ovl)

;; fix-me: return 3 values instead
;; fix-me: maybe take care of blank lines in some formats.
(defun bibhlp-find-reftext-at (point)
  "Found boundary for possible bibl ref at POINT.
If INCLUDE-DOI-ETC then include those \(they are supposed to be at the end).

Return a list \(BEG MID END)."
  (if (and buffer-file-name
           (member (file-name-extension buffer-file-name)
                   '("env" "ris")))
      (list (point-min) nil (point-max))
    (let* ((here (point))
           (end (progn
                  (forward-paragraph)
                  (skip-chars-backward " \t\n\f")
                  (point)))
           (beg (progn
                  (backward-paragraph)
                  (skip-chars-forward " \t\n\f")
                  (point)))
           mid)
      (goto-char end)
      (skip-chars-forward " \t\n\f")
      (while (looking-at "^[A-Z0-9]\\{2\\} +-")
        (forward-paragraph)
        (skip-chars-backward " \t\n\f")
        (setq end (point)))
      (goto-char beg)
      (when (re-search-forward (rx whitespace
                                   (or "[["
                                       "http://" "https://" "ftp:" "mailto:"
                                       "doi:" "pmid:" "pmcid:"))
                               end t)
        (goto-char (match-beginning 0))
        (setq mid (point)))
      (goto-char here)
      (list beg mid end))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Commands

(defun bibhlp-prompt ()
  (let ((cwcfg (current-window-configuration))
        (buf (get-buffer-create "*BIBHLP Promp*"))
        ev
        to-msg
        done)
    (unwind-protect
        (progn
          (display-buffer buf)
          (with-current-buffer buf
            (insert "hej")
            )
          (while (not done)
            (setq ev (read-event))
            (message "ev=%S" ev)
            (cond
             ((eq ev ?g)
              (message "rec g")
              (setq done t))
             (t
              (if (eq (lookup-key global-map (vector ev)) 'self-insert-command)
                  (message "There is no alternative '%c'" ev)
                (setq unread-command-events (list ev))
                (setq done t)
                )
              )
             )
            ))
      (set-window-configuration cwcfg)
      (kill-buffer buf))))

(defun bibhlp-alternatives-for-url (url)
  (let ((prompt (concat "
What do you want to do with the url at point?

Show:   p - Open in a browser cabable of pdf download
        f - Show in Firefox (so you can add it to Zotero)
Find:   e - Find in org-mode buffers
        E - Find in org-mode files
Copy:   u - Copy URL to clipboard
        t - Copy title to clipboard
More:   m - More alternatives
"
                        ))
        done cc)
    (while (not done)
      (setq cc (read-char-exclusive prompt))
      (setq done t)
      (cond
       ((eq cc ?b)
        (browse-url url))
       ((eq cc ?e)
        (orgfl-find-links-in-org-buffers url))
       ((eq cc ?E)
        (orgfl-find-links-in-org-files url nil nil))
       ((eq cc ?c)
        (bibhlp-goto-citeulike url))
       ((eq cc ?d)
        (let ((rec (bibhlp-get-data-from-url url)))
          (bibhlp-make-ris rec)))
       ((eq cc ?f)
        (bibhlp-open-in-firefox url))
       ((eq cc ?p)
        (bibhlp-browse-url-for-pdf url))
       ((eq cc ?m)
        (bibhlp-alternatives-for-entry))
       ((eq cc ?u)
        (org-copy-url-at-point))
       ((eq cc ?t)
        (bibhlp-copy-link-title-at-point))
       ((eq cc ?q) nil)
       (t (setq done nil))))
    ))

(defun bibhlp-alternatives-for-entry ()
  (catch 'top-level
    (let (beg mid end)
      (if mark-active
          (progn
            (setq beg (region-beginning))
            (setq end (region-end)))
        (let ((bme (bibhlp-find-reftext-at (point))))
          (setq beg (nth 0 bme))
          (setq mid (nth 1 bme))
          (setq end (nth 2 bme)))
        (if bibhlp-marking-ovl
            (move-overlay bibhlp-marking-ovl beg end)
          (setq bibhlp-marking-ovl (make-overlay beg end))
          (overlay-put bibhlp-marking-ovl 'face 'secondary-selection)))
      (let ((prompt (concat "
What do you want to do with the marked bibliographic entry?

Search:   g - Google Scholar (which you can connect to your library)
          x - Get ids from CrossRef
Convert:  a - APA style
          m - AMA style
          r - Reference Manager style
"
                            ;; l - LibHub
                            ;; p - PubMed
                            ;; x - CrossRef
                            ;; c - ParsCit
                            ))
            done cc
            (str (buffer-substring-no-properties beg end)))
        (unwind-protect
            (while (not done)
              (setq cc (read-char-exclusive prompt))
              (setq done t)
              (cond
               ((eq cc ?q) nil)
               ((eq cc ?r)
                (bibhlp-make-ris (bibhlp-parse-entry beg mid end)))
               (nil ;;(eq cc ?c)
                (bibhlp-make-ris (parscit-post-reference str)))
               ((eq cc ?x)
                (let* ((rec (catch 'top-level
                              (bibhlp-parse-entry beg mid end)))
                       (apa-ref (when rec (bibhlp-make-apa rec t nil)))
                       (ret (let ((ref (or apa-ref
                                           (if (y-or-n-p "Could not parse entry, send all of it?")
                                               (buffer-substring-no-properties beg (or mid end))
                                             (throw 'top-level nil)))))
                              (bibhlp-get-ids-from-crossref ref)))
                       (other-result (assoc 'other-result ret))
                       (in-file-buffer buffer-file-name))
                  (if other-result
                      (with-current-buffer (get-buffer-create "*BIBHLP*")
                        (erase-buffer)
                        (if other-result
                            (let* ((mode   (nth 1 other-result))
                                   (msg    (nth 2 other-result)))
                              (unless (derived-mode-p mode) (funcall mode))
                              (insert msg))
                          (unless (derived-mode-p 'org-mode) (org-mode))
                          (dolist (r ret)
                            (let ((k (car r))
                                  (v (cdr r)))
                              (insert (format "%s:%s\n" k v)))))
                        (if in-file-buffer
                            (switch-to-buffer-other-window (current-buffer))
                          (switch-to-buffer (current-buffer)))
                        (message "Answer from CrossRef shown in *BIBHLP*"))
                    (goto-char (or mid end))
                    (insert "\n")
                    (dolist (r ret)
                      (let ((k (car r))
                            (v (cdr r)))
                        (insert (format "%s:%s\n" k v))))
                    (when mid (insert "--- old values:\n"))
                    (message "Inserted answer from CrossRef")
                    )))
               ((eq cc ?l)
                ;; Useful if problems with Google Scholar
                (let ((rec (bibhlp-parse-entry beg mid end)))
                  (bibhlp-search-in-libhub rec)))
               ((eq cc ?g)
                (let ((rec (bibhlp-parse-entry beg mid end)))
                  (bibhlp-search-in-google-scholar rec)))
               ((eq cc ?p)
                (let ((rec (bibhlp-parse-entry beg mid end)))
                  (bibhlp-search-in-pubmed rec)))

               ((eq cc ?a)
                (let* ((rec (bibhlp-parse-entry beg mid end))
                       (str (bibhlp-make-apa rec t t))
                       (in-file-buffer buffer-file-name))
                  (with-current-buffer (get-buffer-create "*BIBHLP*")
                    (erase-buffer)
                    (unless (derived-mode-p 'org-mode) (org-mode))
                    (insert str)
                    (if in-file-buffer
                        (switch-to-buffer-other-window (current-buffer))
                      (switch-to-buffer (current-buffer))
                      ))))
               ((eq cc ?m)
                (let* ((rec (bibhlp-parse-entry beg mid end))
                       (str (bibhlp-make-ama rec t t))
                       (in-file-buffer buffer-file-name))
                  (with-current-buffer (get-buffer-create "*BIBHLP*")
                    (erase-buffer)
                    (unless (derived-mode-p 'org-mode) (org-mode))
                    (insert str)
                    (if in-file-buffer
                        (switch-to-buffer-other-window (current-buffer))
                      (switch-to-buffer (current-buffer))))))
               (t (setq done nil))))
          (when (and bibhlp-marking-ovl
                     (overlay-buffer bibhlp-marking-ovl))
            (delete-overlay bibhlp-marking-ovl)))))))

;;;###autoload
(defun bibhlp ()
  "Big Question for handling of bibliographic related things.
Will give you a choice list with what you can do with the
bibliographic reference or the url at point.

For a recognized bibliographic reference at point you can:
  - look it up in Google Scholar (which you can link to your
    university library)
  - get DOI, PMID and PMCID from CrossRef
  - convert it to a different format (only APA and Ref Man)

  The currently recognized reference formats are End Note
  \(.enw), Reference Manager \(.ris), APA style and the style
  AMA etc use.

For an URL at point you can:
  - show it in a specific browser (f ex Firefox/Zotero)
  - search for it in org mode buffers and files


Note: `idxsearch', indexed search, may be a good tool to use
together with this one.  It allows you to use some common pc
index search engines from within Emacs.  It should make it easy
to handle both hits in .org files and .pdf files."
  ;; - There is also a chance that you can get bliograchic data on
  ;;   the page url, but this does not work well.
  (interactive)
  (catch 'top-level
    (let ((url (or (when (derived-mode-p 'org-mode)
                     (org-url-at-point))
                   (progn
                     (require 'url-util)
                     (url-get-url-at-point)))))
      (when url
        (unless (string-match (rx string-start (or "http" "https") ":") url)
          (setq url nil)))
      (if url
          (bibhlp-alternatives-for-url url)
        (bibhlp-alternatives-for-entry)))))

;; (defun bibhlp-goto-article ()
;;   (interactive)
;;   )

;; (bibhlp-goto-citeulike "http://www.ncbi.nlm.nih.gov/pmc/articles/PMC2804881/")
;; (bibhlp-goto-citeulike (concat "http://dx.doi.org/" "doi:10.1001/archgenpsychiatry.2009.192"))

;;;###autoload
(defun bibhlp-goto-citeulike (article-url)
  "Open CiteULike in a web browser and open the article from ARTICLE-URL.
If this article have not been added to CiteULike then you can add
it when the browser opens CitULike.  Othwise the already added
article will be shown.

See URL `http://www.citeulike.org/' for info about CiteULike.

Note: CiteULike is a privately owned site sponsored by Springer
and with close source.  However you can have your data private
and it looks like data can be shared/exported to Zotero later."
  (interactive (list (or (org-url-at-point)
                         (read-string "Article URL: "))))
  (let* ((citeurl (concat "http://www.citeulike.org/posturl?username=beogl&bml=nopopup&url="
                          article-url)))
    (browse-url citeurl)))

;; ;;;###autoload
;; (defun bibhlp-search-ref-at-point-in-libhub ()
;;   "Try to find bibliographic at point in LibHub.
;; LibHub is a library gateway used by some universities to let
;; students and staff access scientific journals etc."
;;   (interactive)
;;   (catch 'top-level
;;     (let ((rec (bibhlp-parse-entry nil nil nil)))
;;       (bibhlp-search-in-libhub rec))))

;;http://www.scopus.com.ludwig.lub.lu.se/results/results.url?sort=plf-f&src=s&st1=10.1016%2fj.biopsych.2007.08.018&sid=8pTEVcKWegfJuOaPSjIfB9Q%3a310&sot=b&sdt=b&sl=35&s=DOI%2810.1016%2fj.biopsych.2007.08.018%29&origin=searchbasic&txGid=8pTEVcKWegfJuOaPSjIfB9Q%3a31

;; http://www.scopus.com.ludwig.lub.lu.se/results/results.url?sort=plf-f&src=s&st1=10.1038%2fnn.2572&sid=Kue6iaMx3Jw-njyFdUKgSTE%3a230&sot=b&sdt=b&sl=20&s=DOI%2810.1038%2fnn.2572%29&origin=searchbasic&txGid=Kue6iaMx3Jw-njyFdUKgSTE%3a23
;; http://www.scopus.com.ludwig.lub.lu.se/results/results.url?sort=plf-f&src=s&st1=10.1038%2fnn.2572&sid=Kue6iaMx3Jw-njyFdUKgSTE%3a230&sot=b&sdt=b&sl=20&s=DOI%2810.1038%2fnn.2572%29&origin=searchbasic
;; http://www.scopus.com.ludwig.lub.lu.se/results/results.url?sort=plf-f&src=s&st1=10.1038%2fnn.2572&sot=b&sdt=b&sl=20&s=DOI%2810.1038%2fnn.2572%29&origin=searchbasic

;;http://www.scopus.com.ludwig.lub.lu.se/results/results.url?sort=plf-f&src=s&st1=10.1016%2fj.biopsych.2007.08.018&sid=8pTEVcKWegfJuOaPSjIfB9Q%3a310&sot=b&sdt=b&sl=35&s=DOI%2810.1016%2fj.biopsych.2007.08.018%29


;; http://www.scopus.com.ludwig.lub.lu.se/results/results.url?sort=plf-f&src=s&sot=b&sdt=b&sl=35&s=DOI%2810.1016%2fj.biopsych.2007.08.018%29

;;http://www.scopus.com.ludwig.lub.lu.se/results/results.url?sort=plf-f&src=s&st1=10.1001%2farchgenpsychiatry.2010.199&sid=b_mAZjtRTCLqrGj5kHJD8G5%3a80&sot=b&sdt=b&sl=39&s=DOI%2810.1001%2farchgenpsychiatry.2010.199%29&origin=searchbasic&txGid=b_mAZjtRTCLqrGj5kHJD8G5%3a8

;; http://www.scopus.com.ludwig.lub.lu.se/results/results.url?sort=plf-f&src=s&st1=10.1001%2farchgenpsychiatry.2010.199&sot=b&sdt=b&sl=39&s=DOI%2810.1001%2farchgenpsychiatry.2010.199%29&origin=searchbasic
(defcustom bibhlp-scopus-url
  "http://www.scopus.com.ludwig.lub.lu.se/results/results.url"
  "Base URL to got to your Scopus."
  :type 'string
  :group 'bibhlp)

;;;###autoload
(defun bibhlp-copy-link-title-at-point ()
  "Copy `org-mode' link at point title to clipboard."
  (let* ((lnk (org-link-at-point))
         (title (nth 2 lnk)))
    (if (not lnk)
        (message "No org-mode linke here")
      (kill-new title)
      (message "Copied link title to clipboard"))))

;;;###autoload
(defun bibhlp-scopus-by-doi (doi)
  (interactive (let* ((lnk (org-link-at-point))
                      (typ (nth 0 lnk))
                      (val (nth 1 lnk)))
                 (list (when (string= typ "doi") val))))
  (message "doi=%s" doi)
  (when doi
    (let ((enc-doi (browse-url-encode-url doi)))
      (browse-url (concat bibhlp-scopus-url
                          ;; http://www.scopus.com.ludwig.lub.lu.se/results/results.url?sort=plf-f&src=s&st1=10.1038%2fnn.2572&sot=b&sdt=b&sl=20&s=DOI%2810.1038%2fnn.2572%29&origin=searchbasic
                          "?sort=plf-f&src=s&st1="
                          ;; 10.1038%2fnn.2572
                          enc-doi
                          "&sot=b&sdt=b&sl=20&s=DOI%28"
                          ;;10.1038%2fnn.2572
                          enc-doi
                          "%29&origin=searchbasic"
                          )))))

(provide 'bibhlp)
;; Local variables:
;; coding: utf-8
;; End:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; bibhlp.el ends here
