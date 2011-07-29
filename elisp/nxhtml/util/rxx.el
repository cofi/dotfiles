;;; rxx.el --- Additional routines for rx regexps
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2010-06-05 Sat
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
;; Parsing of string regexps to rx style.
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
(require 'web-vcs)

(defvar my-rxx-test-details nil)

(defun my-message (format &rest args)
  (when my-rxx-test-details
    (apply 'message format args)))

;;;###autoload
(defun rxx-parse-string (string)
  "Do like `rxx-parse' but parse STRING instead of current buffer.
has the same meaning and return value has the same
format."
  (with-temp-buffer
    (insert string)
    (rxx-parse)))

;;;###autoload
(defun rxx-simplify-result (raw)
  "Simplify result if possible.
Things to take care of:

  \(and a) => a
  \(any a) => a
  \(or a) => a
  \(or a b) => a b ;; single letters
  \(rx (and a b c ...)) => top only: (rx a b c ...)
"
  (assert (eq 'rx (car raw)) t)
  (let ((result (rxx-simplify-result-1 (copy-tree raw))))
    (assert (eq 'rx (car result)) t)
    (my-message "result=%S" result)
    (when (eq 'and (car-safe (nth 1 result)))
      (setq result (cons 'rx (cdr (nth 1 result)))))
    (my-message "raw   =%S" raw)
    (my-message "result=%S" result)
    result
    ))

(defun rxx-escape-ntf (str)
 "Return string STR with \\n, \\t and \\f quoted."
 (mapconcat (lambda (cc)
              (case cc
                (?\n "\\n")
                (?\t "\\t")
                (?\f "\\f")
                (t (char-to-string cc))))
            str
            ""))
;; (rxx-simplify-result '(rx (and (and 1) (or 2) (and (or 4)))))
;; (rxx-simplify-result '(rx (and 1) (or 2) (and (or 5 6) (or (any b a)))))
;; (rxx-simplify-result '(rx "a" (and (or (\? "A") "b"))))
;; (rxx-simplify-result '(rx (submatch (and (or (submatch "ab") "d")) "e")))
(defun rxx-simplify-result-1 (raw)
  (let ((res (cdr raw)))
    (while res
      (let* ((re (car res))
             (what (car-safe re))
             (last (when (listp re) (last re))))
        (if (stringp re)
            (setcar res (let ((print-escape-newlines t))
                          (read (prin1-to-string re))))
          (cond ( (and (memq what '(and or))
                       (= 1 (length (cdr re))))
                  ;; Single element, just take that
                  (setcar res (cadr re))
                  (rxx-simplify-result-1 res)
                  )
                ( (and (memq what '(any))
                       (= 1 (length (cdr re)))
                       ;; This should be a string, check if length of it
                       ;; is one.
                       (let ((str (cadr re)))
                         ;; Could be char class, maybe check better?
                         (assert (or (stringp str) (symbolp str)) t)
                         (when (stringp str)
                           (= 1 (length str)))))
                  )
                ( (and (memq what '(and or))
                       ;;(last '(0 1 (3 4)))
                       (eq what (car-safe last)))
                  ;; Tail is possible to merge logically, to that.
                  (setcar res (cons what (append (butlast res) (cdr last))))
                  ))
          (setq re (car res)) ;; we have change res
          (when (listp re)
            (rxx-simplify-result-1 re))))
      (setq res (cdr res)))
    raw))

;;;###autoload
(defun rxx-parse ()
  "Parse current buffer regexp between point min and max.
Return a cons with car t on success and nil otherwise.  If
success the cdr is the produced form.  Otherwise it is an
informative message about what went wrong.

The produced form includes (rx ...) around it.

Fix-me: Rethink. If then Emacs read syntax for
strings is used.  This meanst that \\ must be doubled and things
like \\n are recognized."
  (when my-rxx-test-details (web-vcs-message-with-face 'highlight "regexp src=%S" (buffer-string)))

  (string-match (buffer-substring-no-properties (point-min) (point-max)) "")
  (rxx-tokenize)
  (let* (ok
         state
         (parse-res (catch 'bad-regexp
                      (prog1
                          ;;(rxx-parse-1 'and-top nil)
                          (rxx-parse-start)
                        (setq ok t))))
         (ret-rx (if (not ok)
                     parse-res ;; Error
                   (rxx-simplify-result (list 'rx parse-res))))
         (ret (if ok
                  (cons t ret-rx)
                (cons nil ret-rx))))
    (when state (error "Internal error: state rest=%S" state))
    (my-message "rxx-parse => %S" ret)
    ret))

(eval-when-compile (require 'cl))

(defvar rxx-tokens nil)

;; This helps:
;;
;; Regular Expressions
;; http://pubs.opengroup.org/onlinepubs/009695399/basedefs/xbd_chap09.html
;;
;; ERE Precedence (from high to low)
;;
;; Collation-related bracket symbols [==] [::] [..]
;; Escaped characters \<special character>
;; Bracket expression []
;; Grouping ()
;; Single-character-ERE duplication * + ? {m,n}
;; Concatenation 
;; Anchoring ^ $
;; Alternation |

(defun rxx-tokenize ()
  (setq rxx-tokens nil)
  (goto-char (point-min))
  (let (pre-tokens cc)
    (while (setq cc (char-after))
      (forward-char)
      (when (eq cc ?\\)
        (let ((c2 (char-after)))
          (when c2 (forward-char))
          (setq cc
                (cond
                 ((memq c2 (append (concat "|"  ;; Alternatives
                                           "{}" ;; Repeating
                                           "()" ;; Grouping
                                           "0123456789" ;; Backref
                                           "w"  ;; Word char
                                           "W"  ;; Non-word char
                                           "s"  ;; Syntax NEXT
                                           "S"  ;; Syntax not NEXT
                                           "c"  ;; Category NEXT
                                           "C"  ;; Category not NEXT
                                           "`'" ;; Empty buffer boundary
                                           "="  ;; Empty at point
                                           "b"  ;; Empty at word boundary
                                           "B"  ;; Empty not at word boundary
                                           "<>" ;; Empty beg/end word
                                           "_"  ;; Empty beg/end symbol (if followed by <>)
                                           )
                                           nil))
                  (list 'BS c2))
                 ((memq c2 (append "nrft" nil))
                  (case cc
                    (?n 10)
                    (?r 13)
                    (?f 12)
                    (?t 9)))
                 (t c2)))))
      (when (member cc (append "^$.*[]-\\" nil))
        (setq cc (list 'STATE cc)))
      (push cc pre-tokens))
    (setq rxx-tokens (reverse pre-tokens))
    (message "rxx-tokens=%S" rxx-tokens)
    (rxx-dump-tokens)
    ))

(defun rxx-pop-token ()
  (pop rxx-tokens))
(defun rxx-push-token (token)
  (push token rxx-tokens))
(defun rxx-next-token (&optional num)
  (setq num (or num 0))
  (nth num rxx-tokens))

(defun rxx-error()
  (message "#### rxx-error:")
  (rxx-dump-tokens)
  (let ((token (car rxx-tokens)))
    (if (listp token)
        (error "  (%S %c)" (car token) (cadr token))
      (error "  %S" token))))

(defun rxx-dump-tokens ()
  (let (out)
    (dolist (token rxx-tokens)
      (setq out
            (concat out
             (cond
              ((listp token)
               (format " %S-%c" (car token) (cadr token)))
              ((characterp token)
               (format " %c" token))
              (t
               (format " %S" token))))))
    (message "RXX TOKENS=%S" out)))



(defun rxx-parse-start ()
  (append '(and) (or (rxx-parse-group-inner) '(""))))


(defun rxx-parse-group ()
  (when (equal '(BS ?\()
               (rxx-next-token))
    (or (rxx-parse-and)
        (rxx-parse-submatch))))

(defun rxx-parse-and ()
  (when (and (equal '(BS ?\()
                    (rxx-next-token))
             (eq ?\? (rxx-next-token 1))
             ;; Fix-me: numbered. Not in rx yet.
             (eq ?\: (rxx-next-token 2)))
    (rxx-pop-token) (rxx-pop-token) (rxx-pop-token)
    (rxx-dump-tokens)
    (append '(and) (rxx-parse-group-inner))))

(defun rxx-parse-submatch ()
  (rxx-dump-tokens)
  (when (and (equal '(BS ?\()
                    (rxx-next-token))
             (not (equal '(BS ?\?)
                         (rxx-next-token 1))))
    (rxx-pop-token)
    (rxx-dump-tokens)
    (append '(submatch) (rxx-parse-group-inner))))

(defun rxx-parse-group-inner ()
  (rxx-dump-tokens)
  (let (results (more t) or-more)
    (while (and rxx-tokens more)
      (setq or-more (rxx-parse-or (car results)))
      (if (not or-more)
          (setq more (rxx-parse-single-item))
        (pop results)
        (setq more or-more))
      (when more (push more results)))
    (when (equal '(BS ?\))
                 (rxx-next-token))
      (rxx-pop-token))
    (rxx-dump-tokens)
    (reverse results)))

(defun rxx-parse-str ()
  (when (characterp (rxx-next-token))
    (rxx-dump-tokens)
    (let (result (more t))
      (while (and rxx-tokens more)
        (let ((token (rxx-pop-token)))
          (setq more (if (characterp token)
                         token
                       (rxx-push-token token)
                       nil))
          (when more (push more result))))
      (concat (reverse result)))))

(defun rxx-parse-or (left)
  (rxx-dump-tokens)
  (when (equal '(BS ?\|)
               (rxx-next-token))
    (rxx-pop-token)
    (let* ((right (rxx-parse-single-item))
           (result (when right `(,right))))
      (when left (push left result))
      (rxx-dump-tokens)
      (append '(or) result nil))))

(defun rxx-parse-brackets ()
  ;; Fix-me: implement char classes
  ;; (rx (any digit))
  ;; (rx (any digit "ac"))
  ;; (rx (any digit) (any "abc"))
  (when (equal '(STATE ?\[)
               (rxx-next-token))
    (rxx-pop-token)
    ;; Fix-me: Can't have \ before ^
    (let ((is-not (when (equal '(STATE ?^)
                               (rxx-next-token))
                    (rxx-pop-token)
                    t))
          (is-first t)
          (more t)
          result)
      (while (and rxx-tokens more)
        (let ((next-token (rxx-next-token)))
          (setq more (if (characterp next-token)
                         (rxx-pop-token)
                       (cond
                        ((eq 'STATE (car next-token))
                         (let ((cc (cadr next-token)))
                           (case cc
                             (?\] (if is-first cc
                                    (rxx-pop-token)
                                    nil))
                             (?\[ (rxx-parse-char-class))
                             ;; Fix-me:
                             (t (rxx-pop-token) cc))))
                        (t (rxx-error)))))
          (when more (push more result))
          (setq is-first nil)))
      ;; Fix-me: Clean up first, sort, remove duplicates
      (setq result (concat (append (reverse result))))
      (setq result (append '(any) `(,result)))
      (when is-not (setq result `(not ,result)))
      result)))

(defun rxx-parse-braces (single)
  "Handle repeating constructs.
Return SINGLE enclosed in them or by itself if no repeating
construct is found.

Repeating constructs are braces and ? * + *? +? ??."
  (cond
   ((eq ?? (rxx-next-token))
    (rxx-pop-token)
    (case (rxx-next-token)
      (?? (rxx-pop-token)
          `(?? ,single))
      (t `(? ,single))))
   ((eq ?* (rxx-next-token))
    (case (rxx-next-token)
      (?? (rxx-pop-token)
          `(*? ,single))
      (t `(* ,single))))
   ((eq ?+ (rxx-next-token))
    (case (rxx-next-token)
      (?? (rxx-pop-token)
          `(+? ,single))
      (t `(+ ,single))))
   ((equal '(BS ?\{)
           (rxx-next-token))
    (rxx-pop-token)
    (let ((more t)
          minr maxr comma)
      (while (and rxx-tokens more)
        (let ((next-token (rxx-next-token)))
          (setq more (cond
                      (  (memq next-token (append "0123456789" nil))
                         (if comma
                             (push (rxx-pop-token) maxr)
                           (push (rxx-pop-token) minr)))
                      (  (eq next-token ?\,)
                         (when comma
                           (throw 'bad-regexp "Double comma in braces"))
                         (rxx-pop-token)
                         (setq comma t))
                      (  (equal '(BS ?\})
                                next-token)
                         (rxx-pop-token)
                         nil)
                      (t (throw 'bad-regexp "Illegel char in braces"))))))
      (unless (or minr maxr) (throw 'bad-regexp "Number missing in braces"))
      (when minr (setq minr (string-to-number (apply 'string (reverse minr)))))
      (when maxr (setq maxr (string-to-number (apply 'string (reverse maxr)))))
      (when (and maxr (not comma)) (error "Error in parser: no comma but maxr"))
      (cond
       ((not comma)
        `( = ,minr ,single))
       ((not maxr)
        `( >= ,minr ,single))
       (t
        (setq minr (or minr 0))
        `(** ,minr ,maxr ,single)))))
   (t single)))

(defun rxx-parse-single-item ()
  (when (and rxx-tokens
             (not (member (rxx-next-token)
                          '(
                            (BS ?\))
                            ;;(BS ?\()
                            ))))
    (rxx-dump-tokens)
    (let ((single (or (rxx-parse-str)
                      (rxx-parse-single-backslash-item)
                      (rxx-parse-group)
                      (rxx-parse-brackets)
                      (rxx-error))))
      (when single
        (rxx-parse-braces single)))))

(defun rxx-parse-single-backslash-item ()
  (let* ((next-token (rxx-next-token))
         (is-bs (and (listp next-token)
                     (eq 'BS (car next-token))))
         (bs-val (nth 1 next-token)))
    (when is-bs
      (prog1
          (cond
           ((memq bs-val (append "0123456789" nil)) `(backref (string-to-number (string bs-val))))
           ((eq bs-val ?w) 'wordchar)
           ((eq bs-val ?W) 'not-wordchar)
           ((or (eq bs-val ?s)
                (eq bs-val ?S))
            (rxx-pop-token)
            (let* ((next-token (rxx-next-token))
                   syntax)
              (when (consp next-token)
                (unless (eq 'STATE (car next-token))
                  ;; Fix-me: return string instead
                  (rxx-error))
                (setq next-token (nth 1 next-token)))
              (setq syntax (case next-token
                             (?- 'whitespace)
                             (?. 'punctuation)
                             (?w 'word)
                             (?_ 'symbol)
                             (?\( 'open-parenthesis)
                             (?\) 'close-parenthesis)
                             (?' 'expression-prefix)
                             (?\" 'string-quote)
                             (?$ 'paired-delimiter)
                             (?\\ 'escape)
                             (?/ 'character-quote)
                             (?< 'comment-start)
                             (?> 'comment-end)
                             (?| 'string-delimiter)
                             (?! 'comment-delimiter)
                             (t (rxx-error))))
              (if (eq bs-val ?s)
                  `(syntax ,syntax)
                `(not (syntax ,syntax)))))
           ((or (eq bs-val ?c)
                (eq bs-val ?C))
            (rxx-pop-token)
            (let* ((next-token (rxx-next-token))
                   category)
              (when (consp next-token)
                (unless (eq 'STATE (car next-token))
                  ;; Fix-me: return string instead
                  (rxx-error))
                (setq next-token (nth 1 next-token)))
              (setq category
                      (case next-token
                        (?0 'consonant)
                        (?1 'base-vowel)
                        (?2 'upper-diacritical-mark)
                        (?3 'lower-diacritical-mark)
                        (?4 'tone-mark)
                        (?5 'symbol)
                        (?6 'digit)
                        (?7 'vowel-modifying-diacritical-mark)
                        (?8 'vowel-sign)
                        (?9 'semivowel-lower)
                        (?< 'not-at-end-of-line)
                        (?> 'not-at-beginning-of-line)
                        (?A 'alpha-numeric-two-byte)
                        (?C 'chinese-two-byte)
                        (?G 'greek-two-byte)
                        (?H 'japanese-hiragana-two-byte)
                        (?I 'indian-two-byte)
                        (?K 'japanese-katakana-two-byte)
                        (?N 'korean-hangul-two-byte)
                        (?Y 'cyrrilian-two-byte)
                        (?^ 'combining-diacritic)
                        (?a 'ascii)
                        (?b 'arabic)
                        (?c 'chinese)
                        (?e 'ethiopic)
                        (?g 'greek)
                        (?h 'korean)
                        (?i 'indian)
                        (?j 'japanese)
                        (?k 'japanese-katakana)
                        (?l 'latin)
                        (?o 'lao)
                        (?q 'tibetanian)
                        (?v 'vietnamese)
                        (?w 'hebrew)
                        (?y 'cyrrilic)
                        (?| 'can-break)
                        (t (rxx-error))))
              (if (eq bs-val ?c)
                  `(category ,category)
                `(not (category ,category)))))

           ((eq bs-val ?`) 'buffer-start)
           ((eq bs-val ?') 'buffer-end)
           ((eq bs-val ?=) 'point)
           ((eq bs-val ?b) 'word-boundary)
           ((eq bs-val ?B) 'not-word-boundary)
           ((eq bs-val ?<) 'word-start)
           ((eq bs-val ?>) 'word-end)
           ((eq bs-val ?_)
            (rxx-pop-token)
            (let* ((next-token (rxx-next-token)))
              (case next-token
                (?< 'symbol-start)
                (?> 'symbol-end)
                (t (rxx-error)))))
           (t (rxx-error)))
        (rxx-pop-token)))))

;; This became too complicated. Rewriting standard way with a tokenizer etc.
;; (defun rxx-parse-1 (what end-with)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tests

(defvar my-rxx-result nil)

(defun my-rxx-insert ()
  "testing"
  (interactive)
  (insert "(rx "
          (format "%S" my-rxx-result)
          ")"))

(defun my-rxx-parse-all ()
  "Test all rows in buffer."
  (interactive)
  (widen)
  (let ((my-rxx-test-details nil))
    (goto-char (point-min))
    (while (not (eobp))
      (my-rxx-parse)
      (forward-line 1))))

(defun my-rxx-parse ()
  "testing line."
  (interactive)
  (save-restriction
    (widen)
    (goto-char (point-at-bol))
    (let* ((ok-on-line (re-search-forward "[ \t]*OK=\\(.*\\)" (point-at-eol) t))
           (alt (when ok-on-line (match-string-no-properties 1)))
           (end (or (when ok-on-line (match-beginning 0)) (point-at-eol))))
      (narrow-to-region (point-at-bol) end)
      (let* ((src (buffer-substring-no-properties (point-max) (point-min)))
             (res-rx-rec (rxx-parse))
             (dummy (my-message "res-rx-rec=%S" res-rx-rec))
             (res-rx-ok (car res-rx-rec))
             (res-rx (when res-rx-ok (cdr res-rx-rec)))
             evaled-done
             ;; (rx-form (if (listp res-rx)
             ;;              (cons 'rx res-rx)
             ;;            (list 'rx res-rx)))
             (res-rx-to-string (condition-case err
                                   (prog1
                                       (eval res-rx)
                                     (setq evaled-done t))
                                 (error (error-message-string err))))
             (res-rx-again-rec (when res-rx-to-string
                                 (with-temp-buffer
                                   (insert res-rx-to-string)
                                   (rxx-parse))))
             (res-rx-again-ok (car res-rx-again-rec))
             (res-rx-again (when res-rx-again-ok (cdr res-rx-again-rec)))
             (same-str     (string= src res-rx-to-string))
             (same-alt-str (string= alt res-rx-to-string))
             (nearly-same-str (or same-str
                                  same-alt-str
                                  (string= (concat "\\(?:" src "\\)")
                                           res-rx-to-string)))
             (same-rx-again (or same-str (equal res-rx-again res-rx)))
             (res-rx-again-str (if (or same-rx-again (not res-rx-again))
                                   ""
                                 (concat ", again=" (prin1-to-string res-rx-again))))
             (ok-face '(:foreground "black" :background "green"))
             (maybe-face '(:foreground "black" :background "yellow"))
             (nearly-face '(:foreground "black" :background "yellow green"))
             (fail-face '(:foreground "black" :background "red"))
             (bad-regexp-face '(:foreground "black" :background "gray"))
             (res-face
              (cond (same-str ok-face)
                    (nearly-same-str nearly-face)
                    (same-rx-again  maybe-face)
                    (t fail-face))))
        (if (not res-rx-ok)
            (let* ((bad (cdr res-rx-rec))
                   (bad-msg (car bad))
                   (bad-pos (cdr bad))
                   (bad-pre  (buffer-substring-no-properties (point-min) bad-pos))
                   (bad-post (buffer-substring-no-properties bad-pos (point-max))))
              (web-vcs-message-with-face
               bad-regexp-face
               "parsed \"%s\" => %s: \"%s\" HERE \"%s\"" src bad-msg bad-pre bad-post))
          (setq my-rxx-result res-rx)
          (my-message "res-rx-to-string=%s" res-rx-to-string)
          (when same-str (setq res-rx-to-string (concat "EQUAL STR=" res-rx-to-string)))
          (when same-alt-str (setq res-rx-to-string (concat "OK STR=" res-rx-to-string)))
          (when same-rx-again (setq res-rx-again "EQUAL RX"))
          (web-vcs-message-with-face
           res-face
           "parsed \"%s\" => %S => \"%s\" => %S" src res-rx res-rx-to-string res-rx-again))))))

(global-set-key [(f9)] 'my-rxx-parse)
(global-set-key [(control f9)] 'my-rxx-parse-all)
(global-set-key [(shift f9)] 'my-rxx-insert)


(provide 'rxx)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; rxx.el ends here
