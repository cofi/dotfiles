;;; auto-dictionary.el --- automatic dictionary switcher for flyspell
;;-*-coding: utf-8;-*-
;;
;; Copyright (C) 2006-2008 Nikolaj Schumacher
;;
;; Author: Nikolaj Schumacher <bugs * nschum de>
;; Version: 1.0.1
;; Keywords: wp
;; URL: http://nschum.de/src/emacs/auto-dictionary/
;; Compatibility: GNU Emacs 21.x, GNU Emacs 22.x
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Add the following to your .emacs file:
;; (require 'auto-dictionary)
;; (add-hook 'flyspell-mode-hook '(lambda () (auto-dictionary-mode 1)))
;;
;; Then just type.  When you stop for a few moments `auto-dictionary-mode' will
;; start evaluating the content.
;;
;; You can also force a check using `adict-guess-dictionary', whether or not
;; `auto-dictionary-mode' is enabled.
;;
;; If you're unhappy with the results, call `adict-change-dictionary' to
;; change it and stop automatic checks.
;;
;; You can use `adict-change-dictionary-hook' to hook into any of these changes,
;; or `adict-conditional-insert' to insert text (like signatures) that will
;; automatically conform to the language.
;;
;;; Change Log:
;;
;; 2007-05-28 (1.0.1)
;;    Speed improvements.
;;    Defined functions before variables that use them.
;;    Improved Swedish word list.  (thanks to Joakim Verona)
;;    Added support for Portuguese.  (thanks to Nelson Ferreira)
;;
;; 2007-05-28 (1.0)
;;    Added support for `flyspell-prog-mode'.
;;    Added `adict-' prefix to functions without.
;;    Support for Slovenian (thanks to Gregor Gorjanc), Hungarian and Romanian
;;      (thanks to Maria Muschinski).
;;    Bug fixes for Emacs 21.  (thanks to Gregor Gorjanc)
;;    Added `adict-conditional-' functions.
;;
;;; Code:

(require 'flyspell)
(require 'ispell)
(eval-when-compile (require 'cl))

(defgroup auto-dictionary nil
  "Automatic dictionary switcher for Flyspell."
  :group 'wp)

(defcustom adict-idle-time 2
  "*Seconds of idle time before `adict-guess-dictionary-maybe' is run.
If this is nil, it is not never automatically."
  :group 'auto-dictionary
  :type 'number)

(defcustom adict-change-threshold .02
  "*Amount of buffer change required before the dictionary is guessed again.
This is the quotient of changes to `buffer-modified-tick' and the buffer size.
Higher values mean fewer checks."
  :group 'auto-dictionary
  :type 'number)

(defface adict-conditional-text-face
  '((((class color) (background dark))
     (:background "MediumBlue"))
    (((class color) (background light))
     (:background "turquoise")))
  "*Face used for text inserted by `adict-conditional-insert'."
  :group 'auto-dictionary)

(defcustom adict-change-dictionary-hook
  '((lambda () (ignore-errors (when flyspell-mode (flyspell-buffer)))))
  "*List of functions to be called when the buffer language is changed.
This is called when `auto-dictionary-mode' changes its mind or
`adict-change-dictionary' is called."
  :group 'auto-dictionary
  :type 'hook)

(defun adict-guess-dictionary-name (names &optional list)
  "Return the element in NAMES found in `ispell-valid-dictionary-list'."
  (if list
       (or (car (member (car names) list))
           (when (cdr names)
             (adict-guess-dictionary-name (cdr names))))
    (or (adict-guess-dictionary-name
         names
         (if (fboundp 'ispell-valid-dictionary-list)
             (ispell-valid-dictionary-list)
           (cdr (mapcar 'car ispell-dictionary-alist))))
        (car names))))

(defvar adict-dictionary-list
  ;; we can't be sure of the actual dictionary names
  (mapcar 'adict-guess-dictionary-name
          '(nil
            ("en" "english")
            ("de" "deutsch" "german")
            ("fr" "francais" "french")
            ("es" "español" "spanish")
            ("sv" "svenska" "swedish")
            ("sl" "slovenian" "slovene")
            ("hu" "magyar" "hungarian")
            ("ro" "românâ" "româneşte" "romanian")
            ("pt" "português" "portuguese")))
  "The dictionaries `auto-dictionary-mode' uses.
Change them if you'd like a different region for your
language (e.g. \"en_US\" or \"american\").  Setting it to nil prevents
that language from being used.  The order must conform to the laguages
specified in `adict-language-list'")

(defvar adict-lighter nil)
(make-variable-buffer-local 'adict-lighter)

(defvar adict-timer nil)
(make-variable-buffer-local 'adict-timer)

(defvar adict-last-check 0)
(make-variable-buffer-local 'adict-last-check)

(defvar adict-stop-updating-on-dictionary-change t
  "*If this is true, calling `ispell-change-dictionary' will disable checks.")

(defalias 'switch-language-hook 'adict-change-dictionary-hook)

;;;###autoload
(define-minor-mode auto-dictionary-mode
  "A minor mode that automatically sets `ispell-dictionary`."
   nil adict-lighter nil
   (if auto-dictionary-mode
       (progn
         (setq adict-last-check 0)
         (adict-update-lighter)
         (unless adict-timer
           (setq adict-timer
                 (when adict-idle-time
                   (run-with-idle-timer adict-idle-time t
                                        'adict-guess-dictionary-maybe
                                        (current-buffer))))))
     (when adict-timer
       (cancel-timer adict-timer))
     (kill-local-variable 'adict-timer)
     (kill-local-variable 'adict-lighter)
     (kill-local-variable 'adict-last-check)))

(defalias 'adict-mode 'auto-dictionary-mode)

;;;###autoload
(defun adict-guess-dictionary (&optional idle-only)
  "Automatically change ispell dictionary based on buffer language.
Calls `ispell-change-dictionary' and runs `adict-change-dictionary-hook'.  If
BUFFER is nil, the current buffer is used.  If IDLE-ONLY is set, abort
when an input event occurs."
  (interactive)
  (let ((lang (nth (adict-find-max (adict-evaluate-buffer idle-only))
                   adict-dictionary-list)))
    (when lang
      (setq adict-last-check (buffer-modified-tick))
      (unless (or (equal ispell-local-dictionary lang)
                  (and (null ispell-local-dictionary)
                       (equal ispell-dictionary lang)))
        (let (adict-stop-updating-on-dictionary-change)
          (ignore-errors (adict-change-dictionary lang))))
      lang)))

(defsubst adict-valid-dictionary-p (lang)
  "Test if LANG is a legal dictionary."
  (member lang
          (if (fboundp 'ispell-valid-dictionary-list)
              (ispell-valid-dictionary-list)
            (mapcar 'car ispell-dictionary-alist))))

;;;###autoload
(defun adict-change-dictionary (&optional lang)
  "Set buffer language to LANG and stop detecting it automatically."
  (interactive)
  (if lang
      (if (adict-valid-dictionary-p lang)
          (progn (message "Buffer dictionary was %s"
                          (or ispell-local-dictionary ispell-dictionary))
                 (ispell-change-dictionary lang)
                 (message "Buffer dictionary is now %s" lang))
        (error "Dictionary \"%s\" not found" lang))
    (call-interactively 'ispell-change-dictionary))
  (adict-update-lighter)
  (run-hook-with-args 'adict-change-dictionary-hook)
  (when (and adict-timer adict-stop-updating-on-dictionary-change)
    (cancel-timer adict-timer)
    (kill-local-variable 'adict-timer)))

(defun adict-guess-dictionary-maybe (buffer)
  "Call `adict-guess-dictionary' or not based on `adict-change-threshold'."
  (with-current-buffer (or buffer (current-buffer))
    (when (and (eq (current-buffer) buffer)
               (> (- (buffer-modified-tick) adict-last-check)
                  (* adict-change-threshold (buffer-size))))
      (adict-guess-dictionary t))))

(defun adict-update-lighter ()
  (setq adict-lighter
        (format " %s"
                (substring (or ispell-local-dictionary ispell-dictionary "??")
                           0 2))))

(defun adict-foreach-word (beg end maxlength function &optional idle-only)
  "Execute FUNCTION for every word between BEG and END of length <= MAXLENGTH.
If IDLE-ONLY is set, abort when an input event occurs."
  (save-excursion
    (goto-char beg)
    (while (and (< (point) end)
                (not (and idle-only (input-pending-p))))
      (skip-syntax-forward "^w")
      (if (or (and flyspell-generic-check-word-p
                   (null (funcall flyspell-generic-check-word-p)))
              (memq nil
                    (mapcar (lambda (ov)
                              (not (overlay-get ov 'adict-conditional-list)))
                            (overlays-at (point)))))
          (skip-syntax-forward "w")
        (setq beg (point))
        (when (<= (skip-syntax-forward "w") maxlength)
;;           (message "%s" (buffer-substring-no-properties beg (point)))
          (funcall function (buffer-substring-no-properties beg (point))))))))

(defun adict-find-max (vector)
  (let* ((index (- (length vector) 1))
         (pos index)
         (max (elt vector pos)))
    (decf index)
    (while (> index 0)
      (let ((val (elt vector index)))
        (when (>= val max)
          (setq max val)
          (setq pos index))
        (decf index)))
    pos))

(defconst adict-language-list
  '(nil "en" "de" "fr" "es" "sv" "sl" "hu" "ro" "pt")
  "The languages, in order, which `adict-hash' contains.")

(defmacro adict-add-word (hash lang &rest words)
  `(dolist (word '(,@words))
     (when (gethash word hash)
       (message "Warning: adict-mode defined %s twice" word))
     (puthash word ,lang ,hash)))

(defvar adict-hash
  ;; Good words are frequent and unique to a language...
  ;; http://www.verbix.com/languages/
  (let ((hash (make-hash-table :test 'equal)))
    ;; all words should be downcase
    (adict-add-word hash 1 "and" "are" "at" "been" "but" "by" "dear" "for"
       "get" "have" "he" "hello" "it" "me" "my" "not" "on" "of" "off" "put"
       "regarding" "set" "she" "some" "that" "than" "the" "there" "to" "us"
       "was" "we" "while" "with" "yes" "you" "your" "yours")
    (adict-add-word hash 2 "eins" "zwei" "drei" "vier" "fünf" "sechs" "sieben"
       "acht" "neun" "zehn" "ab" "aber" "als" "andere" "anderem" "anderen"
       "anderes" "anders" "auf" "aus" "bei" "beide" "beidem" "beiden" "beides"
       "beim" "bereits" "bevor" "bis" "bisher" "bzw" "dabei" "dadurch"
       "dagegen" "daher" "damit" "danach" "dann" "daran" "darauf" "daraus"
       "darin" "darunter" "das" "dass" "davon" "dazu" "dem" "demselben"
       "denen" "denselben" "der" "derart" "deren" "derer" "derselben"
       "desselben" "dessen" "diese" "diesem" "diesen" "dieser" "dieses" "dir"
       "doch" "dort" "durch" "eben" "ebenfalls" "ein" "eine" "einem" "einen"
       "einer" "eines" "einzeln" "einzelne" "entweder" "er" "erst" "etwa"
       "etwas" "falls" "freundlichen" "ganz" "gegen" "gemeinsam" "genau" "haben"
       "hat" "hinter" "ich" "ihnen" "ihre" "ihrem" "ihren" "ihrer" "ihres" "im"
       "immer" "indem" "infolge" "insgesamt" "ist" "jede" "jedem" "jeden"
       "jeder" "jedes" "jedoch" "kann" "kein" "keine" "keinem" "keinen" "keiner"
       "keines" "mal" "mehr" "mehrere" "mehreren" "mehrerer" "mit" "mittels"
       "nach" "nacheinander" "neben" "nicht" "noch" "nur" "ob" "oberhalb" "oder"
       "ohne" "schreibe" "sehr" "selbst" "sich" "sie" "sind" "sobald" "sodass"
       "sofern" "sofort" "solange" "somit" "sondern" "sowie" "sowohl" "statt"
       "teils" "teilweise" "um" "und" "unter" "unterhalb" "vom" "usw" "von"
       "vor" "vorher" "warum" "wegen" "weil" "weiter" "weiterhin" "weitgehend"
       "welche" "welchem" "welchen" "welcher" "welches" "wenigstens" "wenn"
       "werden" "wie" "wieder" "wird" "wo" "wobei" "wodurch" "worauf" "worden"
       "worin" "wurde" "zu" "zueinander" "zugleich" "zum" "zumindest" "zur"
       "zusammen" "zwar" "zwecks" "zwischen" "bezüglich" "dafür" "für"
       "gegenüber" "gemäß" "schließlich" "über" "während" "würde" "zunächst"
       "zusätzlich")
    (adict-add-word hash 3 "aller" "allez" "allons" "alors" "au" "aux" "avoir"
       "bonjour" "ces" "cet" "cette" "combien" "comme" "dans" "dire" "dis"
       "disent" "disons" "dit" "dites" "elle" "elles" "et" "faire" "fais"
       "faisons" "fait" "faites" "il" "ils" "je" "là" "mais" "ne" "oui" "où"
       "parce" "pas" "plaît" "pour" "pourquoi" "quand" "qui" "revoir" "une"
       "des" "vais" "vas" "voient" "voir" "vois" "voit" "vont" "vous" "voyez"
       "voyons" "à" "ça" "être")
    (adict-add-word hash 4 "además" "ahora" "al" "algo" "algunos" "antes"
       "aquí" "así" "aunque" "año" "años" "bueno" "cada" "casa" "casi" "caso"
       "como" "con" "contra" "cosas" "creo" "cuando" "cómo" "decimos" "decir"
       "decis" "del" "desde" "después" "dicen" "dices" "digo" "dijo" "donde"
       "dos" "día" "días" "ejemplo" "ella" "ellos" "entonces" "entre"
       "era" "eres" "esa" "ese" "eso" "esta" "estaba" "estado" "estas"
       "esto" "estos" "está" "están" "forma" "fue" "general" "gente" "gobierno"
       "gran" "había" "hace" "hacemos" "hacen" "hacer" "haces" "hacia"
       "hacéis" "hago" "han" "hasta" "hay" "hecho" "hombre" "hoy" "las" "lo"
       "los" "luego" "mayor" "mejor" "menos" "mientras" "mismo"
       "momento" "mucho" "mujer" "mundo" "muy" "más" "mí" "nada" "ni" "no" "nos"
       "nosotros" "otra" "otras" "otro" "otros" "parece"
       "parte" "país" "pero" "personas" "poco" "poder" "política" "porque"
       "primera" "puede" "pueden" "qué" "sea" "según" "ser" "si"
       "siempre" "sino" "sois" "somos" "son" "soy" "su" "sus" "sí"
       "sólo" "tal" "también" "tan" "tanto" "tenemos" "tener" "tengo"
       "tenéis" "tenía" "tiempo" "tiene" "tienen" "tienes" "time" "toda" "todas"
       "todo" "todos" "trabajo" "tres" "una" "uno" "unos" "usted" "vamos" "ve"
       "veces" "veis" "ven" "veo" "ver" "ves" "vida" "y" "ya" "yo" "él")
    (adict-add-word hash 5 "och" "att" "en" "som" "det" "är" "av" "på"
       "ett" "två" "tre" "fyra" "fem" "sex" "sju" "åtta" "nio" "tio"
       "du" "jag" "inte" "nej" "vad" "defvar" "hej" "har" "kan"
       "om" "för" "till" "barn" "eller" "finns" "många" "när"
       "från" "ska" "klotter" "tycker" "sig" "vara" "vill" "konst" "så" "få"
       "mycket" "andra" "måste" "göra" "skulle" "deras" "sverige" "här" "sina"
       "bara" "också" "kommer" "hur" "alla" "gör" "sedan" "någon"
       "efter")
    (adict-add-word hash 6
       ;; slovenian (based on http://bos.zrc-sazu.si/a_top2000_si.html)
       "ali" "ampak" "bi" "bil" "biti" "bo" "bodo" "bolj" "bom" "brez" "čas"
       "če" "celo" "čeprav" "čez" "dan" "danes" "deset" "do" "dobro" "dolgo"
       "dovolj" "drugače" "drugi" "dva"
       "ena" "enkrat" "ga" "glede" "gotovo" "gre" "hitro" "hvala" "ima"
       "iz" "jasno" "jaz" "jih" "jim" "kaj" "kajti" "kako" "kar"
       "kateri" "kdaj" "kdo" "ker" "kje" "kljub" "kmalu" "ko" "koliko"
       "konec" "kot" "lahko" "lep" "let" "malo" "manj" "močno"
       "mogoče" "mu" "nad" "naj" "največ" "nam" "namreč" "naprej"
       "nas" "naš" "nazaj" "nekaj" "nič" "nihče" "nikoli" "niso"
       "niti" "nov" "očitno" "od" "okoli" "oziroma" "pa" "pač" "pet"
       "po" "počasi" "pod" "pol" "poleg" "potem" "pozdrav" "prav" "pred"
       "predvsem" "prej" "pri" "primer" "prosim" "proti" "prvi" "ravno"
       "res" "saj" "sam" "še" "sedaj" "šele" "sem" "seveda" "sicer"
       "skoraj" "skupaj" "smo" "so" "spet" "sploh" "sta" "ste" "število"
       "štiri" "sto" "stran" "svoj" "ta" "tako" "takoj" "takrat" "tam"
       "tega" "teh" "tem" "težko" "tisoč" "tisto"
       "tokrat" "toliko" "torej" "treba" "tudi" "tukaj" "več" "vedno"
       "veliko" "velja" "vendar" "vsaj" "vsak" "vse" "vsi" "za" "zadnji"
       "zakaj" "zakon" "zaradi" "zato" "zdaj" "že" "zelo" "zgolj")
    (adict-add-word hash 7 "az" "èn" "ö" "ti" "ök" "csak" "hogy"
       "nem" "igen" "és" "így" "úgy" "s" "jól" "van" "nincs" "nekem" "neki"
       "amely" "ki" "fel" "ezek" "azok" "ezen" "azon" "közé"
       "meg" "még" "azaz" "aki" "volt" "egyéb" "vagy" "ennek" "annak" "talán")
    (adict-add-word hash 8 "nu" "ea" "noi" "voi" "ei"
       "să" "în" "peste" "şi" "la" "unless" "despre" "din" "cele" "dintre"
       "avem" "vă" "oricare" "se" "acest" "fi" "pe" "care" "mai" "dacă" "cum"
       "te" "numai" "sunt""fost" "când" "aţi" "am" "pentru" "acum" "acesta"
       "ca" "sub" "ani")
    (adict-add-word hash 9
      ;; from http://home.unilang.org/main/wiki2/index.php/Portuguese_wordlist
      "e" "são" "em" "têm" "mas" "querido" "querida" "caro" "cara" "para"
      "obter" "pegar" "oi" "aquilo" "coisa" "meu" "não" "pôr"
      "meter" "colocar" "acerca" "algum" "alguns" "alguma" "algumas" "lá" "além"
      "nós" "eles" "ela" "elas" "teu" "enquanto" "com" "contigo" "você" "vosso"
      "sim" "olá" "tchau" "adeus" "bem-vindo" "obrigado" "obrigada" "já"
      "também" "sempre" "bonito" "certamente" "claramente" "cedo"
      "longe" "tarde" "provavelmente" "alto" "talvez" "muito" "perto"
      "agora" "apenas" "possivelmente" "raramente" "ainda" "acolá" "hoje"
      "amanhã" "improvável" "bem" "errado" "ontem")
    ;; don't use because they're ambiguous:
    ;; a i des du bien en es les que se tu un va el le te mi be is az da este
    ;; or ce le o de den ha med na sido para soble eu ele nunca ter lugar
    ;; adding another language? email me to make it available to everyone!
    hash))

(defun adict-evaluate-word (word)
  "Determine language of WORD using ``adict-hash''."
  (gethash (downcase word) adict-hash 0))

(defun adict-evaluate-buffer (&optional idle-only)
  "Evaluate all words in the current buffer to find out the text's language.
If IDLE-ONLY is set, abort when an input event occurs."
  (let ((counts (make-vector (length adict-language-list) 0)))
    (adict-foreach-word
     (point-min) (point-max) 8
     (lambda (word)
        ;; increase language count of WORD by one
        (callf incf (elt counts (adict-evaluate-word word))))
     idle-only)
    counts))

;;; Conditional Insertion ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar adict-conditional-overlay-list nil)
(make-variable-buffer-local 'adict-conditional-overlay-list)
(defun adict-conditional-insert (&rest language-text-pairs)

  "Insert text based on dictionary and update it on dictionary changes.
LANGUAGE-TEXT-PAIRS is a list of dictionaries and strings.  The correct
string for the currently active dictionary will be used.  Whenever
`auto-dictionary-mode' changes the dictionary the inserted text will be
changed again.

Use `t' as a dictionary in the last place to catch all remaining
dictionaries.

To highlight this volatile text, `adict-conditional-text-face' is used.


You can use this, for instance, to localize the \" writes\" text in Gnus:

  (defun my-message-insert-citation-line ()
    \"Insert a simple citation line in the correct language.\"
    (when message-reply-headers
      (insert (mail-header-from message-reply-headers) \" \")
      (adict-conditional-insert \"de\" \"schreibt\"
                                \"fr\" \"a écrit\"
                                t \"wrote\")
      (newline)
      (newline)))
  (setq message-citation-line-function 'my-message-insert-citation-line)"
  (let ((overlay (make-overlay 0 0)))
    (adict-conditional-insert-1 language-text-pairs overlay)
    (overlay-put overlay 'evaporate t)
    (overlay-put overlay 'modification-hooks
                 '(adict-conditional-modification))
    (overlay-put overlay 'adict-conditional-list language-text-pairs)
    (overlay-put overlay 'face 'adict-conditional-text-face)
    (add-hook 'adict-change-dictionary-hook
              'adict-conditional-update nil t)
    (push overlay adict-conditional-overlay-list)))

(defun adict-conditional-insert-1 (language-text-pairs overlay)
  (let ((dict (or ispell-local-dictionary ispell-dictionary))
        (beg (point)))
    (while language-text-pairs
      (when (or (eq (car language-text-pairs) t)
                (equal (car language-text-pairs) dict))
        (insert (cadr language-text-pairs))
        (setq language-text-pairs nil))
      (setq language-text-pairs (cddr language-text-pairs)))
    (move-overlay overlay beg (point))))

(defun adict-conditional-modification (overlay afterp beg end
                                       &optional pre-length)
  (when afterp
    (delete-overlay overlay)
    (unless (setq adict-conditional-overlay-list
                  (delq overlay adict-conditional-overlay-list))
      (kill-local-variable 'adict-conditional-overlay-list)
      (remove-hook 'adict-change-dictionary-hook 'adict-conditional-update t))))

(defun adict-conditional-update ()
  (save-excursion
    (let ((inhibit-modification-hooks t))
      (dolist (ov adict-conditional-overlay-list)
        (when (overlay-buffer ov)
          (goto-char (overlay-start ov))
          (delete-region (point) (overlay-end ov))
          (adict-conditional-insert-1 (overlay-get ov 'adict-conditional-list)
                                      ov))))))


;;; Functions for 3rd Party Use ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

(defun adict-guess-word-language (word)
  "Determine language of WORD using ``adict-hash''."
  (elt adict-language-list (adict-evaluate-word word)))

(defun adict-guess-buffer-language (&optional idle-only)
  "Guess the language of the current-buffer using the data in ``adict-hash''.
If IDLE-ONLY is set, abort when an input event occurs."
  (let ((lang (elt adict-language-list
                   (adict-find-max (adict-evaluate-buffer idle-only)))))
    (unless (and idle-only (input-pending-p))
      (when (interactive-p)
        (message "Buffer language: %s" lang))
      lang)))

(provide 'auto-dictionary)

;;; auto-dictionary.el ends here
