;;; powershell-mode.el --- Mode for editing Powershell scripts

;; Copyright (C) 2009, 2010 FrÃ©dÃ©ric Perrin

;; Author: Frédéric Perrin <frederic (dot) perrin (arobas) resel (dot) fr>
;; Keywords: Powershell, Monad, MSH
;; Last-Updated: 2010-12-21 Tue

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Comment:
;; This is still WIP.
;;
;; This was written from scratch, without using Vivek Sharma's code:
;; it had issues I wanted to correct, but unfortunately there were no
;; licence indication, and Vivek didn't answered my mails.
;;
;; This is still pretty basic: there is indentation, syntax
;; hilighting, speedbar/imenu support. The indentation is pretty naÃ¯ve
;; but robust, and sufficient for my current needs.
;;
;;; Changes
;; 2010-12-21 Lennart Borgman: minor changes

;; (setq debug-on-error t)

(defgroup powershell nil
  "Customization group for `powershell-mode'."
  :group 'languages)

(defcustom powershell-indent standard-indent
  "Default number of columns for margin-changing functions to indent."
  :group 'powershell
  :type 'integer)

(defcustom powershell-continuation-indent (/ standard-indent 2)
  "Default amount of horizontal space to indent a continuation line"
  :group 'powershell
  :type 'integer)

(defvar powershell-continued-regexp  ".*\\(|[\\t ]*\\|`\\|,\\)$"
  "Regexp matching a continued line.
Those ends either with an explicit backtick, a comma, or a
pipe).")

(defun powershell-continuation-line-p ()
  "Returns t is the current line is a continuation line (i.e. the
previous line is a continued line, ending with a backtick or a pipe"
  (interactive)
  (save-excursion
    (forward-line -1)
    (looking-at powershell-continued-regexp)))

(defun powershell-indent-line-amount ()
  "Returns the column to which the current line ought to be indented."
  (interactive)
  (beginning-of-line)
  (let ((closing-paren (looking-at "[\t ]*[])}]")))
    ;; a very simple indentation method: if on a continuation line (i.e. the
    ;; previous line ends with a trailing backtick or pipe), we indent relative
    ;; to the continued line; otherwise, we indent relative to the ([{ that
    ;; opened the current block.
    (if (powershell-continuation-line-p)
	(progn
	  (while (powershell-continuation-line-p)
	    (forward-line -1))
	  (+ (current-indentation) powershell-continuation-indent))
      (condition-case nil
	  (progn
	    (backward-up-list)
	    ;; indentation relative to the opening paren: if there is text (no
	    ;; comment) after the opening paren, vertically align the block
	    ;; with this text; if we were looking at the closing paren, reset
	    ;; the indentation; otherwise, indent the block by powershell-indent.
	    (cond ((not (looking-at ".[\t ]*\\(#.*\\)?$"))
		   (forward-char)
		   (skip-chars-forward " \t")
		   (current-column))
		  (closing-paren
		   (current-indentation))
		  (t
		   (+ (current-indentation) powershell-indent))))
	(scan-error ;; most likely, we are at the top-level
	 0)))))

(defun powershell-indent-line ()
  "Indent the current line of powershell mode, leaving the point
in place if it is inside the meat of the line"
  (interactive)
  (let ((savep (> (current-column) (current-indentation)))
	(amount (save-excursion (powershell-indent-line-amount))))
    (if savep
	(save-excursion (indent-line-to amount))
      (indent-line-to amount))))


;; Taken from <http://www.manning.com/payette/AppCexcerpt.pdf> which seems the
;; closest to a grammar definition for powershell. It is not complete, and
;; contains some inaccuracies (e.g. it says that variables match \$[:alnum:]+,
;; so $_ is not a variable it seems...)

(defvar powershell-keywords
  (regexp-opt '("begin" "break" "catch" "continue" "data" "do" "dynamicparam"
		"else" "elseif" "end" "exit" "filter" "finally" "for" "foreach"
		"from" "function" "if" "in" "param" "process" "return"
		"switch" "throw" "trap" "try" "until" "while"))
  "Powershell keywords")

(defvar powershell-operators
  (regexp-opt '("and" "as" "band" "bnot" "bor" "bxor" "casesensitive"
		"ccontains" "ceq" "cge" "cgt" "cle" "clike" "clt" "cmatch"
		"cne" "cnotcontains" "cnotlike" "cnotmatch" "contains"
		"creplace" "eq" "exact" "f" "file" "ge" "gt" "icontains"
		"ieq" "ige" "igt" "ile" "ilike" "ilt" "imatch" "ine"
		"inotcontains" "inotlike" "inotmatch" "ireplace" "is"
		"isnot" "le" "like" "lt" "match" "ne" "not" "notcontains"
		"notlike" "notmatch" "or" "replace" "wildcard"))
  "Powershell operators")

(defvar powershell-scope-names
  (regexp-opt
  '("env" "function" "global" "local" "private" "script" "variable"))
  "Names of scopes in Powershell mode.")

;; Taken from Get-Variable on a fresh shell, merged with man
;; about_automatic_variables
(defvar powershell-builtin-variables
  (regexp-opt
   '("^" "_" "$" "?" "Args" "ConfirmPreference" "ConsoleFileName"
     "DebugPreference" "Error" "ErrorActionPreference" "ErrorView"
     "ExecutionContext" "foreach" "FormatEnumerationLimit" "HOME" "Host"
     "Input" "LASTEXITCODE" "MaximumAliasCount" "MaximumDriveCount"
     "MaximumErrorCount" "MaximumFunctionCount" "MaximumHistoryCount"
     "MaximumVariableCount" "MyInvocation" "NestedPromptLevel" "OFS"
     "OutputEncoding" "PID" "PROFILE" "PSHOME" "PWD" "ProgressPreference"
     "ReportErrorShowExceptionClass" "ReportErrorShowInnerException"
     "ReportErrorShowSource" "ReportErrorShowStackTrace" "ShellId"
     "ShouldProcessPreference" "ShouldProcessReturnPreference" "StackTrace"
     "VerbosePreference" "WarningPreference" "WhatIfPreference" "false"
     "input" "lastWord" "line" "null" "true" ))
  "Names of the built-in Powershell variables. They are hilighted
differently from the other variables.")

(defvar powershell-font-lock-keywords-1
  `(;; Type annotations
    ("\\[\\([[:word:].]+\\(\\[\\]\\)?\\)\\]" 1 font-lock-type-face)
    ;; syntaxic keywords
    (,(concat "\\<" powershell-keywords "\\>") . font-lock-keyword-face)
    ;; operators
    (,(concat "\\<-" powershell-operators "\\>") . font-lock-builtin-face)
    ;; the REQUIRES mark
    ("^#\\(REQUIRES\\)" 1 font-lock-warning-face t))
  "Keywords for the first level of font-locking in Powershell mode.")

(defvar powershell-font-lock-keywords-2
  (append
   powershell-font-lock-keywords-1
   `(;; Built-in variables
     (,(concat "\\(\\$" powershell-builtin-variables "\\)\\>")
      1 font-lock-builtin-face t)))
  "Keywords for the second level of font-locking in Powershell mode.")

(defvar powershell-font-lock-keywords-3
  (append
   powershell-font-lock-keywords-2
   `(;; Variables in curly brackets
     ("\\${\\([^}]+\\)}" 1 font-lock-variable-name-face)
     ;; Variables, with a scope
     (,(concat "\\$\\(" powershell-scope-names "\\):"
	       "\\([[:alnum:]_]+\\)")
      (1 (cons font-lock-type-face '(underline)))
      (2 font-lock-variable-name-face))
     ;; Variables, without a scope. XXX: unify this with the
     ;; previous rule?
     ("\\$\\([[:alnum:]_]+\\)" 1 font-lock-variable-name-face)
     ;; hilight properties, but not the methods (personnal preference)
     ("\\.\\([[:alnum:]_.]+\\)\\>\\s *[^(]" 1 font-lock-variable-name-face)))
  "Keywords for the maximum level of font-locking in Powershell mode.")


(defvar powershell-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)
    ;; Powershell uses a backtick as its escape character.
    (modify-syntax-entry ?` "\\" table)
    (modify-syntax-entry ?\\ "_" table)
    (modify-syntax-entry ?- "w" table)
    (modify-syntax-entry ?' "\"" table)
    table)
  "Syntax table for Powershell mode")



(defvar powershell-imenu-expression
  `(("Functions" "function \\(\\w+\\)" 1)
    ("Top variables" ,(concat "^\\$\\(" powershell-scope-names "\\)?:?"
			      "\\([[:alnum:]_]+\\)")
     2))
  "List of regexps matching important expressions, for speebar & imenu.")


(require 'compile nil t)
;; A better command would be something like "powershell.exe -NoLogo
;; -NonInteractive -Command & (buffer-file-name)". But it will just
;; sit there waiting...  The following will only work when .ps1 files
;; are associated with powershell.exe. And if they don't contain spaces.
;;
;;;;;;;;;
;; Lennart Borgman:
;;
;; I tried to find out another way to do the compile and run the
;; script, but I was unable to get anything except ftype + assoc
;; working. Note however that you can add the parameters
;; -NonInteractive etc to the ftype command (and that the name can
;; have spaces), see the beginning of this file.
;;
;;;;;;;
;; Tests I make
;;
;; (let ((b (get-buffer-create "TEST")) (shell-file-name "powershell.exe")) (display-buffer b) (start-file-process-shell-command "NAME" b "c:\\u\\DesktopSearch.ps1"))

;; (let ((b (get-buffer-create "TEST")) (shell-file-name "powershell.exe")) (display-buffer b) (start-file-process-shell-command "NAME" b "c:\\u\\ps1.cmd c:\\u\\DesktopSearch.ps1"))

;; (let ((b (get-buffer-create "TEST"))) (display-buffer b) (start-file-process-shell-command "NAME" b "c:\\u\\ps1.cmd c:\\u\\DesktopSearch.ps1"))

;; (let ((b (get-buffer-create "TEST"))) (display-buffer b) (start-process "NAME" b "cmd.exe" "/c" "c:\\u\\ps1.cmd" "c:\\u\\DesktopSearch.ps1" "yyy")) <---------------

;; (let ((b (get-buffer-create "TEST"))) (display-buffer b) (start-process "NAME" b "cmd.exe" "/c" "c:\\u\\DesktopSearch.ps1" "c" "abstract")) <====================

;; (let ((b (get-buffer-create "TEST"))) (display-buffer b) (start-process "NAME" b "powershell.exe" "-nologo" "-noninteractive" "-file" "c:\\u\\DesktopSearch.ps1"))

;; (let ((b (get-buffer-create "TEST")) (shell-file-name "powershell.exe")) (display-buffer b) (start-file-process-shell-command "NAME" b (shell-quote-argument "c:\\u\\DesktopSearch.ps1")))

(defvar powershell-compile-command
  ;;'(buffer-file-name)
  ;;'(concat "powershell -NoLogo -NonInteractive " (buffer-file-name))
  ;;'(shell-quote-argument (buffer-file-name))
  ;;'(concat "c:\\u\\ps1.cmd " (shell-quote-argument (convert-standard-filename (buffer-file-name))))
  ;;'(convert-standard-filename (buffer-file-name))
  '(shell-quote-argument (convert-standard-filename (buffer-file-name)))
  "Default command used to invoke a powershell script")

;; The column number will be off whenever tabs are used. Since this is
;; the default in this mode, we will not capture the column number.
(defvar powershell-compilation-error-regexp-alist
      '(powershell "At \\(.*\\):\\([0-9]+\\) char:\\([0-9]+\\)" 1 2))


;; the hook is automatically run by derived-mode
(defvar powershell-mode-hook '(imenu-add-menubar-index)
  "Hook run after the initialization of Powershell mode.")

(define-derived-mode powershell-mode fundamental-mode "PS"
  "A major mode for editing Powershell script files.
NOTE: It is a bit problematic running Powershell scripts from
withing Emacs, but it can be done.
To make it work you must set up the file associations something like this:

 assoc .ps1=MS.PowerShellScript
 ftype MS.PowerShellScript=PATH-TO\powershell.exe -noprofile -noninteractive -file \"%1\" %2 %3 %4

With this you can run `compile' from this mode \(and use the
script from within Emacs)."
  (set (make-local-variable 'indent-line-function) 'powershell-indent-line)
  (set (make-local-variable 'font-lock-defaults)
       '((powershell-font-lock-keywords-1
	  powershell-font-lock-keywords-2
	  powershell-font-lock-keywords-3)
	 nil t))
  (set (make-local-variable 'comment-start) "# ")
  (set (make-local-variable 'comment-start-skip) "#+\\s*")
  ;; not sure why this is not the default
  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  (set-syntax-table powershell-mode-syntax-table)
  (set (make-local-variable 'imenu-generic-expression)
       powershell-imenu-expression)
  (when (require 'speedbar nil t)
    (speedbar-add-supported-extension ".ps1?"))
  (set (make-local-variable 'imenu-case-fold-search) nil)
  (set (make-local-variable 'compile-command) powershell-compile-command)
  ;;(set (make-local-variable 'compile-shell-file-name) "powershell.exe")
  ;;(setq compilation-error-regexp-alist-alist (assq-delete-all 'powershell compilation-error-regexp-alist-alist))
  (unless (assoc 'powershell compilation-error-regexp-alist-alist)
    (setq compilation-error-regexp-alist-alist
          (cons powershell-compilation-error-regexp-alist
                compilation-error-regexp-alist-alist)))
  (unless (memq 'powershell compilation-error-regexp-alist)
    (setq compilation-error-regexp-alist
          (cons 'powershell compilation-error-regexp-alist)))
  )

(provide 'powershell-mode)
