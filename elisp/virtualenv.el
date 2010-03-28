;;; virtualenv.el --- Virtualenv for Python mode  -*- coding: utf-8 -*-

 ;; Copyright (c) 2010 - Jesse Legg <jesse@jesselegg.com>
 ;; MIT License

 ;; Permission is hereby granted, free of charge, to any person
 ;; obtaining a copy of this software and associated documentation
 ;; files (the "Software"), to deal in the Software without
 ;; restriction, including without limitation the rights to use,
 ;; copy, modify, merge, publish, distribute, sublicense, and/or sell
 ;; copies of the Software, and to permit persons to whom the
 ;; Software is furnished to do so, subject to the following
 ;; conditions:

 ;; The above copyright notice and this permission notice shall be
 ;; included in all copies or substantial portions of the Software.

 ;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 ;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
 ;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 ;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
 ;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
 ;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 ;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
 ;; OTHER DEALINGS IN THE SOFTWARE.
 ;;

 ;; NOTES:
 ;; Support for virtual environments in python.el. This should 
 ;; probably be converted to a derived mode or something else.

(require 'python)

(defvar virtualenv-postactivate-hook nil)
(defvar virtualenv-postdeactivate-hook nil)

(defcustom virtualenv-use-ipython t
  "Use IPython interpreter when available in a virtual environment")

(defcustom virtualenv-root-dir (concat (getenv "HOME") "/.virtualenvs")
  "Default location for user's virtual environments")

(defcustom virtualenv-ipython-flags '("-cl")
  "Extra flags to pass to IPython interpreters")

(defcustom virtualenv-python-flags '("-i")
  "Extra flags to pass to Python interpreters")

(defvar virtualenv-active nil
  "The current active virtual environment or nil")

(defconst virtualenv-default-interpreter python-python-command)
(defconst virtualenv-default-interpreter-args python-python-command-args)

(defun virtualenv-get-interpreter (virtualenv)
  (if (and virtualenv-use-ipython
           (file-exists-p (concat virtualenv "/bin/ipython")))
      (concat virtualenv "/bin/ipython")
    (concat virtualenv "/bin/python")))

(defun virtualenv-get-interpreter-args (virtualenv)
  (if (and virtualenv-use-ipython
           (file-exists-p (concat virtualenv "/bin/ipython")))
      virtualenv-ipython-flags
    virtualenv-python-flags))

(defun virtualenv-set-interpreter (virtualenv)
  (if (null virtualenv)
      (progn 
        (setq python-python-command virtualenv-default-interpreter)
        (setq python-python-command-args virtualenv-default-interpreter-args))
    (let ((interpreter (virtualenv-get-interpreter virtualenv))
          (interpreter-args (virtualenv-get-interpreter-args virtualenv)))
      (setq python-python-command interpreter)
      (setq python-python-command-args interpreter-args))))

(defun virtualenv-activate-environment (virtualenv)
  (virtualenv-set-interpreter virtualenv)
  (python-toggle-shells 'cpython)
  (setq virtualenv-active virtualenv)
  (run-hook-with-args virtualenv-postactivate-hook virtualenv))

(defun virtualenv-activate (env)
  (interactive "sName of environment to activate: ")
  (virtualenv-activate-environment (concat virtualenv-root-dir env)))

(defun virtualenv-deactivate-environment ()
  (let ((virtualenv-current-env virtualenv-active))
    (setq virtualenv-active nil)
    (virtualenv-set-interpreter nil)
    (python-toggle-shells 'cpython)
    (run-hook-with-args virtualenv-postdeactivate-hook virtualenv-current-env)))

(defun virtualenv-deactivate ()
  (interactive)
  (virtualenv-deactivate-environment))

(provide 'virtualenv)
