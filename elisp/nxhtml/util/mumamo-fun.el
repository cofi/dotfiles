;;; mumamo-fun.el --- Multi major mode functions
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2008-03-09T01:35:21+0100 Sun
;; Version: 0.51
;; Last-Updated: 2008-08-04T17:54:29+0200 Mon
;; URL:
;; Keywords:
;; Compatibility:
;;
;; Features that might be required by this library:
;;
;;   `backquote', `bytecomp', `cl', `flyspell', `ispell', `mumamo',
;;   `sgml-mode'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Defines some "multi major modes" functions.  See mumamo.el for more
;; information.
;;
;;;; Usage:
;;
;;  See mumamo.el for how to use the multi major mode functions
;;  defined here.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(eval-when-compile (require 'cl))
(eval-when-compile (add-to-list 'load-path default-directory))
(eval-when-compile (require 'mumamo))
(eval-and-compile (require 'mumamo-chunks))
(declare-function nxhtml-validation-header-mode "nxhtml-mode")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; PHP, HTML etc.

;;;###autoload
(define-mumamo-multi-major-mode html-mumamo-mode
  "Turn on multiple major modes for (X)HTML with main mode `html-mode'.
This covers inlined style and javascript and PHP."
  ("HTML Family" html-mode
   (mumamo-chunk-xml-pi
    mumamo-chunk-alt-php
    mumamo-chunk-alt-php=
    mumamo-chunk-inlined-style
    mumamo-chunk-inlined-script
    mumamo-chunk-style=
    mumamo-chunk-onjs=
    )))
(add-hook 'html-mumamo-mode-hook 'mumamo-define-html-file-wide-keys)
(mumamo-inherit-sub-chunk-family 'html-mumamo-mode)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; XHTML w nxml-mode

;;;###autoload
(define-mumamo-multi-major-mode nxml-mumamo-mode
  "Turn on multiple major modes for (X)HTML with main mode `nxml-mode'.
This covers inlined style and javascript and PHP.

See also `mumamo-alt-php-tags-mode'."
  ("nXml Family" nxml-mode
   (mumamo-chunk-xml-pi
    mumamo-chunk-alt-php
    mumamo-chunk-inlined-style
    mumamo-chunk-inlined-script
    mumamo-chunk-style=
    mumamo-chunk-onjs=
    )))
(add-hook 'nxml-mumamo-mode-hook 'mumamo-define-html-file-wide-keys)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Mason (not quite ready)
;; http://www.masonhq.com/docs/manual/Devel.html#examples_and_recommended_usage

;;;###autoload
(define-mumamo-multi-major-mode mason-html-mumamo-mode
  "Turn on multiple major modes for Mason using main mode `html-mode'.
This covers inlined style and javascript."
  ("Mason html Family" html-mode
   (
    mumamo-chunk-mason-perl-line
    mumamo-chunk-mason-perl-single
    mumamo-chunk-mason-perl-block
    mumamo-chunk-mason-perl-init
    mumamo-chunk-mason-perl-once
    mumamo-chunk-mason-perl-cleanup
    mumamo-chunk-mason-perl-shared
    mumamo-chunk-mason-simple-comp
    mumamo-chunk-mason-compcont
    mumamo-chunk-mason-args
    mumamo-chunk-mason-doc
    mumamo-chunk-mason-text
    mumamo-chunk-inlined-style
    mumamo-chunk-inlined-script
    mumamo-chunk-style=
    mumamo-chunk-onjs=
    )))
(add-hook 'mason-html-mumamo-mode-hook 'mumamo-define-html-file-wide-keys)
(mumamo-inherit-sub-chunk-family-locally 'mason-html-mumamo-mode 'mason-html-mumamo-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Embperl

;;;###autoload
(define-mumamo-multi-major-mode embperl-html-mumamo-mode
  "Turn on multiple major modes for Embperl files with main mode `html-mode'.
This also covers inlined style and javascript."
  ("Embperl HTML Family" html-mode
   (mumamo-chunk-embperl-<-
    mumamo-chunk-embperl-<+
    mumamo-chunk-embperl-<!
    mumamo-chunk-embperl-<$
    mumamo-chunk-inlined-style
    mumamo-chunk-inlined-script
    mumamo-chunk-style=
    mumamo-chunk-onjs=
    )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; django

;;;###autoload
(define-mumamo-multi-major-mode django-html-mumamo-mode
  "Turn on multiple major modes for Django with main mode `html-mode'.
This also covers inlined style and javascript."
  ("Django HTML Family" html-mode
   (mumamo-chunk-django4
    mumamo-chunk-django
    mumamo-chunk-django2
    mumamo-chunk-django3
    mumamo-chunk-inlined-style
    mumamo-chunk-inlined-script
    mumamo-chunk-style=
    mumamo-chunk-onjs=
    )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Genshi / Kid

;;;###autoload
(define-mumamo-multi-major-mode genshi-html-mumamo-mode
  "Turn on multiple major modes for Genshi with main mode `html-mode'.
This also covers inlined style and javascript."
  ("Genshi HTML Family" html-mode
   (
    ;;mumamo-chunk-genshi%
    mumamo-chunk-genshi$
    mumamo-chunk-py:=
    mumamo-chunk-py:match
    mumamo-chunk-xml-pi
    mumamo-chunk-inlined-style
    mumamo-chunk-inlined-script
    mumamo-chunk-style=
    mumamo-chunk-onjs=
    )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; MJT

;;;###autoload
(define-mumamo-multi-major-mode mjt-html-mumamo-mode
  "Turn on multiple major modes for MJT with main mode `html-mode'.
This also covers inlined style and javascript."
  ("MJT HTML Family" html-mode
   (
    mumamo-chunk-mjt$
    mumamo-chunk-xml-pi
    mumamo-chunk-inlined-style
    mumamo-chunk-inlined-script
    mumamo-chunk-style=
    mumamo-chunk-onjs=
    )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; smarty

;;;###autoload
(define-mumamo-multi-major-mode smarty-html-mumamo-mode
  "Turn on multiple major modes for Smarty with main mode `html-mode'.
This also covers inlined style and javascript."
  ("Smarty HTML Family" html-mode
   (mumamo-chunk-xml-pi
    mumamo-chunk-style=
    mumamo-chunk-onjs=
    ;;mumamo-chunk-inlined-style
    ;;mumamo-chunk-inlined-script
    mumamo-chunk-smarty-literal
    mumamo-chunk-smarty-t
    mumamo-chunk-smarty-comment
    mumamo-chunk-smarty
    )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ssjs - server side javascript

;; http://www.sitepoint.com/blogs/2009/03/10/server-side-javascript-will-be-as-common-as-php/
;;
;; It looks like there are different syntaxes, both
;;
;;  <script runat="server">...</script> and <% ... %>.

;;;###autoload
(define-mumamo-multi-major-mode ssjs-html-mumamo-mode
  "Turn on multiple major modes for SSJS with main mode `html-mode'.
This covers inlined style and javascript."
  ("HTML Family" html-mode
   (mumamo-chunk-inlined-style
    mumamo-chunk-inlined-script
    mumamo-chunk-inlined-ssjs
    mumamo-chunk-ssjs-%
    mumamo-chunk-style=
    mumamo-chunk-onjs=
    )))
(add-hook 'html-mumamo-mode-hook 'mumamo-define-html-file-wide-keys)
(mumamo-inherit-sub-chunk-family 'ssjs-html-mumamo-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; gsp

;;;###autoload
(define-mumamo-multi-major-mode gsp-html-mumamo-mode
  "Turn on multiple major modes for GSP with main mode `html-mode'.
This also covers inlined style and javascript."
  ("GSP HTML Family" html-mode
   (mumamo-chunk-gsp
    mumamo-chunk-inlined-style
    mumamo-chunk-inlined-script
    mumamo-chunk-style=
    mumamo-chunk-onjs=
    )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; jsp - Java Server Pages

;;;###autoload
(define-mumamo-multi-major-mode jsp-html-mumamo-mode
  "Turn on multiple major modes for JSP with main mode `html-mode'.
This also covers inlined style and javascript."
  ("JSP HTML Family" html-mode
   (mumamo-chunk-jsp-hidden-comment
    mumamo-chunk-jsp
    mumamo-chunk-inlined-style
    mumamo-chunk-inlined-script
    mumamo-chunk-style=
    mumamo-chunk-onjs=
    )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; eruby

;; See also http://wiki.rubyonrails.org/rails/pages/UnderstandingViews

;; ;;;###autoload
;; (define-mumamo-multi-major-mode eruby-mumamo-mode
;;   "Turn on multiple major mode for eRuby with unspecified main mode.
;; Current major-mode will be used as the main major mode."
;;   ("eRuby Family" nil
;;    (mumamo-chunk-eruby-comment
;;     mumamo-chunk-eruby=
;;     mumamo-chunk-eruby
;;     )))

;;;###autoload
(define-mumamo-multi-major-mode eruby-html-mumamo-mode
  "Turn on multiple major modes for eRuby with main mode `html-mode'.
This also covers inlined style and javascript.

The eRuby chunks handled are:

       <% Ruby code -- inline with output %>
       <%= Ruby expression -- replace with result %>
       <%# comment -- ignored -- useful in testing %>

See URL `https://bugs.launchpad.net/nxhtml/+bug/579581' for
information about <%% ... %%>, % and %%."
  ("eRuby Html Family" html-mode
   (
    mumamo-chunk-eruby-comment
    mumamo-chunk-eruby=
    mumamo-chunk-eruby
    mumamo-chunk-inlined-style
    mumamo-chunk-inlined-script
    mumamo-chunk-style=
    mumamo-chunk-onjs=
    )))

;;;###autoload
(define-mumamo-multi-major-mode eruby-javascript-mumamo-mode
  "Turn on multiple major modes for eRuby with main mode `javascript-mode'."
  ("eRuby Html Family" javascript-mode
   (
    mumamo-chunk-eruby-comment
    mumamo-chunk-eruby=quoted
    mumamo-chunk-eruby
    )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; heredoc

;;;###autoload
(define-mumamo-multi-major-mode sh-heredoc-mumamo-mode
  "Turn on multiple major modes for sh heredoc document.
See `mumamo-heredoc-modes' for how to specify heredoc major modes."
  ("SH HereDoc" sh-mode
   (mumamo-chunk-sh-heredoc
    )))
(mumamo-inherit-sub-chunk-family 'sh-heredoc-mumamo-mode)


;;;###autoload
(define-mumamo-multi-major-mode php-heredoc-mumamo-mode
  "Turn on multiple major modes for PHP heredoc document.
See `mumamo-heredoc-modes' for how to specify heredoc major modes."
  ("PHP HereDoc" php-mode
   (mumamo-chunk-php-heredoc
    )))
(mumamo-inherit-sub-chunk-family 'php-heredoc-mumamo-mode)
(mumamo-inherit-sub-chunk-family-locally 'php-heredoc-mumamo-mode 'html-mumamo-mode)


;;;###autoload
(define-mumamo-multi-major-mode perl-heredoc-mumamo-mode
  "Turn on multiple major modes for Perl heredoc document.
See `mumamo-heredoc-modes' for how to specify heredoc major modes."
  ("Perl HereDoc" perl-mode
   (mumamo-chunk-perl-heredoc
    )))
(mumamo-inherit-sub-chunk-family 'perl-heredoc-mumamo-mode)

;;;###autoload
(define-mumamo-multi-major-mode cperl-heredoc-mumamo-mode
  "Turn on multiple major modes for Perl heredoc document.
See `mumamo-heredoc-modes' for how to specify heredoc major modes."
  ("Perl HereDoc" cperl-mode
   (mumamo-chunk-perl-heredoc
    )))
(mumamo-inherit-sub-chunk-family 'cperl-heredoc-mumamo-mode)


;;;###autoload
(define-mumamo-multi-major-mode python-heredoc-mumamo-mode
  "Turn on multiple major modes for Python heredoc document.
See `mumamo-heredoc-modes' for how to specify heredoc major modes."
  ("Python HereDoc" python-mode
   (mumamo-chunk-python-heredoc
    )))
(mumamo-inherit-sub-chunk-family 'python-heredoc-mumamo-mode)


;;;###autoload
(define-mumamo-multi-major-mode ruby-heredoc-mumamo-mode
  "Turn on multiple major modes for Ruby heredoc document.
See `mumamo-heredoc-modes' for how to specify heredoc major modes."
  ("Ruby HereDoc" ruby-mode
   (mumamo-chunk-ruby-heredoc
    )))
(mumamo-inherit-sub-chunk-family 'ruby-heredoc-mumamo-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Tex meta

;;;###autoload
(define-mumamo-multi-major-mode metapost-mumamo-mode
  "Turn on multiple major modes for MetaPost."
  ("MetaPost TeX Family" metapost-mode
   (mumamo-chunk-textext
    mumamo-chunk-verbatimtex
    )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; OpenLaszlo

;;;###autoload
(define-mumamo-multi-major-mode laszlo-nxml-mumamo-mode
  "Turn on multiple major modes for OpenLaszlo."
  ("OpenLaszlo Family" nxml-mode
   (mumamo-chunk-inlined-script
    mumamo-chunk-inlined-lzx-method
    mumamo-chunk-inlined-lzx-handler
    )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; csound

;;;###autoload
(define-mumamo-multi-major-mode csound-sgml-mumamo-mode
  "Turn on mutiple major modes for CSound orc/sco Modes."
  ("CSound orc/sco Modes" sgml-mode
   (mumamo-chunk-csound-sco
    mumamo-chunk-csound-orc
    )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; noweb

;;;###autoload
(define-mumamo-multi-major-mode noweb2-mumamo-mode
  "Multi major mode for noweb files."
  ("noweb Family" latex-mode
   (mumamo-noweb2-code-chunk)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Template-Toolkit

;; (setq auto-mode-alist
;;       (append '(("\\.tt2?$" . tt-mode))  auto-mode-alist ))
;;(require 'tt-mode)

;;;###autoload
(define-mumamo-multi-major-mode tt-html-mumamo-mode
  "Turn on multiple major modes for TT files with main mode `nxhtml-mode'.
TT = Template-Toolkit.

This also covers inlined style and javascript."
  ("TT HTML Family" html-mode
   (mumamo-chunk-tt
    mumamo-chunk-inlined-style
    mumamo-chunk-inlined-script
    mumamo-chunk-style=
    mumamo-chunk-onjs=
    )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Asp

;;;###autoload
(define-mumamo-multi-major-mode asp-html-mumamo-mode
  "Turn on multiple major modes for ASP with main mode `html-mode'.
This also covers inlined style and javascript."
  ("ASP Html Family" html-mode
   (mumamo-chunk-asp%
    mumamo-chunk-asp-server-script
    mumamo-chunk-inlined-script
    mumamo-chunk-style=
    mumamo-chunk-onjs=
    )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Org-mode

;;;###autoload
(define-mumamo-multi-major-mode org-mumamo-mode
  "Turn on multiple major modes for `org-mode' files with main mode `org-mode'.
** Note about HTML subchunks:
Unfortunately this only allows `html-mode' (not `nxhtml-mode') in
sub chunks."
  ("Org Mode + Html" org-mode
   (mumamo-chunk-org-html
    mumamo-chunk-org-src
    )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Mako

;; See http://www.makotemplates.org/docs/syntax.html

;;;###autoload
(define-mumamo-multi-major-mode mako-html-mumamo-mode
  "Turn on multiple major modes for Mako with main mode `html-mode'.
This also covers inlined style and javascript."
  ;; Fix-me: test case
  ;;
  ;; Fix-me: Add chunks for the tags, but make sure these are made
  ;; invisible to nxml-mode parser.
  ;;
  ;; Fix-me: Maybe finally add that indentation support for one-line chunks?
  ("Mako HTML Family" html-mode
   (
    mumamo-chunk-mako-one-line-comment
    mumamo-chunk-mako-<%doc
    mumamo-chunk-mako-<%include
    mumamo-chunk-mako-<%inherit
    mumamo-chunk-mako-<%namespace
    mumamo-chunk-mako-<%page

    mumamo-chunk-mako-<%def
    ;;mumamo-chunk-mako-<%namesp:name
    mumamo-chunk-mako-<%call
    ;;mumamo-chunk-mako-<%text

    mumamo-chunk-mako-<%
    mumamo-chunk-mako-%
    mumamo-chunk-mako$

    mumamo-chunk-xml-pi
    mumamo-chunk-inlined-style
    mumamo-chunk-inlined-script
    mumamo-chunk-style=
    mumamo-chunk-onjs=
    )))
(mumamo-inherit-sub-chunk-family-locally 'mako-html-mumamo-mode 'mako-html-mumamo-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; XSL

;;;###autoload
(define-mumamo-multi-major-mode xsl-nxml-mumamo-mode
  "Turn on multi major mode for XSL with main mode `nxml-mode'.
This covers inlined style and javascript."
  ("XSL nXtml Family" nxml-mode
   (
    mumamo-chunk-inlined-style
    mumamo-chunk-inlined-script
    )))

;;;###autoload
(define-mumamo-multi-major-mode xsl-sgml-mumamo-mode
  "Turn on multi major mode for XSL with main mode `sgml-mode'.
This covers inlined style and javascript."
  ("XSL SGML Family" sgml-mode
   (
    mumamo-chunk-inlined-style
    mumamo-chunk-inlined-script
    )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Markdown

;;;###autoload
(define-mumamo-multi-major-mode markdown-html-mumamo-mode
  "Turn on multi major markdown mode in buffer.
Main major mode will be `markdown-mode'.
Inlined html will be in `html-mode'.

You need `markdown-mode' which you can download from URL
`http://jblevins.org/projects/markdown-mode/'."
  ("Markdown HTML Family" markdown-mode
   (
    mumamo-chunk-markdown-html
    )))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Latex related

;;;###autoload
(define-mumamo-multi-major-mode latex-clojure-mumamo-mode
  "Turn on multi major mode latex+clojure.
Main major mode will be `latex-mode'.
Subchunks will be in `clojure-mode'.

You will need `clojure-mode' which you can download from URL
`http://github.com/jochu/clojure-mode/tree'."
  ("Latex+clojur Family" latex-mode
   (
    mumamo-latex-closure-chunk
    )))

;;;###autoload
(define-mumamo-multi-major-mode latex-haskell-mumamo-mode
  "Turn on multi major mode latex+haskell.
Main major mode will be `latex-mode'.
Subchunks will be in `haskell-mode'.

You will need `haskell-mode' which you can download from URL
`http://projects.haskell.org/haskellmode-emacs/'."
  ("Latex+haskell Family" latex-mode
   (
    mumamo-latex-haskell-chunk
    )))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Python + ReST

;;;###autoload
(define-mumamo-multi-major-mode python-rst-mumamo-mode
  "Turn on multiple major modes for Python with RestructuredText docstrings."
  ("Python ReST Family" python-mode
   (
    mumamo-python-rst-long-string-chunk
    )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Amrita

;;;###autoload
(define-mumamo-multi-major-mode amrita-mumamo-mode
  "Turn on multiple major modes for Amrita.
Fix-me: This does not yet take care of inner chunks."
  ("Amrita Family" amrita-mode
   (
    mumamo-chunk-amrita-fold
    )))

(provide 'mumamo-fun)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mumamo-fun.el ends here
