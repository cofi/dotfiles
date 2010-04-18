<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml"><head><title>EmacsWiki: keychain-environment.el</title><link rel="alternate" type="application/wiki" title="Edit this page" href="http://www.emacswiki.org/emacs?action=edit;id=keychain-environment.el" /><link type="text/css" rel="stylesheet" href="/emacs/wiki.css" /><meta name="robots" content="INDEX,FOLLOW" /><link rel="alternate" type="application/rss+xml" title="EmacsWiki" href="http://www.emacswiki.org/emacs?action=rss" /><link rel="alternate" type="application/rss+xml" title="EmacsWiki: keychain-environment.el" href="http://www.emacswiki.org/emacs?action=rss;rcidonly=keychain-environment.el" />
<link rel="alternate" type="application/rss+xml"
      title="Emacs Wiki with page content"
      href="http://www.emacswiki.org/emacs/full.rss" />
<link rel="alternate" type="application/rss+xml"
      title="Emacs Wiki with page content and diff"
      href="http://www.emacswiki.org/emacs/full-diff.rss" />
<link rel="alternate" type="application/rss+xml"
      title="Emacs Wiki including minor differences"
      href="http://www.emacswiki.org/emacs/minor-edits.rss" />
<link rel="alternate" type="application/rss+xml"
      title="Changes for keychain-environment.el only"
      href="http://www.emacswiki.org/emacs?action=rss;rcidonly=keychain-environment.el" /><meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/></head><body class="http://www.emacswiki.org/emacs"><div class="header"><a class="logo" href="http://www.emacswiki.org/emacs/SiteMap"><img class="logo" src="/emacs_logo.png" alt="[Home]" /></a><span class="gotobar bar"><a class="local" href="http://www.emacswiki.org/emacs/SiteMap">SiteMap</a> <a class="local" href="http://www.emacswiki.org/emacs/Search">Search</a> <a class="local" href="http://www.emacswiki.org/emacs/ElispArea">ElispArea</a> <a class="local" href="http://www.emacswiki.org/emacs/HowTo">HowTo</a> <a class="local" href="http://www.emacswiki.org/emacs/RecentChanges">RecentChanges</a> <a class="local" href="http://www.emacswiki.org/emacs/News">News</a> <a class="local" href="http://www.emacswiki.org/emacs/Problems">Problems</a> <a class="local" href="http://www.emacswiki.org/emacs/Suggestions">Suggestions</a> </span>
<!-- Google CSE Search Box Begins  -->
<form class="tiny" action="http://www.google.com/cse" id="searchbox_004774160799092323420:6-ff2s0o6yi"><p>
<input type="hidden" name="cx" value="004774160799092323420:6-ff2s0o6yi" />
<input type="text" name="q" size="25" />
<input type="submit" name="sa" value="Search" />
</p></form>
<script type="text/javascript" src="http://www.google.com/coop/cse/brand?form=searchbox_004774160799092323420%3A6-ff2s0o6yi"></script>
<!-- Google CSE Search Box Ends -->
<br /><span class="specialdays">Zimbabwe, National Day</span><h1><a title="Click to search for references to this page" rel="nofollow" href="http://www.google.com/cse?cx=004774160799092323420:6-ff2s0o6yi&amp;q=%22keychain-environment.el%22">keychain-environment.el</a></h1></div><div class="wrapper"><div class="content browse"><p class="download"><a href="download/keychain-environment.el">Download</a></p><pre class="code"><span class="linecomment">;;; keychain-environment.el --- Loads keychain environment variables into emacs</span>
 
<span class="linecomment">;; Copyright (C) 2008,2009 Paul Tipper</span>
 
<span class="linecomment">;; Author:  Paul Tipper &lt;bluefoo at googlemail dot com&gt;</span>
<span class="linecomment">;; Keywords: keychain, ssh</span>
<span class="linecomment">;; Created: 18 Dec 2008</span>

<span class="linecomment">;; Version: 1.0.1</span>

<span class="linecomment">;; This file is not part of GNU Emacs.</span>
 
<span class="linecomment">;; This program is free software; you can redistribute it and/or modify</span>
<span class="linecomment">;; it under the terms of the GNU General Public License as published by</span>
<span class="linecomment">;; the Free Software Foundation, either version 3 of the License, or</span>
<span class="linecomment">;; (at your option) any later version.</span>
 
<span class="linecomment">;; This program is distributed in the hope that it will be useful,</span>
<span class="linecomment">;; but WITHOUT ANY WARRANTY; without even the implied warranty of</span>
<span class="linecomment">;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the</span>
<span class="linecomment">;; GNU General Public License for more details.</span>
 
<span class="linecomment">;; You should have received a copy of the GNU General Public License</span>
<span class="linecomment">;; along with this program.  If not, see &lt;http://www.gnu.org/licenses/&gt;.</span>

<span class="linecomment">;;; Commentary: </span>
<span class="linecomment">;; </span>
<span class="linecomment">;; Designed for use with Keychain, see:</span>
<span class="linecomment">;; (http://www.gentoo.org/proj/en/keychain/) a tool for loading the</span>
<span class="linecomment">;; SSH Agent and keeping it running and accessible on a machine for</span>
<span class="linecomment">;; longer than a single login seession.</span>
<span class="linecomment">;; </span>
<span class="linecomment">;; This library loads the file "$HOME/.keychain/$HOSTNAME-sh" and parses</span>
<span class="linecomment">;; it for the SSH_AUTH_SOCK and SSH_AUTH_PID variables, placing these into the</span>
<span class="linecomment">;; environment of Emacs.</span>
<span class="linecomment">;;</span>
<span class="linecomment">;; This is useful for situations where you are running Emacs under X, not</span>
<span class="linecomment">;; directly from a terminal, and its inheriting its environment from the</span>
<span class="linecomment">;; window manager, which doesn't have these variables as you started keychain</span>
<span class="linecomment">;; after you logged in (say as part of your .bashrc)</span>
<span class="linecomment">;;</span>
<span class="linecomment">;; The function (refresh-keychain-environment) can also be run at any time</span>
<span class="linecomment">;; these variables change.</span>

<span class="linecomment">;;; Installation:</span>
<span class="linecomment">;; Put the file in your load-path then use:</span>
<span class="linecomment">;; </span>
<span class="linecomment">;;   (require 'keychain-environment)</span>
<span class="linecomment">;;   (eval-after-load "keychain-environment" '(refresh-keychain-environment))</span>
<span class="linecomment">;;</span>
<span class="linecomment">;; If you want to customise the location of the keychain file then use this:</span>
<span class="linecomment">;;</span>
<span class="linecomment">;;   (setq keychain-environment-file "~/path-to-file")</span>
 
<span class="linecomment">;;; History:</span>
<span class="linecomment">;; 2008-12-18 Initial development.</span>
<span class="linecomment">;; 2009-02-25 Fixed bug with system-name being evaluated to the full hostname</span>

<span class="linecomment">;;; Code: </span>

(if (not (boundp 'keychain-environment-file))
    (defvar keychain-environment-file  (concat (getenv "<span class="quote">HOME</span>")
                                               "<span class="quote">/.keychain/</span>" 
                                               (car (split-string system-name 
                                                                  "<span class="quote">\\.</span>" 
                                                                  t))
                                               "<span class="quote">-sh</span>")
      "<span class="quote">Stores the location of the keychain file to load.  Normally
found in the '$HOME/.keychain' directory and called
'$HOSTNAME-sh'.</span>"))


<span class="linecomment">;; Really there should be an easier method of doing this surely?</span>
(if (not (fboundp 'read-file))
    (defun read-file (filename)
      "<span class="quote">Takes a filename, reads the data from it and returns it as a string</span>"
      
      (let* ((real-filename (expand-file-name filename))
             (visited (find-buffer-visiting real-filename))
             (orig-buffer (current-buffer))
             (buf (find-file-noselect real-filename))
             (data (save-excursion
                     (set-buffer buf)
                     (let ((data (buffer-substring-no-properties (point-min) 
                                                                 (point-max))))
                       (set-buffer orig-buffer)
                       data))))
        
        <span class="linecomment">;; Only kill the buffer if we didn't have a copy when we started</span>
        (if (null visited)
            (kill-buffer buf))
        
        <span class="linecomment">;; And return the data.</span>
        data)))

(defun refresh-keychain-environment ()
  "<span class="quote">Reads the keychain file for /bin/sh and sets the SSH_AUTH_SOCK
and SSH_AGENT_PID variables into the environment and returns them
as a list.</span>"
  (interactive)
  (let* ((data (read-file keychain-environment-file))
         (auth-sock (progn 
                      (string-match "<span class="quote">SSH_AUTH_SOCK=\\(.*?\\);</span>" data)
                      (match-string 1 data)))
         (auth-pid (progn
                     (string-match "<span class="quote">SSH_AGENT_PID=\\([0-9]*\\)?;</span>" data)
                     (match-string 1 data))))
    (setenv "<span class="quote">SSH_AUTH_SOCK</span>" auth-sock)
    (setenv "<span class="quote">SSH_AUTH_PID</span>" auth-pid)
    (list auth-sock auth-pid)))


(provide 'keychain-environment)</pre></div><div class="wrapper close"></div></div><div class="footer"><hr /><span class="gotobar bar"><a class="local" href="http://www.emacswiki.org/emacs/SiteMap">SiteMap</a> <a class="local" href="http://www.emacswiki.org/emacs/Search">Search</a> <a class="local" href="http://www.emacswiki.org/emacs/ElispArea">ElispArea</a> <a class="local" href="http://www.emacswiki.org/emacs/HowTo">HowTo</a> <a class="local" href="http://www.emacswiki.org/emacs/RecentChanges">RecentChanges</a> <a class="local" href="http://www.emacswiki.org/emacs/News">News</a> <a class="local" href="http://www.emacswiki.org/emacs/Problems">Problems</a> <a class="local" href="http://www.emacswiki.org/emacs/Suggestions">Suggestions</a> </span><span class="translation bar"><br />  <a class="translation new" rel="nofollow" href="http://www.emacswiki.org/emacs?action=translate;id=keychain-environment.el;missing=de_en_es_fr_it_ja_ko_pt_ru_se_zh">Add Translation</a></span><span class="edit bar"><br /> <a class="edit" accesskey="e" title="Click to edit this page" rel="nofollow" href="http://www.emacswiki.org/emacs?action=edit;id=keychain-environment.el">Edit this page</a> <a class="history" rel="nofollow" href="http://www.emacswiki.org/emacs?action=history;id=keychain-environment.el">View other revisions</a> <a class="admin" rel="nofollow" href="http://www.emacswiki.org/emacs?action=admin;id=keychain-environment.el">Administration</a></span><span class="time"><br /> Last edited 2009-02-25 11:07 UTC by <a class="author" title="from wwwcache2.lancs.ac.uk" href="http://www.emacswiki.org/emacs/PaulTipper">PaulTipper</a> <a class="diff" rel="nofollow" href="http://www.emacswiki.org/emacs?action=browse;diff=2;id=keychain-environment.el">(diff)</a></span><div style="float:right; margin-left:1ex;">
<!-- Creative Commons License -->
<a href="http://creativecommons.org/licenses/GPL/2.0/"><img alt="CC-GNU GPL" style="border:none" src="/pics/cc-GPL-a.png" /></a>
<!-- /Creative Commons License -->
</div>

<!--
<rdf:RDF xmlns="http://web.resource.org/cc/"
 xmlns:dc="http://purl.org/dc/elements/1.1/"
 xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">
<Work rdf:about="">
   <license rdf:resource="http://creativecommons.org/licenses/GPL/2.0/" />
  <dc:type rdf:resource="http://purl.org/dc/dcmitype/Software" />
</Work>

<License rdf:about="http://creativecommons.org/licenses/GPL/2.0/">
   <permits rdf:resource="http://web.resource.org/cc/Reproduction" />
   <permits rdf:resource="http://web.resource.org/cc/Distribution" />
   <requires rdf:resource="http://web.resource.org/cc/Notice" />
   <permits rdf:resource="http://web.resource.org/cc/DerivativeWorks" />
   <requires rdf:resource="http://web.resource.org/cc/ShareAlike" />
   <requires rdf:resource="http://web.resource.org/cc/SourceCode" />
</License>
</rdf:RDF>
-->

<p class="legal">
This work is licensed to you under version 2 of the
<a href="http://www.gnu.org/">GNU</a> <a href="/GPL">General Public License</a>.
Alternatively, you may choose to receive this work under any other
license that grants the right to use, copy, modify, and/or distribute
the work, as long as that license imposes the restriction that
derivative works have to grant the same rights and impose the same
restriction. For example, you may choose to receive this work under
the
<a href="http://www.gnu.org/">GNU</a>
<a href="/FDL">Free Documentation License</a>, the
<a href="http://creativecommons.org/">CreativeCommons</a>
<a href="http://creativecommons.org/licenses/sa/1.0/">ShareAlike</a>
License, the XEmacs manual license, or
<a href="/OLD">similar licenses</a>.
</p>
</div>
</body>
</html>
