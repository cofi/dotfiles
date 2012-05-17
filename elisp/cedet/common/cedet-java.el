;;; cedet-java.el --- Support functions on top of Java's JDK.
;;
;; Copyright (C) 2011, 2012 Eric M. Ludlam
;;
;; Author: Eric M. Ludlam <eric@siege-engine.com>
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Java support utilities for CEDET.

;;; Code:
(require 'inversion)

(defvar cedet-java-min-version "1.4"
  "Minimum version of the java JDR.")

(defcustom cedet-java-command "java"
  "The command used for running Java."
  :group 'java
  :type 'string)

(defcustom cedet-jar-command "jar"
  "The command used for running Java jar command."
  :group 'java
  :type 'string)

(defcustom cedet-javap-command "javap"
  "The command used for running Java's javap command."
  :group 'java
  :type 'string)

(defcustom cedet-java-classpath-extension nil
  "List of extended classpath directories and Jar files to pass to java commands."
  :group 'java
  :type '(repeat string))

(defcustom cedet-java-version-regexp "java version \"\\([0-9._]+\\)\""
  "Regual expression used to parse java -version for version number"
  :group 'java
  :type 'string)

;;; Java command Support
;;
(defun cedet-java-call (flags)
  "Call java with the list of FLAGS."
  (let ((b (get-buffer-create "*Java Output*"))
	(cd default-directory)
	)
    (with-current-buffer b
      (setq default-directory cd)
      (erase-buffer))
    (when cedet-java-classpath-extension
      (setq flags (cons "-classpath"
			(cons (mapconcat 'identity cedet-java-classpath-extension ":")
			      flags))))
    (apply 'call-process cedet-java-command
	   nil b nil flags)
    b))

;;;###autoload
(defun cedet-java-version-check (&optional noerror)
  "Check the version of the installed java command.
If optional programatic argument NOERROR is non-nil, then
instead of throwing an error if Global isn't available, then
return nil."
  (interactive)
  (let ((rev (cedet-java-get-version)))
    (if (inversion-check-version rev nil cedet-java-min-version)
	(if noerror
	    nil
	  (error "Version of Java is %s.  Need at least %s"
		 rev cedet-java-min-version))
      ;; Else, return TRUE, as in good enough.
      (when (cedet-called-interactively-p 'interactive)
	(message "Java %s  - Good enough." rev))
      t)))

(defun cedet-java-get-version ()
  "Return the version string from executing the java command.
Parses the java string with `cedet-java-version-regexp'."
  (let ((b (condition-case nil
	       (cedet-java-call (list "-version"))
	     (error nil)))
	(rev nil))
    (if (not b)
	(progn
	  (when (cedet-called-interactively-p 'interactive)
	    (message "java not found."))
	  nil)
      (with-current-buffer b
	(goto-char (point-min))
	(re-search-forward cedet-java-version-regexp nil t)
	(setq rev (match-string 1))))
    rev))

;;; Java "jar" command Support
;;
(defun cedet-jar-table-of-contents (jarfile)
  "Extract the table of contents from JARFILE.
Return the contents as a list of paths to files.
Exclude empty directories."
  (let* ((b (cedet-jar-call (list "-tf" jarfile)))
	 (strs (split-string
		(save-current-buffer
		  (set-buffer b) (buffer-string))
		"\n" t))
	 (ans nil))
    (dolist (C strs)
      (when (string-match "\\.class$" C)
	(push C ans)))
    (nreverse ans)))

(defun cedet-jar-call (flags)
  "Call java's jar command with the list of FLAGS."
  (let ((b (get-buffer-create "*Jar Output*"))
	(cd default-directory)
	)
    (with-current-buffer b
      (setq default-directory cd)
      (erase-buffer))
    (apply 'call-process cedet-jar-command
	   nil b nil flags)
    b))

;;; Javap command Support
;;
(defun cedet-javap-call (flags)
  "Call javap with the list of FLAGS."
  (let ((b (get-buffer-create "*javap output*"))
	(cd default-directory)
	)
    (with-current-buffer b
      (setq default-directory cd)
      (erase-buffer))
    (when cedet-java-classpath-extension
      (setq flags (cons "-classpath"
			(cons (mapconcat 'identity cedet-java-classpath-extension ":")
			      flags))))
    (apply 'call-process cedet-javap-command
	   nil b nil
	   flags)
    b))

(defun cedet-javap-get-class (jar class)
  "In JAR, get a javap dump of CLASS, return the buffer."
  (let ((cedet-java-classpath-extension
	 (if jar (list jar) cedet-java-classpath-extension)))
    (cedet-javap-call
     (if cedet-java-classpath-extension
	 (list "-protected" "-bootclasspath" "" class)
       (list "-protected" class)
       ))))

;;;###autoload
(defun cedet-javap-dump-class (class)
  "Dump out a Java signatures for CLASS.
Display in a javap output buffer."
  (interactive "sClass: ")
  (switch-to-buffer (cedet-javap-get-class nil class)))


;;; JAVA Platform Specific Hacks/Detection
;;
;; Thanks Joel Borggren-Franck for the data needed for the first pass
;;
;; He said:
;; > But here is a list to get you started looking:
;; > - Oracle supplied bundle: JAVA_HOME/jre/lib/rt.jar
;; > - Mac OS X JDK 6:
;; > /Library/Java/JavaVirtualMachines/1.6.0_29-b11-402.jdk/Contents/Classes/classes.jar
;; > - Max OS X Oracle JDK 7 Preview: /Library/Java/JavaVirtualMachines/JDK
;; > 1.7.0 Developer Preview.jdk/Contents/Home/jre/lib/rt.jar
;; > - On Windows JAVA_HOME is usually a variation of C:\Program Files\Java ...
(defcustom cedet-java-jdk-root nil
  "Specify the root location of your JDK.
If nil, the root will be derived as needed.  Specify this variable if
the JDK cannot be found.
Examples:
 MACOS: /Library/Java/JavaVirtualMachines/1.6.0_29-b11-402.jdk/
 Windows: C:\Program Files\Java\
 Linux: /usr/local/lib/jvm/java-6-openjdk/"
  :group 'java
  :type 'string)

(defun cedet-java-find-jdk ()
  "Return the location of the JDK core .jar file."
  (let ((javahome (getenv "JAVA_HOME"))
	)
    (cond

     ;; User specified.
     (cedet-java-jdk-root
      (expand-file-name "jre/lib/rt.jar" cedet-java-jdk-root))

     ;; WITH ORACLE Environment Variable
     (javahome
      (expand-file-name "jre/lib/rt.jar" javahome))

     ;; MACOS
     ((eq system-type 'darwin)
      (let* ((libtmp "/Library/Java/JavaVirtualMachines/")
	     (libtmpb  (expand-file-name "JDK" libtmp))
	     (lib (if (file-exists-p libtmpb) libtmpb libtmp))
	     (file "Contents/Classes/classes.jar")
	     (dirf (directory-files lib nil ".jdk" nil)))
	;; The first match in dirf is the oldest (alpha sort = oldest # first)
	(expand-file-name file (expand-file-name (car (last dirf)) lib))))
     
     ;; WINDOWS
     ((eq system-type 'windows-nt)
      (let* ((lib "c:/program files/java/jre/lib/")
	     (file "rt.jar"))
	;; The first match in dirf is the oldest (alpha sort = oldest # first)
	(expand-file-name file lib)))
      
     ;; Misc Unix (tested on Linux)
     ;; /usr/lib/jvm/java-6-sun/jre/lib/rt.jar
     ;; /usr/lib/jvm/java-6-openjdk/jre/lib/rt.jar
     (t
      (let ((basedirs '("/usr/lib/jvm" "/usr/local/lib/jvm"))
	    (dflt "default-java")
	    (jdks '("sun" "openjdk" "gcj"))
	    dirs
	    (file "jre/lib/rt.jar")
	    base jdk)
	;; Find base dir
	(while (and (not base) basedirs)
	  (when (file-exists-p (car basedirs))
	    (setq base (car basedirs))))
	(catch 'nojdk
	  ;; Find the right JDK to use.
	  (if (file-exists-p (expand-file-name dflt base))
	      (expand-file-name file (expand-file-name dflt base))
	    (throw 'nojdk nil))
	  ;; No magic default via OS.  Use directory-files.
	  (while (and jdks (not jdk))
	    ;; Should we call cedet-java-get-version, and use that in
	    ;; our query???	    
	    (setq dirs (directory-files base nil (car jdks) nil))
	    (when (and dirs (file-exists-p (expand-file-name (car (last dirs)) base)))
	      (setq jdk (car (last dirs)))))
	  (unless jdk (throw 'nojdk nil))
	  ;; Full expand
	  (expand-file-name file (expand-file-name jdk base)))))
     )))

(defun cedet-java-describe ()
  "Describe the discernable java environment."
  (interactive)

  (with-output-to-temp-buffer "*CEDET Java Environment*"

    (princ "CEDET Java Operational Environment:\n\n")
    (princ "Current Java Version: ")
    (princ (cedet-java-get-version))
    (princ "\n\nMinimum Desired version: ")
    (princ cedet-java-min-version)
    (princ "\nJava Command: ")
    (princ cedet-java-command)
    (princ "\nJar Command: ")
    (princ cedet-jar-command)
    (princ "\nJavap Command: ")
    (princ cedet-javap-command)

    ;; CLASSPATH
    (princ "\n\nUser Specified Global Classpath Extension: \n")
    (dolist (P cedet-java-classpath-extension)
      (princ "  >  ")
      (princ P)
      (princ "\n"))

    ;; EDE
    (when (and (or (featurep 'ede) (featurep 'cedet/ede))
	       (ede-current-project))
      (let ((jcp (ede-java-classpath (ede-current-project))))
	(princ "\n\nProject Specified Classpath Extension (EDE): \n")
	(dolist (P jcp)
	  (princ "  >  ")
	  (princ P)
	  (princ "\n"))))

    ;; JDK home
    (princ "\n\nJava JDK core Jar file: \n  ")
    (let ((jdkhome (cedet-java-find-jdk)))
      (princ (or jdkhome "No JDK Found.
   Set cedet-java-jdk-root to a valid jdk root.\n")))
    ))

(provide 'cedet-java)

;;; cedet-java.el ends here
