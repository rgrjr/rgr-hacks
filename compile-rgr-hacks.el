;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; Compile rgr-hacks code.
;;;
;;; $Id$
;;;
;;;    Modification history:
;;;
;;; created.  -- rgr, 23-Apr-03.
;;;

(defvar rgr-hacks-compiled-modules nil
  "Alist of (name-stem-string . mod-time) of loaded versions.  Modules
absent from this list may still have been loaded, just not by
rgr-hacks-compile-module (see below).")
(defvar rgr-hacks-compile-self-debug-p nil
  "Internal flag for debugging compiles.")
(defvar rgr-hacks-compile-self-n-files-compiled 0
  "Count of files compiled this time; not valid globally.")

(defvar rgr-hacks-source-files
	'(;; [this comes first because it has some emacs version stuff.
	  ;; -- rgr, 23-Apr-03.]
	  "rgr-hacks"
	  ;; [provides the rgr-update-autoloads command.  -- rgr, 26-Apr-03.]
	  ("rgr-random-hacks" require (autoload chistory))
	  "ange-ftp-hacks"
	  "bagels"
	  "c-hacks"
	  ;; this requires that the PSA software be installed.  [oops; can't
	  ;; even compile this.  -- rgr, 23-Apr-03.
	  ("psa-support" require (psa-server))
	  "rgr-abbrev-completion"
	  ;; this requires that ACL be installed, and we don't want to test for
	  ;; its presence by loading it, because that's what the rgr-allegro.el
	  ;; file is supposed to do.  -- rgr, 23-Apr-03.
	  ("rgr-allegro" load-p nil)
	  "rgr-bib"
	  ;; [should try to remove this dependency.  -- rgr, 26-Apr-03.]
	  ("rgr-check-mail" require (discus-date))
	  "rgr-compile-hacks"
	  "rgr-dired"
	  "rgr-elisp-hacks"
	  "rgr-fill-comment"
	  ;; "rgr-geometry"
	  "rgr-home-hacks"
	  "rgr-html-boilerplate"
	  "rgr-html-hacks"
	  "rgr-html-head"
	  "rgr-html-nest"
	  "rgr-html-random"
	  "rgr-html-tags"
	  "rgr-intervals"
	  "rgr-lisp-hacks"
	  "rgr-mail-hacks"
	  "rgr-makefile-hacks"
	  ("rgr-mouse-19"
	   if (member rgr-emacs-major-version '(19 20))
	   require (ilisp-mouse))
	  ("rgr-mouse-20" if (eq rgr-emacs-major-version 20))
	  ("rgr-mouse-21"
	   if (eq rgr-emacs-major-version 21)
	   require (ilisp-mouse))
	  ("rgr-mouse" use (ilisp-mouse tex-mode) require (browse-url))
	  "rgr-perl-hacks"
	  ;; [not finished.  -- rgr, 23-Apr-03.]
	  ;; "rgr-rect-hacks"
	  "rgr-rmail-hacks"
	  "rgr-shell-hacks"
	  ;; [should try to remove this dependency.  -- rgr, 26-Apr-03.]
	  ("rgr-squid-log" require (discus-date))
	  "rgr-tcl"
	  "rgr-tripwire-minus-rpm"
	  ("rgr-w3-hacks" require (w3))
	  "rgr-x11-hacks"
	  ("vm+qmail" require (vm)))
  "List of source file specs, which can be either the file stem, or a
pair of (file-stem . properties), where properties is a disembodied plist.")

(defun rgr-hacks-compile-module (name &rest options)
  ;; Compile and load the file if it needs it, returning t iff we decided to
  ;; compile.  -- rgr, 21-Sep-02.
  (let* ((force-p (rgr-hacks-getf options 'force-p))
	 (must-load-p (rgr-hacks-getf options 'load-p t))
	 (requirements (rgr-hacks-getf options 'require))
	 (name-stem (if (symbolp name) (symbol-name name) name))
	 (source-name (concat name-stem ".el"))
	 (binary-name (concat source-name "c"))
	 (bin-date nil)
	 (skipped-p nil)
	 (compiled-p nil)
	 (need-to-load-p nil))
    (cond ((not (file-exists-p source-name))
	    (message "Can't find %s -- not compiling." source-name))
	  ((not (or force-p
		    (not (file-exists-p binary-name))
		    (file-newer-than-file-p source-name binary-name))))
	  ((not (eval (rgr-hacks-getf options 'if t)))
	    (setq skipped-p 'if)
	    (message "File %S skipped because of 'if' test." source-name))
	  ((let ((tail requirements))
	     (while tail
	       (if (require (car tail) nil t)
		   (setq tail (cdr tail))
		   (setq skipped-p (car tail) tail nil)))
	     skipped-p)
	    (message "File %S skipped because '%s' could not be loaded."
		     source-name skipped-p))
	  (t
	    (setq compiled-p t need-to-load-p must-load-p)
	    (if rgr-hacks-compile-self-debug-p
		(message "Need to compile %s" source-name)
		(byte-compile-file source-name))
	    (setq rgr-hacks-compile-self-n-files-compiled
		  (1+ rgr-hacks-compile-self-n-files-compiled))))
    (if skipped-p
	(setq must-load-p nil))
    (if (and must-load-p (not need-to-load-p))
	;; Check for newer binary on disk.
	(let ((entry (assoc name-stem rgr-hacks-compiled-modules)))
	  (setq bin-date (nth 5 (file-attributes binary-name)))
	  (if bin-date
	      (setq need-to-load-p
		    (or (null entry)
			(not (equal bin-date (cdr entry)))))
	      ;; (error "No binary '%s'; can't load." binary-name)
	      )))
    (cond ((not need-to-load-p))
	  (rgr-hacks-compile-self-debug-p
	    (message "Need to load %s" binary-name))
	  (t
	    (load binary-name)
	    (setq rgr-hacks-compiled-modules
		  (cons (cons name-stem
			      (or bin-date
				  (nth 5 (file-attributes binary-name))))
			rgr-hacks-compiled-modules))))
    compiled-p))

(defun rgr-hacks-compile-self ()
  "Compile and load rgr-hacks emacs-lisp binaries that are out of date."
  (interactive)
  (let ((force-p nil)
	(load-path (cons "." load-path))
	(rgr-hacks-compile-self-n-files-compiled 0))
    (message "rgr-hacks compilation: starting.")
    (or (boundp 'rgr-emacs-major-version)
	;; Need to load this beforehand to get rgr-emacs-flavor,
	;; rgr-emacs-*-version, etc.
	(load "rgr-hacks.el"))
    (message "Emacs flavor %s, version %d.%d"
	     rgr-emacs-flavor rgr-emacs-major-version rgr-emacs-minor-version)
    (let ((tail rgr-hacks-source-files))
      (while tail
	(let* ((f (car tail))
	       (name (if (consp f) (car f) f))
	       (attributes (if (consp f) (cdr f) nil))
	       (flavors (rgr-hacks-getf attributes 'flavors))
	       (result (and (if (null flavors)
				t
				(member rgr-emacs-flavor flavors))
			    (apply (function rgr-hacks-compile-module)
				   name 'force-p force-p
				   attributes))))
	  (if (and result
		   (rgr-hacks-getf attributes 'has-macros))
	      (setq force-p t)))
	(setq tail (cdr tail))))
    (message "rgr-hacks compilation: Done, %d file%s compiled."
	     rgr-hacks-compile-self-n-files-compiled
	     (if (= rgr-hacks-compile-self-n-files-compiled 1) "" "s"))))

