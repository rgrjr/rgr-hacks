;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; Compile rgr-hacks code.
;;;
;;; [created.  -- rgr, 23-Apr-03.]
;;;

(require 'cl-lib)

(defvar rgr-hacks-compiled-modules nil
  "Alist of (name-stem-string . mod-time) of loaded versions.  Modules
absent from this list may still have been loaded, just not by
rgr-hacks-compile-module (see below).")
(defvar rgr-hacks-compile-self-debug-p nil
  "Internal flag for debugging compiles.")
(defvar rgr-hacks-compile-self-n-files-compiled 0
  "Count of files compiled this time; not valid globally.")

;; Set up to load some optional packages if we can.
(if (file-directory-p "/usr/share/emacs/site-lisp/vm")
    ;; This is the emacs-vm RPM location.
    (add-to-list 'load-path "/usr/share/emacs/site-lisp/vm"))
(let ((rgr-imported-packages (expand-file-name "../imported")))
  (if (and (file-directory-p rgr-imported-packages)
	   (not (member rgr-imported-packages load-path)))
      (let ((autoloads (expand-file-name "loaddefs.el" rgr-imported-packages)))
	(setq load-path (cons rgr-imported-packages load-path))
	(if (file-readable-p autoloads)
	    (load autoloads))
	t)))
(dolist (dir '("/shared/emacs/site-lisp" "/usr/local/src/emacs/site-lisp"))
  (cond ((file-directory-p dir)
	  ;; This makes it possible to find html-helper-mode at home vs. work.
	  (setq load-path (cons dir load-path))
	  (load (expand-file-name "site-start.el" dir)))))

(defvar rgr-hacks-source-files
	'(;; [this comes first because it has some emacs version stuff.
	  ;; -- rgr, 23-Apr-03.]
	  "rgr-hacks"
	  "rgr-term-setup"
	  ;; orphaned features.
	  ("ilisp-possibilities" require (diff-mode))
	  "ilisp-mouse"
	  ;; [provides the rgr-update-autoloads command.  -- rgr, 26-Apr-03.]
	  ("rgr-random-hacks" require (autoload ffap chistory))
	  "rgr-bio-hacks"
	  "bagels"
	  "rgr-backup-hacks"
	  "rgr-date"
	  "rgr-c-hacks"
	  "rgr-abbrev-completion"
	  "tags-grep"
	  "rgr-compile-hacks"
	  ("rgr-cvs-hacks" version 23)
	  ("rgr-new-vc-hacks" version 23)
	  "rgr-diff-hacks"
	  "rgr-dired"
	  "rgr-elisp-hacks"
	  "rgr-enumerate"
	  "rgr-ffap"
	  "rgr-fill-comment"
	  "rgr-genbank"
	  ;; "rgr-geometry"
	  ("rgr-html-hacks" require (html-helper-mode))
	  ("rgr-html-head" require (html-helper-mode))
	  ("rgr-html-nest" require (html-helper-mode))
	  "rgr-intervals"
	  "rgr-lisp-hacks"
	  ("rgr-mail-hacks" require (vm))
	  "rgr-snoop-maildir"
	  "rgr-makefile-hacks"
	  ("rgr-mouse" use (tex-mode) require (ilisp-mouse browse-url))
	  "rgr-mouse-doc"
	  "rgr-perl-hacks"
	  ("rgr-ruby-hacks" require (ruby-mode))
	  ("rgr-python-hacks" require (python))
	  "rgr-rect-hacks"
	  "rgr-log-view"
	  "rgr-shell-hacks"
	  "rgr-squid-log"
	  ("rgr-vm-hacks" require (vm))
	  "rgr-x11-hacks"
	  ("vm+qmail" require (vm)))
  "List of source file specs, which can be either the file stem, or a
pair of (file-stem . properties), where properties is a disembodied plist.")

(defun rgr-check-module-requirements (module-name options)
  ;; Check for requirements, returning true and printing message(s) if we must
  ;; skip this module.  Note that we always let all checks run, even if we get
  ;; an early failure, so that all "must skip" messages are shown.
  (let ((skipped-p nil)
	(version (cl-getf options 'version)))
    (cond ((and version (< emacs-major-version version))
	    (setq skipped-p 'version)
	    (message "Module %s skipped because it requires Emacs version %s."
			 module-name version)))
    (let ((tail (cl-getf options 'require)))
      (while tail
	(cond ((condition-case error (not (require (car tail)))
		 (error
		  ;; (message "Error loading %S:  %S" (car tail) error)
		  t))
		(setq skipped-p (car tail))
	        (message "Module %s skipped because '%s' could not be loaded."
			 module-name skipped-p)))
	(setq tail (cdr tail))))
    skipped-p))

(defun rgr-hacks-compile-module (name &rest options)
  ;; Compile and load the file if it needs it, returning t iff we decided to
  ;; compile.  -- rgr, 21-Sep-02.
  (let* ((force-p (cl-getf options 'force-p))
	 (must-load-p (cl-getf options 'load-p t))
	 (name-stem (if (symbolp name) (symbol-name name) name))
	 (source-name (concat name-stem ".el"))
	 (binary-name (concat source-name "c"))
	 (bin-date nil)
	 (skipped-p (rgr-check-module-requirements name options))
	 (compiled-p nil)
	 (need-to-load-p nil))

    ;; Compile if necessary.
    (cond (skipped-p)
	  ((not (file-exists-p source-name))
	    (message "Can't find %s -- not compiling." source-name))
	  ((not (or force-p
		    (not (file-exists-p binary-name))
		    (file-newer-than-file-p source-name binary-name))))
	  ((not (eval (cl-getf options 'if t)))
	    (setq skipped-p 'if)
	    (message "File %S skipped because of 'if' test." source-name))
	  (t
	    (setq compiled-p t need-to-load-p must-load-p)
	    (cond (rgr-hacks-compile-self-debug-p
		    (message "Need to compile %s" source-name))
		  (t
		    (message "Compiling %s ..." source-name)
		    (byte-compile-file source-name)))
	    (setq rgr-hacks-compile-self-n-files-compiled
		  (1+ rgr-hacks-compile-self-n-files-compiled))))

    ;; Check for newer binary on disk.
    (if (and (not skipped-p) must-load-p (not need-to-load-p))
	(let ((entry (assoc name-stem rgr-hacks-compiled-modules)))
	  (setq bin-date (nth 5 (file-attributes binary-name)))
	  (if bin-date
	      (setq need-to-load-p
		    (or (null entry)
			(not (equal bin-date (cdr entry)))))
	      ;; (error "No binary '%s'; can't load." binary-name)
	      )))

    ;; Load if necessary.
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
    (message "Emacs version %d.%d" emacs-major-version emacs-minor-version)
    (let ((tail rgr-hacks-source-files))
      (while tail
	(let* ((f (car tail))
	       (name (if (consp f) (car f) f))
	       (attributes (if (consp f) (cdr f) nil))
	       (version (cl-getf attributes 'major-version))
	       (result (and (or (null version)
				(>= emacs-major-version version))
			    (apply (function rgr-hacks-compile-module)
				   name 'force-p force-p
				   attributes))))
	  (if (and result
		   (cl-getf attributes 'has-macros))
	      (setq force-p t)))
	(setq tail (cdr tail))))
    (message "rgr-hacks compilation: Done, %d file%s compiled."
	     rgr-hacks-compile-self-n-files-compiled
	     (if (= rgr-hacks-compile-self-n-files-compiled 1) "" "s"))))

