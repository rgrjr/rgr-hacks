;;;****************************************************************************
;;;
;;;    Very random hackery.  Most of this is oriented toward doing emacs system
;;; maintenance tasks.
;;;
;;;    Do (setq debug-on-error t) to debug this stuff.  Use
;;; (read-key-sequence "Testing? ") to discover the arcane encodings used for
;;; non-ASCII character in FSF emacs 19.  Also, doing
;;;
;;;	(let ((e (read-event))) (cons (event-modifiers e) (event-basic-type e)))
;;;
;;; will give you the raw events and their symbolic decoding.  There is
;;; apparently no way provided to encode these symbolic characters into what
;;; read-event returns (but see ./encode-key.el).  Use (e.g.) [?\C-\M-%] in
;;; define-key calls.
;;;
;;;    To compile this without error, do the following:
;;;
;;;	(mapcar 'require '(autoload chistory))
;;;
;;;    Modification history:
;;;
;;; rgr-update-autoloads: 19.31 updated.  -- rgr, 20-Jul-96.
;;; rgr-command-history-hook: new, not working well.  -- rgr, 12-Aug-96.
;;; psa-status: remodularize to handle different servers.  -- rgr, 6-Mar-98.
;;; psa-status: (&optional test-p).  -- rgr, 6-May-98.
;;; rgr-update-public-autoloads: new command.  -- rgr, 23-Mar-98.
;;; rgr-update-autoloads: update for emacs 20.  -- rgr, 11-Aug-99.
;;; rgr-update-dist-autoloads: new.  -- rgr, 12-Aug-99.
;;; rgr-list-files-needing-compile: new.  -- rgr, 20-Dec-99.
;;; rgr-update-directory-autoloads: split out internal fn.  -- rgr, 21-Mar-00.
;;; psa-status-internal: do server-running check only at BU.  -- rgr, 16-Jun-00.
;;; new rgr-batch-update-autoloads, minor updating.  -- rgr, 21-Dec-00.
;;; psa-get-server-pid: make work when "." is not on PATH.  -- rgr, 23-Jan-01.
;;;

;;;; Autoload-generating commands.

(defvar rgr-hacks-public-autoloads
	(expand-file-name "~rogers/emacs/rgr-public-hacks.el"))
(defvar rgr-hacks-public-files
	'("bmerc-hacks.el"
	  "rgr-html-hacks.el" "rgr-html-head.el" "rgr-html-nest.el"
	  "rgr-html-boilerplate.el" "rgr-html-tags.el")
  "Files in the same directory as rgr-hacks-public-autoloads that are
considered public.  rgr-update-public-autoloads puts their autoload
forms there.")

;;;###autoload
(defun rgr-update-public-autoloads ()
  ;; Update rgr-hacks-public-autoloads with all the current autoloads in the
  ;; rgr-hacks-public-files set, and no old ones.  [based on the
  ;; update-autoloads-from-directory function (19.31 version), which it can't
  ;; call because it needs just a subset of files.  -- rgr, 23-Mar-98.]
  ;; [updated slightly to match 20.7 (and 20.4) version.  -- rgr, 21-Dec-00.]
  (interactive) 
  (require 'autoload)
  (let ((files (append rgr-hacks-public-files nil))
	(autoloads-file rgr-hacks-public-autoloads)
	(dir (file-name-directory rgr-hacks-public-autoloads)))
    (save-excursion
      (set-buffer (find-file-noselect autoloads-file))
      (save-excursion
	(goto-char (point-min))
	(while (search-forward generate-autoload-section-header nil t)
	  (let* ((form (autoload-read-section-header))
		 (file (nth 3 form)))
	    (cond ((not (stringp file)))
		  ((not (file-exists-p (expand-file-name file dir)))
		    ;; Remove the obsolete section.
		    (let ((begin (match-beginning 0)))
		      (search-forward generate-autoload-section-trailer)
		      (delete-region begin (point))))
		  (t
		    (update-file-autoloads file)))
	    (setq files (delete file files)))))
      ;; Elements remaining in FILES have no existing autoload sections.
      (mapcar 'update-file-autoloads files)
      (save-buffer))))

(defun rgr-update-directory-autoloads (directory &optional file)
  ;; internal grinder for the rgr-update-autoloads and
  ;; rgr-batch-update-autoloads fns.  -- rgr, 21-Dec-00.
  (require 'autoload)
  (let* ((dir (expand-file-name directory))
	 (generated-autoload-file
	   (expand-file-name (or file "loaddefs.el") dir)))
    (funcall (cond ((fboundp 'update-autoloads-from-directories)
		     ;; emacs 20 version (takes &rest directories).
		     'update-autoloads-from-directories)
		   ((fboundp 'update-autoloads-from-directory)
		     ;; emacs 19.31+ version (takes a single directory).
		     'update-autoloads-from-directory)
		   (t
		     ;; called update-directory-autoloads prior to 19.31.  --
		     ;; rgr, 20-Jul-96.]
		     'update-directory-autoloads))
	     dir)))

;;;###autoload
(defun rgr-update-autoloads ()
  "Update the ~/emacs/rgr-hacks-autoloads.el file with the latest
autoloads from this directory."
  (interactive)
  (rgr-update-directory-autoloads "~/emacs" "rgr-hacks-autoloads.el"))

;;;###autoload
(defun rgr-batch-update-autoloads ()
  "Calls rgr-update-directory-autoloads on command-line-args-left."
  (apply 'rgr-update-directory-autoloads command-line-args-left)
  (setq command-line-args-left nil))

;; (expand-file-name "autoloads.el" (expand-file-name "~/emacs"))
;; (rgr-update-directory-autoloads "~/emacs/discus")

;;;###autoload
(defun rgr-update-dist-autoloads ()
  "Update the ~/emacs/dist/emacs/loaddefs.el file with the latest
autoloads from this directory."
  (interactive)
  (require 'autoload)
  (let* ((default-directory (expand-file-name "~/emacs/dist/emacs/"))
	 (generated-autoload-file (expand-file-name "loaddefs.el")))
    (funcall (if (fboundp 'update-autoloads-from-directories)
		 ;; emacs 20 version (takes &rest directories)
		 'update-autoloads-from-directories
		 ;; emacs 19 version (takes a single directory).  [this was
		 ;; called update-directory-autoloads prior to 19.31.  -- rgr,
		 ;; 20-Jul-96.]
		 'update-autoloads-from-directory)
	     default-directory)))

;;;; Byte compilation.

;;;###autoload
(defun rgr-list-files-needing-compile (directory &optional arg)
  ;; based on byte-recompile-directory
  "Recompile every `.el' file in DIRECTORY that needs recompilation.
This is if a `.elc' file exists but is older than the `.el' file.
Files in subdirectories of DIRECTORY are processed also.

If the `.elc' file does not exist, normally the `.el' file is *not*
listed as needing to be compiled.  But a prefix argument (optional
second arg) means list it anyway.

A nonzero prefix argument also means ask about each subdirectory."
  (interactive "DList files needing recompile in directory: \nP")
  (if arg
      (setq arg (prefix-numeric-value arg)))
  (if noninteractive
      nil
    (save-some-buffers)
    (force-mode-line-update))
  (let ((directories (list (expand-file-name directory)))
	(file-count 0)
	(dir-count 0)
	last-dir)
    (with-output-to-temp-buffer "*elisp files*"
     (while directories
       (setq directory (car directories))
       (message "Checking %s..." directory)
       (let ((files (directory-files directory))
	     source dest)
	 (while files
	   (setq source (expand-file-name (car files) directory))
	   (cond ((member (car files) '("." ".." "RCS" "CVS")))
		 ((and (file-directory-p source)
		       (not (file-symlink-p source)))
		   ;; This file is a subdirectory.  Handle them differently.
		   (when (or (null arg)
			     (eq 0 arg)
			     (y-or-n-p (concat "Check " source "? ")))
		     (setq directories
			   (nconc directories (list source)))))
		 ;; It is an ordinary file.  Decide whether to compile it.
		 ((not (and (string-match emacs-lisp-file-regexp source)
			    (not (auto-save-file-name-p source))))
		   ;; not an emacs lisp file.
		   )
		 ((progn (setq dest (byte-compile-dest-file source))
			 (not (file-exists-p dest)))
		   (if arg
		       (princ (format "%s is not compiled.\n" source))))
		 ;; File was already compiled.
		 ((file-newer-than-file-p source dest)
		   (princ (format "%s needs to be compiled.\n" source))
		   (setq file-count (1+ file-count))
		   (if (not (eq last-dir directory))
		       (setq last-dir directory
			     dir-count (1+ dir-count)))))
	   (setq files (cdr files))))
       (setq directories (cdr directories))))
    (message "Done (total of %d file%s need to be compiled%s)."
	     file-count (if (= file-count 1) "" "s")
	     (if (> dir-count 1) (format " in %d directories" dir-count) ""))))

;;;; Server status commands.

;; [this is identical to the ctserv-get-server-pid function in
;; /usr/local/lib/emacs/site-lisp/ctserv-request.el, and is duplicated in order
;; to avoid tangled modularity.  -- rgr, 6-Mar-98.]  [that function is now
;; broken, because it doesn't have the fix i just made to this one to get around
;; the fact that "." is no longer on $PATH.  -- rgr, 23-Jan-01.]
(defun psa-get-server-pid (server-pathname &optional no-message-p)
  "Return the process ID of the server indicated by server-pathname.
This will be -1 if the server is not operating.  An error is signalled
if (e.g.) the pathname does not indicate a server."
  (let ((buffer (get-buffer-create " psa get-server-pid"))
	(psa-bin-dir (expand-file-name "~psa/bin/"))
	(old-path (getenv "PATH")))
    (unwind-protect
	 (save-excursion
	   (set-buffer buffer)
	   (or no-message-p
	       (message "Checking whether the server is running . . ."))
	   (setenv "PATH" (concat psa-bin-dir ":" old-path))
	   (call-process (expand-file-name "get-server-pid" psa-bin-dir)
			 nil t nil server-pathname)
	   (goto-char (point-min))
	   (let ((pid (read buffer)))
	     (or (numberp pid)
		 (error "Error in get-server-pid: '%s'"
			(buffer-substring (point-min)
					  (progn (end-of-line)
						 (point)))))
	     (or no-message-p
		 (message "Checking whether the server is running . . . done."))
	     pid))
      (setenv "PATH" old-path)
      (kill-buffer buffer))))

(defun psa-status-internal (server-pathname)
  ;; helper for the psa-status command.
  (let ((server-name (file-name-nondirectory server-pathname))
	(buffer nil))
    (and (string-match "\\.bu\\.edu$" (system-name))
	 (let ((pid (psa-get-server-pid server-pathname)))
	   (if (= pid -1)
	       (error "The %s server seems to have died." server-name))))
    ;; Server running; send it something to chew on.
    (require 'sendmail)
    (unwind-protect
	 (save-excursion
	   (setq buffer (get-buffer-create (concat " " server-name " status")))
	   (set-buffer buffer)
	   (insert "To: " server-name "@darwin.bu.edu\n"
		   "Subject: $$status$$\n"
		   mail-header-separator "\n"
		   "Generated automatically by the psa-status emacs command.\n")
	   ;; mail it.  based on what mail-send does.
	   (funcall send-mail-function)
	   (message "Sent status request to %s; %s"
		    server-name "check your mail in a few minutes."))
      (and buffer (kill-buffer buffer)))))

;;;###autoload
(defun psa-status (&optional test-p)
  "Request the status of the psa server via email."
  (interactive "P")
  (psa-status-internal
    (expand-file-name (if test-p "~psa/psa-test" "~psa/psa-request"))))

;;;###autoload
(defun ctserv-status ()
  "Request the status of the `ctserv' server."
  (interactive)
  (ctserv-request "server-status; status-all"))

;;;; Other

(defun rgr-command-history-hook ()
  ;; Unfortunately this doesn't work right; the hook function doesn't get called
  ;; until after the history buffer is built.  -- rgr, 12-Aug-96.
  (or (member 'switch-to-buffer default-command-history-filter-garbage)
      (setq default-command-history-filter-garbage
	    (cons 'switch-to-buffer default-command-history-filter-garbage))))

;;;; Done.

(provide 'rgr-random-hacks)

