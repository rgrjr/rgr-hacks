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

(eval-when-compile
  (require 'ffap))

(defvar rgr-emacs)

;;;; Autoload-generating commands.

(defun rgr-update-directory-autoloads (directory &optional file)
  ;; internal grinder for the rgr-update-autoloads and
  ;; rgr-batch-update-autoloads fns.  -- rgr, 21-Dec-00.
  (require 'autoload)
  (let* ((dir (expand-file-name directory))
	 (generated-autoload-file
	   (expand-file-name (or file "loaddefs.el") dir)))
    (update-directory-autoloads dir)))

;;;###autoload
(defun rgr-update-autoloads ()
  "Update the rgr-hacks-autoloads.el file with the latest autoloads
from this directory."
  (interactive)
  (rgr-update-directory-autoloads rgr-emacs "rgr-hacks-autoloads.el")
  (let ((imported (expand-file-name "../imported" rgr-emacs)))
    (if (file-directory-p imported)
	(rgr-update-directory-autoloads imported))))

;;;###autoload
(defun rgr-batch-update-autoloads ()
  "Calls rgr-update-directory-autoloads on command-line-args-left."
  (let ((delete-old-versions t)	;; force backup deletion without query.
	(kept-old-versions 0))
    (apply 'rgr-update-directory-autoloads command-line-args-left))
  (setq command-line-args-left nil))

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

;;;; Hacking meals.

;;;###AUTOLOAD
(defun rgr-region-meals (start end)
  (interactive "r")
  (shell-command-on-region start end
			   "../meals.pl --recipe-file ../recipes.text --det"))

;;;###AUTOLOAD
(defun rgr-dup-line ()
  "Duplicate the current line before the marked line.
If the first non-whitespace char is a '#', remove that and all whitespace
that comes immediately afterward.  The mark is moved after the added line,
point is moved to the next line."
  (interactive)
  (save-excursion
    (let ((line (buffer-substring-no-properties
		  (progn (beginning-of-line) (point))
		  (progn (end-of-line) (point)))))
      (goto-char (or (mark) (error "The mark is not set.")))
      (insert-before-markers line)
      (save-excursion
	(beginning-of-line)
	(skip-chars-forward " \t\n")
	(if (looking-at "#[ \t\n]*")
	    (replace-match "")))
      (insert-before-markers "\n")))
  (forward-line 1))

;;;; Done.

(provide 'rgr-random-hacks)
