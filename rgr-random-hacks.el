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
;;;    [old] Modification history:
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
  (apply 'rgr-update-directory-autoloads command-line-args-left)
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

;;;; FASTA-format sequence hackery.

;; By looking for offsets at the start of the line, this sometimes works for
;; random sequence alignments as well.

(defvar rgr-fasta-subscripted-locus-re
        (let ((captured-digits "\\([0-9]+\\)"))
	  (concat "^[ \t]*\\([^][ \t\n:]+\\["
		  captured-digits ":" captured-digits "\\]:?\\) *")))

(defun rgr-fasta-goto-start-with-offset ()
  (cond ((save-excursion
	   (beginning-of-line)
	   (looking-at rgr-fasta-subscripted-locus-re))
	  ;; This captures lines that start "foo[287:503]:  178 PRXSQGL...",
	  ;; where the 178 is relative to the 287 subsequence starting index.
	  ;; The offset is (+ (1- 287) (1- 178)) = 463 here, so P is 464.
	  (let ((offset (1- (string-to-int (match-string 2)))))
	    (goto-char (match-end 0))
	    (cond ((looking-at "\\([0-9]+\\) *")
		    (goto-char (match-end 0))
		    (+ offset (1- (string-to-int (match-string 1)))))
		  (t
		    offset))))
	((save-excursion
	   (beginning-of-line)
	   (looking-at "^[ \t]*\\([^ \t\n:]+:[ \t]*\\)?\\([0-9]+\\) *"))
	  (goto-char (match-end 0))
	  (1- (string-to-int (match-string 2))))
	((or (looking-at "^>")
	     (re-search-backward "^>" nil t))
	  (forward-line)
	  0)
	(t
	  (error "Can't find the start of a FASTA sequence."))))

;;;###autoload
(defun rgr-fasta-goto-base (base-number)
  "Given that we are within a FASTA sequence, goto to the numbered base."
  (interactive "NGoto FASTA base: ")
  (setq base-number (prefix-numeric-value base-number))
  (or (> base-number 0)
      (error "Base number must be positive."))
  (let ((pos nil))
    (save-excursion
      (let ((offset (1+ (rgr-fasta-goto-start-with-offset))))
	(while (and (< offset base-number)
		    (not (member (char-after) '(?> nil))))
	  (skip-chars-forward "^>A-Za-z")
	  (forward-char 1)
	  (setq offset (1+ offset)))
	(skip-chars-forward "0-9 \t\n")
	(if (not (looking-at "[a-zA-Z]"))
	    (error "No such base."))
	(setq pos (point))))
    (and pos
	 (goto-char pos))))

;;;###autoload
(defun rgr-fasta-what-base ()
  "Within a FASTA sequence, show the base number of the base after point."
  (interactive)
  (save-excursion
    (let ((start (point))
	  (base-number (rgr-fasta-goto-start-with-offset)))
      (skip-chars-forward "^>A-Za-z0-9")
      (while (and (< (point) start)
		  (not (member (char-after) '(?> nil))))
	(forward-char 1)
	(setq base-number (1+ base-number))
	(skip-chars-forward "^>A-Za-z"))
      (skip-chars-forward " \t\n")
      (if (member (char-after) '(?> nil))
	  (message "After base %d." base-number)
	  (message "Before base %d." (1+ base-number))))))

(defvar rgr-seqaln-diff-line-match "^      [ |]+$")

(defun rgr-seqaln-find-match-point (point)
  (save-excursion
    (goto-char point)
    (while (and (not (eobp))
		(looking-at "^$\\|Score:"))
      (forward-line))
    (cond ((eobp)
	    ;; Oops; we are at the end.  Move backward to the end of the last
	    ;; nonblank line.
	    (while (and (not (bobp)) (eolp))
	      (forward-line -1))
	    (if (bobp)
		(error "Empty buffer."))
	    (end-of-line)))
    (let ((col (current-column)))
      (if (< col 6)
	  (setq col 6))
      (beginning-of-line)
      (cond ((or (looking-at rgr-seqaln-diff-line-match)
		 (progn (forward-line 1)
			(looking-at rgr-seqaln-diff-line-match))
		 (progn (forward-line -2)
			(looking-at rgr-seqaln-diff-line-match)))
	      (move-to-column col)
	      (if (eolp)
		  (forward-char -1))
	      (point))
	    (t
	      (goto-char point)
	      (message "This does not appear to be in seqaln output.")
	      '(sit-for 1)
	      nil)))))

;;;###autoload
(defun rgr-seqaln-percent-homology (start end)
  "With point and mark in the same alignment, report the pct identity between."
  (interactive "r")
  (let ((match-start (rgr-seqaln-find-match-point start))
	(match-end (rgr-seqaln-find-match-point end))
	(n-matches 0) (n-positions 0))
    ;; (message "[got %S and %S.]" match-start match-end)
    (if (and match-start match-end)
	(save-excursion
	  (goto-char match-start)
	  (while (<= (point) match-end)
	    (cond ((eobp)
		    (error "Not in an alignment any more."))
		  ((eolp)
		    (forward-line 3)
		    (or (looking-at rgr-seqaln-diff-line-match)
			(error "Not in an alignment any more."))
		    (forward-char 6)))
	    (if (= (char-after) ?|)
		(setq n-matches (1+ n-matches)))
	    (setq n-positions (1+ n-positions))
	    (forward-char))
	  (message "%s:  %d/%d matches, %.1f%%"
		   (buffer-name) n-matches n-positions
		   (/ (* n-matches 100) n-positions))))))

'(defun rgr-foo ()
  ;; Batch percent identity computation.
  (interactive)
  (let ((files (directory-files
		"/home/shared/monsanto/rogers/icp/profile/collin"
		t "\\.seqaln$")))
    (while files
      (let ((file (car files)))
	(save-excursion
	  (set-buffer (find-file-noselect file))
	  (rgr-seqaln-percent-homology (point-min) (point-max))))
      (setq files (cdr files)))))

;;;; Done.

(provide 'rgr-random-hacks)

