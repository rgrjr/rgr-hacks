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
;;; $Id$

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
	  (let ((offset (1- (string-to-number (match-string 2)))))
	    (goto-char (match-end 0))
	    (cond ((looking-at "\\([0-9]+\\) *")
		    (goto-char (match-end 0))
		    (+ offset (1- (string-to-number (match-string 1)))))
		  (t
		    offset))))
	((save-excursion
	   (beginning-of-line)
	   (looking-at "^[ \t]*\\([^ \t\n:]+:[ \t]*\\)?\\([0-9]+\\) *"))
	  (goto-char (match-end 0))
	  (1- (string-to-number (match-string 2))))
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

;;;###autoload
(defun rgr-view-sequence-at-point ()
  (interactive)
  (require 'ffap)
  (let ((string (ffap-string-at-point 'url))
	(host "localhost"))
    (if (and string
	     (string-match "^\\(.+\\)@\\(.+\\)$" string))
	(setq host (match-string 2 string)
	      string (match-string 1 string)))
    ;; groES_vector and OPH%@alexandria and groES_v% and exp14%@localhost:8081
    (and string (> (length string) 0)
	 (let* ((page-base
		 (if (string-match "^[0-9]+$" string)
		     "tools/view-sequence.cgi?sequence_id="
		     "search/find-sequence.cgi?reload=Reload&substance_name="))
		(url (concat "http://" host "/modest/" page-base
			     ;; [this should be escaped.  -- rgr, 22-Dec-05.]
			     string)))
	   (browse-url url)))))

;;;###autoload
(defun rgr-delete-non-sequence-characters ()
  "Hack to convert the rest of the file to something resembling a
protein sequence."
  (interactive)
  (while (re-search-forward "[^ \t\nacdefghiklmnpqrstvwy]+" nil t)
    (replace-match "" t t)))

;;;; Hacking sequence annotation.

(defun rgr-add-entry (alist dir key value)
  (let ((entry (assoc key alist)))
    (if (null entry)
	(setq entry (list key)
	      alist (cons entry alist)))
    (let ((subentry (assoc dir (cdr entry))))
      (cond ((null subentry)
	     (setq subentry (list dir))
	     (setcdr entry (cons subentry (cdr entry)))))
      (setcdr subentry (cons value (cdr subentry))))
    alist))

(defun rgr-annotation-display-diffs (alist &optional have-delimiter-p)
  ;; Find and display differences.
  (let ((tail (sort alist #'(lambda (cell1 cell2)
			      (string-lessp (car cell1) (car cell2))))))
    '(message "%S" tail)
    (while tail
      (let* ((entry (car tail))
	     (key (car entry))
	     (add (cdr (assoc ?+ (cdr entry))))
	     (del (cdr (assoc ?- (cdr entry)))))
	;; [this assumes that (a) the locus always sorts first, and (b) if we
	;; get here at all, there must be some significant different, so we need
	;; to print the locus anyway for identification.
	(while (or add del)
	  (cond ((or (not (equal (car del) (car add)))
		     (equal key "(locus)"))
		 (cond ((not have-delimiter-p)
			(princ "@@\n")
			(setq have-delimiter-p t)))
		 (cond ((equal (car del) (car add))
			 ;; Display the locus for user orientation.
			 (princ (format " %s=%s\n" key (car del))))
		       (t
			 (if (car del)
			     (princ (format "-%s=%s\n" key (car del))))
			 (if (car add)
			     (princ (format "+%s=%s\n" key (car add))))))))
	  (setq add (cdr add))
	  (setq del (cdr del))))
      (setq tail (cdr tail)))))

;;;###AUTOLOAD
(defun rgr-diff-annotation (start end)
  "Show differences between sequence annotation found in diff output."
  (interactive "r")
  (with-output-to-temp-buffer "*annotation-diffs*"
    (save-excursion
      (let ((alist nil))
	;; Collect attribute values.
	(goto-char start)
	(beginning-of-line)
	(while (< (point) end)
	  (cond ((looking-at "^\\([-+]\\)> *\\([^ \t\r\n]+\\) *")
		  (let ((dir (aref (match-string-no-properties 1) 0))
			(locus (match-string-no-properties 2)))
		    (setq alist (rgr-add-entry alist dir "(locus)" locus))
		    (goto-char (match-end 0))
		    (while (looking-at "\\([^ ]+\\)=\\([^,\r\n]+\\)*")
		      (let ((key (match-string-no-properties 1))
			    (value (match-string-no-properties 2)))
			(setq alist (rgr-add-entry alist dir key value)))
		      (goto-char (match-end 0))
		      (if (looking-at ",")
			  (skip-chars-forward ", \t\r\n")))))
		((looking-at "^@@")
		  ;; End of the hunk.
		  (rgr-annotation-display-diffs alist)
		  (princ (buffer-substring-no-properties
			   (point)
			   (save-excursion
			     (progn (forward-line) (point)))))
		  (setq alist nil)
		  (forward-line))
		((looking-at "^ ")
		  ;; End of difference within the hunk.
		  (rgr-annotation-display-diffs alist t)
		  (setq alist nil)
		  (forward-line))
		((looking-at "^\\(\\+\\+\\+\\|---\\)")
		  ;; Start of a new file; end the old hunk, and echo the line.
		  (rgr-annotation-display-diffs alist)
		  (setq alist nil)
		  (let ((start (point)))
		    (forward-line)
		    (princ (buffer-substring start (point)))))
		(t
		  (forward-line))))
	;; Catch leftovers.
	(rgr-annotation-display-diffs alist)
	(message "Done.")))))

;;;; Hacking meals.

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

