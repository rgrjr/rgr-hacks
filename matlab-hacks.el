;;;; Random matlab hacking.
;;;
;;;    Put this in your .emacs file to get this:
;;;
;;;	(add-hook 'matlab-mode-hook 'bmerc-matlab-mode-hook)
;;;	(add-hook 'matlab-shell-mode-hook 'bmerc-matlab-shell-mode-hook)
;;;
;;;    Note that matlab-forward-sexp doesn't deal with comments at all . . . and
;;; this affects bmerc-matlab-indent-sexp, which should handle comments
;;; specially.  -- rgr, 21-Sep-99.
;;;
;;;    Bug:  indent-line-function (e.g. matlab-indent-line) should accept a
;;; numeric arg; see the indent-for-tab-command fn.  -- rgr, 21-Sep-99.
;;;
;;;    Modification history:
;;;
;;; created (still working on this).  -- rgr, 4-Jan-99.
;;; bmerc-matlab-shell-mode-hook: fix matlab-mode-install-path.  -- rgr,
;;;	25-Mar-99.
;;; bmerc-matlab-find-file-on-path-other-window, bmerc-matlab-indent-buffer,
;;;	bmerc-matlab-indent-sexp, and related hacks.  -- rgr, 21-Sep-99.
;;; bmerc-matlab-mode-hook: added fill-column init.  -- rgr, 23-Sep-99.
;;; matlab-rsh: new command, set matlab-shell-echoes, both for new
;;;	matlab.el version 2.2.2.  -- rgr, 24-Sep-99.
;;; bmerc-matlab-delete-indentation: new.  -- rgr, 6-Mar-00.
;;; bmerc-matlab-indent-sexp: use bmerc-matlab-indent-region.  -- rgr, 7-Mar-00.
;;; bmerc-matlab-add-to-modification-history, bmerc-matlab-indent-line (made
;;;	somewhat smarter that matlab-indent-line).  -- rgr, 9-Mar-00.
;;; bmerc-matlab-line-type: fix continuation bug.  -- rgr, 10-Mar-00.
;;; fix bmerc-matlab-id-at-point bug, unbind M-a, M-e, M-q.  -- rgr, 13-Mar-00.
;;; bmerc-matlab-shell-mode-hook: add C-c . etc. here.  -- rgr, 26-Jul-00.
;;; bmerc-matlab-shell-mode-hook: comint-process-echoes.  -- rgr, 8-Sep-00.
;;; bmerc-matlab-indent-region: use one % instead of two.  -- rgr, 19-Dec-00.
;;; bmerc-matlab-resync-path: new.  -- rgr, 11-Jan-01.
;;; bmerc-matlab-append-path-directory: fix typo.  -- rgr, 18-Jan-01.
;;; bmerc-matlab-resync-path: say if not changed.  -- rgr, 26-Feb-01.
;;; bmerc-matlab-grovel-path-output: Solaris toolbox dir.  -- rgr, 6-Mar-01.
;;;

;; Note that matlab.el needs (require 'cl) to compile correctly.  -- rgr,
;; 24-Sep-99.
(require 'matlab)

(defvar bmerc-matlab-after-line-indentations
	'((for . 2) (while . 2) (if . 2) (elseif . 2) (else . 2))
  "Adjustment to the indentation of the next line relative to the
previous line.")
(defvar bmerc-matlab-before-line-indentations
	'((else . -2) (elseif . -2) (end . -2))
  "Adjustment to the indentation of *this* line relative to the previous
line.")
(defvar bmerc-matlab-statement-names '(else elseif end for if while function)
  "Statements of interest (mostly for indentation).")
(defvar bmerc-matlab-statement-regexp
	(apply 'concat
	       (cons "%"
		     (mapcar (function (lambda (name)
			       (concat "\\|" (symbol-name name) "\\>")))
			     bmerc-matlab-statement-names)))
  "Regexp that matches interesting statement types, including comments.")

;;;###autoload
(defun bmerc-matlab-add-to-modification-history
       (&optional insert-definition-name-p)
  "Add to a modification history at the end of the first paragraph.
Sets the mark before moving there, and inserts a fresh comment line.
If no history exists (which it determines by searching for the string
in the rgr-modification-history-herald variable), then you are asked
about starting one.  (If you are asked this when there already is one,
then somebody probably inserted a blank line at the beginning of the
file.)"
  (interactive "P")
  (require 'rgr-hacks)
  (rgr-add-to-modification-history-internal
    "%    " "^%+ *" "%"
    ;; Use %* so we don't skip blank lines.
    "^%* *$"))

;;;; Indentation

(defun bmerc-matlab-find-prev-code-line ()
  "Move backwards one or more lines until a code line (a nonblank line
that does not start with a comment) is found."
  (forward-line -1)
  (while (and (not (bobp))
	      (looking-at "[ \t]*\\(%\\|$\\)"))
    (forward-line -1)))

(defun bmerc-matlab-line-type ()
  ;; Return a symbol identifying what kind of thing we have on the current line.
  (save-excursion
    (beginning-of-line)
    (cond ((save-excursion
	     ;; [this didn't work; you can have "..." before a comment.  The
	     ;; matlab version has problems, but at least it recognizes this
	     ;; case correctly.  -- rgr, 10-Mar-00.]
	     ;; (skip-chars-backward " \t\n.")
	     ;; (looking-at "[ \t]*\\.\\.\\.")
	     (forward-line -1)
	     (matlab-lattr-cont))
	    'matlab-continuation)
	  ((progn (back-to-indentation)
		  (looking-at bmerc-matlab-statement-regexp))
	    (intern (downcase (match-string 0)))))))

(defun bmerc-matlab-calc-indent ()
  ;; wrapper around the matlab.el matlab-calc-indent function.  -- rgr,
  ;; 9-Mar-00.
  "Return the appropriate indentation for this line as an integer."
  (interactive)
  (let ((previous-line-type
	  (save-excursion
	    (let ((start (point)))
	      (or (matlab-prev-line)
		  ;; isn't this beginning-of-line redundant?
		  (progn (beginning-of-line)
			 (forward-line -1)))
	      (matlab-beginning-of-command)
	      ;; We're now at the beginning of what matlab-mode considers the
	      ;; previous command.  See if we agree.
	      (bmerc-matlab-line-type)))))
    (cond ((eq previous-line-type '%)
	    ;; Oops; this is broken.  We need to calculate this ourselves.
	    (let ((line-type (bmerc-matlab-line-type))
		  (previous-indent
		    (save-excursion
		      (bmerc-matlab-find-prev-code-line)
		      (matlab-beginning-of-command)
		      (setq previous-line-type (bmerc-matlab-line-type))
		      (current-indentation))))
	      (message "Indenting prev %S %S, this %S"
		       previous-line-type previous-indent line-type)
	      (+ previous-indent
		 (or (cdr (assoc previous-line-type
				 bmerc-matlab-after-line-indentations))
		     0)
		 (or (cdr (assoc line-type
				 bmerc-matlab-before-line-indentations))
		     0))))
	  (t
	    ;; Use the matlab version
	    (matlab-calc-indent)))))

(defun bmerc-matlab-indent-line ()
  "Indent a line in `matlab-mode'."
  ;; hacked version of matlab-indent-line that uses bmerc-matlab-calc-indent
  ;; instead of matlab-calc-indent.  -- rgr, 9-Mar-00.
  (interactive)
  (let ((i (bmerc-matlab-calc-indent))
	(c (current-column)))
    (save-excursion
      (back-to-indentation)
      (cond ((not (= i (current-column)))
	      (beginning-of-line)
	      (delete-horizontal-space)
	      (indent-to i)))
      ;; If line contains a comment, format it.  [evidently disabled.  -- rgr,
      ;; 9-Mar-00.]
      (if () (if (matlab-lattr-comm) (matlab-comment))))
    (if (<= c i)
	(move-to-column i))))

(defun bmerc-matlab-indent-region (&optional start end)
  "Indent every line in the region."
  (interactive "r")
  (or end (setq end (point-max)))
  (save-excursion
    (goto-char (or start (point-min)))
    (beginning-of-line)
    (while (and (< (point) end)
		(not (eobp)))
      (cond ((looking-at "^[ \t]*$")
	      ;; keep blank lines empty.
	      (replace-match ""))
	    ((looking-at "^[ \t]*%+[ \t]*")
	      ;; standardize comment indentation
	      (replace-match "% ")
	      (bmerc-matlab-indent-line))
	    (t
	      (bmerc-matlab-indent-line)))
      (forward-line))))

(defun bmerc-matlab-indent-buffer ()
  "Indent every line from here to the end of the buffer.
On large files, this can take a while; please be patient."
  (interactive)
  (bmerc-matlab-indent-region (point) (point-max)))

(defun bmerc-matlab-indent-sexp (&optional endpos)
  "Indent the matlab expression or statement starting after point."
  (interactive)
  (bmerc-matlab-indent-region (save-excursion
				;; don't re-indent the current line.
				(forward-line)
				(point))
			      (or endpos
				  (save-excursion
				    (matlab-forward-sexp)
				    (point)))))

(defun bmerc-matlab-delete-indentation (arg)
  "Like \\[delete-indentation], joins this line to previous and fixes up whitespace at join.
If there is an ellipsis ('...') on the previous line, delete it.
If there is a fill prefix, delete it from the beginning of this line.
With argument, join this line to following line."
  ;; Probably doesn't handle EOL comments right; you're on your own.
  (interactive "*P")
  (delete-indentation arg)
  ;; Don't delete an ellipsis if we've just joined up a blank line; the ellipsis
  ;; still needs to be there (if it was correct in the first place).
  (if (and (not (looking-at "[ \t]*$"))
	   (save-excursion
	     (skip-chars-backward " \t.")
	     (looking-at "[ \t]*\\.\\.\\.[ \t]*")))
      (replace-match " ")))

;;;; resetting matlab-mode-install-path based on "path" command output.

(defun bmerc-matlab-parent-directory (file-name)
  ;; [not actually used.  -- rgr, 11-Jan-01.]
  (let ((parent (file-name-directory file-name)))
    (if (equal parent file-name)
	;; assume there was a trailing slash on file-name already, and that we
	;; really want its parent instead.
	(file-name-directory (directory-file-name parent))
	;; got it (with trailing slash).
	parent)))

(defun bmerc-matlab-directory-prefix-p (dir1 dir2)
  "return non-nil if dir1 is a directory prefix (i.e. an ancestor) of dir2."
  (let ((dir1-len (length dir1))
	(dir2-len (length dir2)))
    (and (> dir2-len dir1-len)
	 (equal dir1 (substring dir2 0 dir1-len))
	 ;; make sure the prefix is really a directory name.  [unix-dependent,
	 ;; though.  -- rgr, 11-Jan-01.]
	 (or (= (aref dir1 (1- dir1-len)) ?/)
	     (= (aref dir2 dir1-len) ?/)))))

(defun bmerc-matlab-append-path-directory (new-dir dir-list)
  ;; Return a new version of dir-list that does not contain any directories
  ;; shadowed by new-dir.
  (or (= (aref new-dir (1- (length new-dir))) ?/)
      (setq new-dir (file-name-as-directory new-dir)))
  (if (null dir-list)
      (list new-dir)
      (let* ((tail (cdr dir-list))
	     (new-dir-len (length new-dir))
	     (old-dir (car dir-list))
	     (old-dir-len (length old-dir))
	     (old-dir-parent
	       (directory-file-name (bmerc-matlab-parent-directory old-dir))))
	(cond ((or (equal old-dir new-dir)
		   (bmerc-matlab-directory-prefix-p old-dir new-dir))
		;; new-dir is already there, or is subsumed by this entry.
		dir-list)
	      (t
		;; we need both.
		(cons old-dir
		      (bmerc-matlab-append-path-directory new-dir tail)))))))

(defun bmerc-matlab-grovel-path-output (&optional end)
  ;; Starting from point, collect all path directories between here and end into
  ;; a list.  Shadowed directories are omitted.
  (let ((result nil)
	;; /usr/local/matlab/toolbox/ works on the alphas, but the Solaris
	;; version is /afs/bu.edu/common/IT/matlab-6.0/toolbox/, so it's much
	;; better to generalize.
	;; (toolbox-dir "/usr/local/matlab/toolbox/")
	(toolbox-dir-regexp "/matlab[-.0-9]*/toolbox/")
	(dir nil)
	(end (or end (point-max))))
    (beginning-of-line)
    (while (< (point) end)
      (cond ((and (looking-at "^\t\\(/[^ \t\n]+\\)$")
		  (setq dir (match-string 1))
		  (file-directory-p dir))
	      (if (string-match toolbox-dir-regexp dir)
		  ;; consolidate toolbox entries; we know the .m files in them
		  ;; must be orthogonal, because Mathworks couldn't afford to
		  ;; let any of them be shadowed.
		  (setq dir (substring dir 0 (match-end 0))))
	      (setq result
		    (bmerc-matlab-append-path-directory dir result))))
      (forward-line))
    result))

;; [doesn't work; output is asynchronous.  -- rgr, 11-Jan-01.]
'(defun bmerc-matlab-resync-path ()
  (interactive)
  (let* ((proc (or (get-buffer-process (current-buffer))
		   (error "Current buffer has no process")))
	 (pmark (process-mark proc))
	 (input "path"))
    (save-excursion
      (goto-char pmark)
      (let ((start (point)))
	(if (not comint-process-echoes)
	    (insert-before-markers input))
	(insert-before-markers ?\n)
	(funcall comint-input-sender proc input)
	;; This used to call comint-output-filter-functions,
	;; but that scrolled the buffer in undesirable ways.
	(run-hook-with-args 'comint-output-filter-functions "")))))

(defun bmerc-matlab-resync-path ()
  "Invoke this after a matlab 'path' command to rebuild the
matlab-mode-install-path search list from what matlab is actually using."
  (interactive)
  (message "Synchronizing matlab path ...")
  (let ((new-path (or (save-excursion
			(goto-char (or (marker-position comint-last-input-end)
				       (error "No last command.")))
			(bmerc-matlab-grovel-path-output))
		      (error "Last command was not 'path'."))))
    (cond ((equal new-path matlab-mode-install-path)
	    (message "Synchronizing matlab path ... done; %d %s."
		     (length matlab-mode-install-path)
		     "top-level directories [unchanged]"))
	  (t
	    (setq matlab-mode-install-path new-path)
	    (message "Synchronizing matlab path ... done; %d %s."
		     (length matlab-mode-install-path)
		     "top-level directories")))))

;;;; matlab-rsh

;;;###autoload
(defun matlab-rsh (host)
  "Like a combination of \\[rsh] and \\[matlab-shell], runs matlab on
the specified host via rsh."
  (interactive "sRun matlab on host: ")
  (if (and gnus-local-domain
	   (not (string-match "\\." host)))
      (setq host (concat host "." gnus-local-domain)))
  (cond ((and host
	      (not (equal host (system-name))))
	  (let ((command (concat matlab-shell-command
				 " " matlab-shell-command-switches))
		(display (getenv "DISPLAY")))
	    (if (and (eq window-system 'x)
		     display)
		(setq command (concat "setenv DISPLAY "
				      ;; work around braindead initializations
				      (if (= (aref display 0) ?\:)
					  (system-name)
					  "")
				      display "; " command)))
	    (message "Running '%s' on %s" command host)
	    (sit-for 1)
	    (let ((matlab-shell-command "rsh")
		  (matlab-shell-command-switches (list host command)))
	      (matlab-shell))))
	(t
	  ;; Run on the local machine.
	  (message "Running '%s' locally" matlab-shell-command)
	  (sit-for 1)
	  (matlab-shell))))

;;;; Finding dot-m files

(defun bmerc-matlab-id-at-point ()
  ;; Return a string containing the identifier nearest point.
  (save-excursion
    ;; If the previous character could be part of an identifier, it must be part
    ;; of this one, so move to the start of it.
    (if (and (not (bolp))
	     (save-excursion
	       (forward-char -1)
	       (looking-at "\\sw")))
	(forward-sexp -1))
    (matlab-navigation-syntax
      (if (looking-at "\\sw+\\>")
	  (match-string 0)))))

(defun bmerc-matlab-prompt-for-file-name (default)
  ;; Prompt the user for a 'file', with visible default.  Really, we just want
  ;; an identifier.
  (if default
      (let ((s (read-string (format "File (default %s): " default))))
	(if (string= s "") default s))
      (read-string "File: ")))

(defun bmerc-matlab-find-file-noselect (filename &optional no-error-p)
  ;; like find-file-noselect, but uses matlab-find-file-under-path logic instead
  ;; of expand-file-name to find the same .m file (if all is set up right) that
  ;; matlab would.  [stolen from the matlab-find-file-on-path fn.  -- rgr,
  ;; 21-Sep-99.]
  (if (string= filename "")
      (error "You must specify an M file"))
  (if (not (string-match "\\.m$" filename))
      (setq filename (concat filename ".m")))
  (let ((fname nil)
	(expanded (expand-file-name filename))
	(dirs matlab-mode-install-path))
    ;; [no, this is broken.  we shouldn't casually look for things in the
    ;; current directory; if that is not the first directory on
    ;; matlab-mode-install-path, they may be shadowed by a file of the same name
    ;; in an earlier directory.  -- rgr, 21-Sep-99.]  [except that the current
    ;; directory, e.g. ~psa/matlab/, is rarely on the path.  -- rgr, 21-Sep-99.]
    (cond ((file-exists-p expanded)
	    (setq fname expanded))
	  (t
	    (while (and (not fname) dirs)
	      ;; [why do we need to check for stringp?  -- rgr, 21-Sep-99.]
	      (if (stringp (car dirs))
		  (progn
		    (message "Searching for %s in %s" filename (car dirs))
		    (setq fname (matlab-find-file-under-path (car dirs)
							     filename))))
	      (setq dirs (cdr dirs)))))
    (cond (fname
	    (find-file-noselect fname))
	  (no-error-p nil)
	  (t
	    (error "File %s not found; check `matlab-mode-install-path'."
		   filename)))))

(defun bmerc-matlab-find-file-on-path (filename)
  "Find FILENAME on the current Matlab path.
The Matlab path is determined by `matlab-mode-install-path' and the
current directory.  You must add user-installed paths into
`matlab-mode-install-path' if you would like to have them included."
  (interactive
    (list (bmerc-matlab-prompt-for-file-name (bmerc-matlab-id-at-point))))
  (switch-to-buffer (bmerc-matlab-find-file-noselect filename)))

(defun bmerc-matlab-find-file-on-path-other-window (filename)
  "Find FILENAME on the current Matlab path in another window.  This is
like \\[bmerc-matlab-find-file-on-path] but puts the file in another
window."
  (interactive
    (list (bmerc-matlab-prompt-for-file-name (bmerc-matlab-id-at-point))))
  (switch-to-buffer-other-window (bmerc-matlab-find-file-noselect filename)))

(defun bmerc-matlab-find-file-on-path-other-frame (filename)
  "Find FILENAME on the current Matlab path in another frame.  This is
like \\[bmerc-matlab-find-file-on-path] but puts the file in another
frame."
  (interactive
    (list (bmerc-matlab-prompt-for-file-name (bmerc-matlab-id-at-point))))
  (switch-to-buffer-other-frame (bmerc-matlab-find-file-noselect filename)))

;;; Mode hook stuff.

(defun bmerc-matlab-common-mode-hook (map)
  "Stuff for both .m files and matlab-shell mode.  -- rgr, 25-Jul-00."
  ;; file finders.
  (define-key map "\C-c." 'bmerc-matlab-find-file-on-path)
  (define-key map "\C-c4." 'bmerc-matlab-find-file-on-path-other-window)
  (define-key map "\C-c5." 'bmerc-matlab-find-file-on-path-other-frame))

;;;###autoload
(defun bmerc-matlab-shell-mode-hook ()
  ;; install common commands.
  (bmerc-matlab-common-mode-hook matlab-shell-mode-map)
  ;; now why does matlab-shell-mode set this to t?  [now hacked locally.  --
  ;; rgr, 4-Jan-99.]  [oops; this came undone?  -- rgr, 8-Sep-00.]
  (setq comint-process-echoes t)
  ;; [the standard init uses matlab-mode-determine-mfile-path, which screws this
  ;; up.  -- rgr, 25-Mar-99.]
  (setq matlab-mode-install-path '("/usr/local/matlab/toolbox/")))

;;;###autoload
(defun bmerc-matlab-mode-hook ()
  (setq fill-column 80)
  (bmerc-matlab-common-mode-hook matlab-mode-map)
  ;; commands
  (define-key matlab-mode-map "\C-i" 'bmerc-matlab-indent-line)
  (define-key matlab-mode-map "\M-^" 'bmerc-matlab-delete-indentation)
  (define-key matlab-mode-map "\C-\M-q" 'bmerc-matlab-indent-sexp)
  (define-key matlab-mode-map "\M-*" 'bmerc-matlab-add-to-modification-history)
  (define-key matlab-mode-map "\M-q" 'rgr-fill-comment)
  ;; the matlab-mode versions of these are kinda useless.  -- rgr, 13-Mar-00.
  (define-key matlab-mode-map "\M-e" nil)
  (define-key matlab-mode-map "\M-a" nil))

(provide 'matlab-hacks)

;;; (setq comint-process-echoes t)

