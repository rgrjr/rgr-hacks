;;;****************************************************************************
;;;
;;;    GNU Emacs compile-mode hackery.
;;;
;;;    compile mode consists of support for M-x grep and M-x compile commands.
;;; We add a few extra tricks.
;;;
;;;    To compile this without error, do the following:
;;;
;;;	  (mapcar 'require '(compile))
;;;
;;;    Modification history:
;;;
;;; cdgrep command.  -- rgr, 4-Apr-94.
;;; split out of ./rgr-hacks.el file.  -- rgr, 26-Mar-96.
;;; rgr-clean-c-compilation-buffer: new.  -- rgr, 16-Jul-96.
;;; compile-this-file: must (require 'compile) [bug fix].  -- rgr, 16-Jul-95.
;;; rgr-sunos-undeclared-functions: added fputs, sscanf.  -- rgr, 8-Aug-96.
;;; compile-this-file: optimize -O2 by default.  -- rgr, 14-Aug-95.
;;; rgr-recompile: new.  -- rgr, 30-Aug-96.
;;; rgr-find-last-compilation-buffer: hack for rgr-clean-c-compilation-buffer.
;;;	-- rgr, 5-Sep-96.
;;; rgr-maybe-clean-c-compilation-buffer: auto feature.  -- rgr, 13-Nov-96.
;;; rgr-sunos-undeclared-functions: add _filbuf.  -- rgr, 14-Nov-96.
;;; rgr-recompile: fix "Invoke compile afresh" arg bug.  -- rgr, 14-Nov-96.
;;; rgr-recompile: make "last compile in cd" really work.  -- rgr, 25-Nov-96.
;;; split out of ./rgr-shell-hacks.el file.  -- rgr, 26-Nov-96.
;;; putchar expansion includes _flsbuf.  -- rgr, 13-Mar-97.
;;; rgr-sunos-undeclared-functions: strtol.  -- rgr, 30-May-97.
;;; rgr-recompile: must expand dir before comparing.  -- rgr, 16-Jun-97.
;;; rgr-gcc-alpha-assembler-warning & support.  -- rgr, 3-Nov-97.
;;; cdgrep: establish default directory.  -- rgr, 10-Mar-98.
;;; rgr-grep-summarize-hits: new command.  -- rgr, 11-Mar-98.
;;; rgr-sunos-undeclared-functions: added ungetc.  -- rgr, 24-Jul-98.
;;; cdgrep: improved to use the cdgrep.pl script.  -- rgr, 16-Jun-99.
;;; rgr-compilation-mode-hook: compilation-window-height.  -- rgr, 10-Oct-00.
;;; rgr-grep-annotate-current-buffer and friends.  -- rgr, 27-Feb-02.
;;; rgr-compilation-mode-hook: oops; mapc is not elisp.  -- rgr, 15-Apr-02.
;;; trivial tweaks.  -- rgr, 24-Jul-02.
;;; flush obsolete cdgrep command.  -- rgr, 13-Apr-03.
;;; rgr-grep-annotate-with-definition-names: use ilisp-possibilities instead of
;;;	old rgr-next-possibility version.  -- rgr, 19-Apr-03.
;;;

(require 'compile)

(defvar compilation-buffer-compile-command nil)

;;;; compile-this-file

;;;***autoload
(defun compile-this-file ()
  "Compile the current buffer by running gcc asynchronously."
  (interactive)
  (and (buffer-modified-p)
       (or (not (buffer-file-name))
	   (format "Save file %s? " (buffer-file-name)))
       (save-buffer))
  (setq compile-command (concat "gcc -ansi -pedantic -O2 -c "
				(file-name-nondirectory (buffer-file-name))))
  (compile-internal compile-command "No more errors"))

;;;; rgr-clean-c-compilation-buffer

(defvar rgr-gcc-implicit-declaration-warning
       "warning: implicit declaration of function `\\([^ ]+\\)'"
  "Regular expression that matches implicit function declaration
warnings, and identifies the name of the function.")
(defvar rgr-sunos-undeclared-functions '(atoi strtol
					 fclose fprintf fputs fscanf
					 printf sscanf ungetc
					 ;; getc expansion includes _filbuf.
					 _filbuf
					 ;; putchar expansion includes _flsbuf
					 _flsbuf)
  "Functions left undeclared by SunOS headers.")

(defvar rgr-gcc-alpha-assembler-warning
        (and (equal system-configuration "alpha-dec-osf1")
	     (concat "as0: .*: extraneous values on version stamp ignored\n"
		     "      .verstamp.*\n"))
  "Idiot extra lines to kill on gcc/alpha combo.")

(defun rgr-find-last-compilation-buffer (dir)
  "Find the last compilation-mode buffer associated with dir.
Should be consolidated with rgr-recompile code."
  ;; [***bug***: this doesn't work; buffer-list isn't ordered right.  -- rgr,
  ;; 5-Sep-96.]
  (let ((tail (buffer-list)) (compilation-buffer nil))
    (save-excursion
      (while tail
	(let ((buffer (car tail)))
	  (set-buffer buffer)
	  (if (and (eq major-mode 'compilation-mode)
		   (not (string-match "grep" (buffer-name)))
		   (equal dir default-directory))
	      (setq compilation-buffer buffer
		    tail nil)))
	(setq tail (cdr tail))))
    compilation-buffer))

(defun rgr-clean-c-compilation-buffer-internal ()
  ;; Do the grunt work, assuming we are in the buffer.
  (goto-char (point-min))
  (while (re-search-forward rgr-gcc-implicit-declaration-warning nil t)
    (let ((function (intern (buffer-substring (match-beginning 1)
					      (match-end 1)))))
      (forward-line)
      (if (memq function rgr-sunos-undeclared-functions)
	  (delete-region (point)
			 (progn (forward-line -1)
				(point))))))
  (cond (rgr-gcc-alpha-assembler-warning
	  (goto-char (point-min))
	  (while (re-search-forward rgr-gcc-alpha-assembler-warning nil t)
	    (replace-match ""))))
  ;; Postpass to get rid of useless file names.  Go backwards from the end of
  ;; the buffer in order to trash multiple useless "In function" lines
  ;; generated for the same file.
  (goto-char (point-max))
  (while (re-search-backward "^\\([^ \t\n]+:\\) In function" nil t)
    (let* ((line-start (match-beginning 0))
	   (file-prefix (buffer-substring line-start (match-end 1))))
      (forward-line)
      (if (looking-at (regexp-quote file-prefix))
	  ;; Real errors in this function; leave it alone.
	  (forward-line -1)
	  ;; Dangling "In function" line; nuke it.
	  (delete-region line-start (point))))))

;;;###autoload
(defun rgr-clean-c-compilation-buffer ()
  "Get rid of all warnings that match rgr-gcc-implicit-declaration-warning
and name a function on the rgr-sunos-undeclared-functions list.  If not
in a compilation buffer, the last one associated with the current
directory is used.  The warning is assumed to fit on one line."
  (interactive)
  (save-excursion
    (or (eq major-mode 'compilation-mode)
	(set-buffer
	  (or (rgr-find-last-compilation-buffer default-directory)
	      (error "No compilation in %s directory." default-directory))))
    (rgr-clean-c-compilation-buffer-internal)))

;;;###autoload
(defun rgr-maybe-clean-c-compilation-buffer (buffer message)
  ;; Set compilation-finish-function to this in order to do an implicit
  ;; rgr-clean-c-compilation-buffer after every compilation.
  (if (and (string-match "finished" message)
	   (string-match "compilation" (buffer-name buffer)))
      (save-excursion
	(set-buffer buffer)
	(rgr-clean-c-compilation-buffer-internal))))

;;;; rgr-grep-summarize-hits

(defvar rgr-grep-summarize-hits-command-string
	"perl -ne 'print \"$1\\n\" if /^([^ :]+):/;' | uniq -c"
  "Shell command to summarize the hits in a grep (compilation) buffer.")

;;;###autoload
(defun rgr-grep-summarize-hits ()
  "Display a summary of hits from the last \\[grep] command."
  (interactive)
  ;; this is based on shell-command-on-region
  (let ((buffer nil))
    (save-excursion
      (set-buffer (compilation-find-buffer))
      (setq buffer (get-buffer-create (concat (buffer-name) " summary")))
      ;; Clear the output buffer, then run the command with output there.
      (save-excursion
	(set-buffer buffer)
	(setq buffer-read-only nil)
	(erase-buffer))
      (call-process-region (point-min) (point-max) shell-file-name
			   nil buffer nil
			   shell-command-switch
			   rgr-grep-summarize-hits-command-string))
    (set-window-start (display-buffer buffer) 1)))

;;;; Annotating a grep buffer with Lisp definition names.

;; A more general mechanism (e.g. one that knew about perl or C definitions) is
;; devoutly to be desired.

(defun rgr-grep-fixup-next-error (next-error)
  ;; Fixup cons of (error-marker . source-spec) by side-effect.  This is done by
  ;; converting source-spec into a marker if it is not one already (or nil),
  ;; returning next-error.  [Based on some wild code, slightly tamed, in a while
  ;; test in the compilation-next-error-locus fn.  -- rgr, 27-Feb-02.]
  (cond ((or (null (cdr next-error))
	     (markerp (cdr next-error)))
	  ;; This error has already been done (or is boring).
	  next-error)
	(t
	  ;; This error has a filename/lineno pair.
	  ;; Find the file and turn it into a marker.
	  (let* ((fileinfo (car (cdr next-error)))
		 (buffer (apply 'compilation-find-file
				(car next-error) fileinfo)))
	    (if (null buffer)
		;; We can't find this error's file.
		(setcdr next-error nil)
		;; We found the file.  Get a marker for this error.
		;; compilation-error-screen-columns is buffer-local so we must
		;; be careful to extract its value before switching to the
		;; source file buffer.
		(let ((columns compilation-error-screen-columns)
		      (error-line (nth 1 (cdr next-error)))
		      (error-column (nth 2 (cdr next-error))))
		  (set-buffer buffer)
		  (save-excursion
		    (save-restriction
		      (widen)
		      (goto-line error-line)
		      ;; Columns in error msgs are 1-origin.
		      (cond ((or (not error-column) (zerop error-column))
			      (beginning-of-line))
			    (columns
			      (move-to-column (1- error-column)))
			    (t
			      (forward-char (1- error-column))))
		      (setcdr next-error (point-marker))))))
	    next-error))))

;;;###autoload
(defun rgr-grep-annotate-current-buffer (current-buffer &optional msg)
  ;; intended for compilation-finish-functions list usage.
  ;; [but i broke it some how . . .  -- rgr, 27-Feb-02.]
  (compile-reinitialize-errors t)
  ;; [for the rgr-lisp-def-name fn.  -- rgr, 27-Feb-02.]
  (require 'rgr-lisp-hacks)
  (let ((tail compilation-error-list) (defn-name nil)
	(last-definition-buffer nil)
	(last-definition-point nil))
    (while tail
      (let* ((next-error (rgr-grep-fixup-next-error (car tail)))
	     (error-marker (car next-error))
	     (source-marker (cdr next-error)))
	(and source-marker
	     (set-buffer (marker-buffer source-marker)))
	(cond ((null source-marker))
	      ((not (member major-mode '(emacs-lisp-mode fi:common-lisp-mode
					 common-lisp-mode lisp-mode)))
		;; bug: only know how to do Lisp modes.
		)
	      ((progn (goto-char source-marker)
		      (if (and (not (bobp))
			       (not (looking-at "^(")))
			  (beginning-of-defun))
		      (not (looking-at "^(")))
		;; couldn't find a defun.
		)
	      ((and (eq (current-buffer) last-definition-buffer)
		    (eq (point) last-definition-point))
		;; still in the same definition
		)
	      ((setq defn-name (rgr-lisp-def-name))
		(setq last-definition-buffer (current-buffer))
		(setq last-definition-point (point))
		(set-buffer (marker-buffer error-marker))
		(goto-char error-marker)
	        (let ((defn-name-start (point))
		      (defn-name-end nil))
		  (insert-before-markers defn-name)
		  (setq defn-name-end (point))
		  (goto-char defn-name-start)
		  ;; skip past the "defun " (or whatever).
		  (skip-chars-forward "^ \t")
		  (skip-chars-forward " \t")
		  (put-text-property (point) defn-name-end
				     'face 'font-lock-function-name-face)
		  ;; get back to where we were inserting.
		  (goto-char defn-name-end))
	        (insert-before-markers" :\n"))))
      (setq tail (cdr tail)))))

;;;###autoload
(defun rgr-grep-annotate-with-definition-names ()
  "Annotate the output of the last \\[grep] command with Lisp definition
names for each hit."
  ;; this requires the ilisp-possibilities stuff.
  (interactive)
  (require 'ilisp-possibilities)
  (save-excursion
    (set-buffer (or (ilisp-current-possibility-buffer)
		    (error "No current possibilities buffer.")))
    (rgr-grep-annotate-current-buffer (current-buffer))))

;;;; rgr-recompile

(defun rgr-remember-compile-command ()
  (set (make-local-variable 'compilation-buffer-compile-command)
       compile-command))

;; This is sufficiently benign that we can do it unconditionally.  -- rgr,
;; 26-Nov-96.
(add-hook 'compilation-mode-hook 'rgr-remember-compile-command)

;;;###autoload
(defun rgr-recompile (&optional reprompt-p)
  "Redo the last compile command involving the current directory,
without asking any questions."
  (interactive "P")
  (let* ((dir default-directory)
	 (compilation-buffer (rgr-find-last-compilation-buffer dir)))
    (if compilation-buffer
	(let ((command nil))
	  (save-excursion
	    ;; Get the command to redo.
	    (set-buffer compilation-buffer)
	    (setq command (or compilation-buffer-compile-command
			      ;; hack.
			      compile-command))
	    (if reprompt-p
		;; this is taken from the compile command's interactive spec.
		(setq command (read-from-minibuffer "Compile command: "
						    command nil nil
						    '(compile-history . 1))))
	    ;; Save buffers in this directory.
	    (let ((tail (buffer-list))
		  (pattern (concat "^" (regexp-quote (expand-file-name dir)))))
	      (while tail
		(set-buffer (car tail))
		(if (and (buffer-modified-p)
			 (not (buffer-base-buffer))
			 buffer-file-name
			 (string-match pattern buffer-file-name))
		    (save-buffer))
		(setq tail (cdr tail)))))
	  ;; Recompile (copied from the body of the compile command).
	  (let ((default-directory dir))
	    (save-excursion
	      (setq compile-command command)
	      (compile-internal compile-command "No more errors"))))
	;; Invoke compile afresh.
	(compile (if (or compilation-read-command current-prefix-arg)
		     (read-from-minibuffer "Compile command: "
					   compile-command nil nil
					   '(compile-history . 1))
		     compile-command)))))

;;;; Tag ends.

;;;###autoload
(defun rgr-compilation-mode-hook ()
  (mapcar (function (lambda (fn)
	    (or (member fn compilation-finish-functions)
		(setq compilation-finish-functions
		      (cons fn compilation-finish-functions)))))
	  '(rgr-grep-annotate-current-buffer
	    rgr-maybe-clean-c-compilation-buffer))
  ;; Customize the window height.  This is 1/4 of the frame, but never less than
  ;; six lines.  [stolen from the rgr-rmail-mode-hook version.  -- rgr,
  ;; 10-Oct-00.]
  (setq compilation-window-height (max (/ (frame-height) 4) 6)))

(provide 'rgr-compile-hacks)
