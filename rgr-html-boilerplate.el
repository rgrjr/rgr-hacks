;;;; Automatic boilerplate.
;;;
;;;    Modification history:
;;;
;;; rgr-html-update-buffer-boilerplate: new feature.  -- rgr, 29-Nov-97.
;;; rgr-html-update-boilerplate: finished single-shot ver.  -- rgr, 3-Dec-97.
;;; rgr-html-execute-do: support "do" boilerplate.  -- rgr, 5-Dec-97.
;;; rgr-html-boilerplate-tag-re: flush whitespace, new
;;;	rgr-html-delete-boilerplate fn.  -- rgr, 27-Jan-98.
;;; split out of ./rgr-html-hacks.el file.  -- rgr, 23-Mar-98.
;;; rgr-html-batch-update-boilerplate: trying to get working.  -- rgr, 8-Apr-98.
;;; rgr-html-update-boilerplate-internal: rgr-html-parse-tag-attributes (name
;;;	change).  -- rgr, 16-Apr-98.
;;; rgr-html-update-boilerplate-internal: insert-do, insert-string escapes,
;;;	use correct "<!--!boilerplate->>" syntax.  -- rgr, 1-Jul-98.
;;; rgr-html-execute-do: no messages if noninteractive.  -- rgr, 8-Apr-99.
;;; rgr-html-update-boilerplate-internal: ignore-status op.  -- rgr, 15-Nov-99.
;;; rgr-html-update-boilerplate-internal, rgr-html-execute-do: eql -> eq.
;;;	-- rgr, 1-May-00.
;;;

(require 'rgr-html-hacks)

(defvar rgr-html-boilerplate-tag-re
	(concat "<!-*\\(/\\)?"
		;; [***kludge***: the "-*" is for backward compatibility; there
		;; should always be exactly two dashes.  -- rgr, 1-Jul-98.]
		;; We need the \(..\) around this, so rgr-html-forward-markup
		;; doesn't lose on the tag name.
		"\\(boilerplate\\)\\>"))

(defvar rgr-html-flush-transcript-p t
  "*Controls whether to get rid of old 'do' boilerplate output when
rerunning, or leave the *boilerplate-commands* buffer alone.")

(defun rgr-html-execute-do (command &optional insert-stdout-p ignore-status-p)
  ;; command is a string with a Unix command; execute it in the
  ;; *boilerplate-commands* buffer.  [ought to have some sort of banner to
  ;; separate multiple invocations.  -- rgr, 5-Dec-97.]
  (while (string-match "&[lg]t;\\|\\\\\n[ \t]*" command)
    ;; If not in a regexp, "\\\\\n[ \t]*" would look like "\\^J[ ^I]*", and
    ;; means "match a backslash followed immediately by a newline and optional
    ;; whitespace."  This is the `make' multiline continuation convention.  --
    ;; rgr, 8-Feb-98.  [***kludge***: < and > in strings shortcircuit emacs
    ;; syntax.  -- rgr, 8-Feb-98.]
    (let* ((match (match-string 0 command))
	   (redirect (cond ((equal match "&gt;") ">")
			   ((equal match "&lt;") "<")
			   (t " "))))
      ;; (message "Got %s" match) (sit-for 1)
      (setq command (replace-match redirect t t command))))
  (let ((thisdir default-directory)
	(outbuf (get-buffer-create "*boilerplate-commands*"))
	(start-point nil) (end-point nil)
	(outwin nil))
    (save-excursion
      (set-buffer outbuf)
      (setq buffer-read-only nil)
      (cond (rgr-html-flush-transcript-p
	      (buffer-disable-undo (current-buffer))
	      (erase-buffer)
	      (buffer-enable-undo (current-buffer))))
      (cond ((or rgr-html-flush-transcript-p
		 (not (equal default-directory thisdir)))
	      (setq default-directory thisdir)
	      (insert "cd " thisdir "\n")))
      (setq rgr-html-flush-transcript-p nil)
      (insert command "\n")
      (setq start-point (point))
      (set-buffer-modified-p nil))
    ;; Pop up the compilation buffer.
    (setq outwin (display-buffer outbuf))
    (save-excursion
      (set-buffer outbuf)
      (setq default-directory thisdir)
      (set-window-start outwin (point-min))
      (or (eq outwin (selected-window))
	  (set-window-point outwin (point-min)))
      ;; (compilation-set-window-height outwin)
      ;; Start the "compilation" as if no asynchronous processes were available.
      (or noninteractive
	  (message "Executing `%s'..." command))
      ;; Fake modeline display as if `start-process' were run.
      (setq mode-line-process ":run")
      (force-mode-line-update)
      (sit-for 0)			; Force redisplay
      (let ((status (call-process shell-file-name nil outbuf nil "-c" command)))
	(cond ((eq status 0))
	      ((numberp status)
	        (or ignore-status-p
		    (error "Command exited abnormally with code %d" status)))
	      ((stringp status)
		(error "Command got signal %s" status))
	      (t
		(error "Command exited with odd status %s" status))))
      (setq mode-line-process ":exit")
      (force-mode-line-update)
      (sit-for 0)			; Force redisplay
      (or noninteractive
	  (message "Executing `%s'...done" command)))
    (if insert-stdout-p
	(insert-buffer-substring outbuf start-point))))

(defun rgr-html-update-all-boilerplate ()
  ;; Grunter for rgr-html-update-buffer-boilerplate and
  ;; rgr-html-update-boilerplate-internal functions.
  (goto-char (point-min))
  (while (re-search-forward rgr-html-boilerplate-tag-re nil t)
    (if (match-beginning 1)
	(error "Unmatched <!--/boilerplate--> tag."))
    (rgr-html-update-boilerplate-internal)))

(defun rgr-html-update-boilerplate-internal ()
  ;; Assumes we're inside the <!--boilerplate--> tag, just after the name.  Go do it
  ;; for this guy, leaving point after the end of the matching <!--/boilerplate-->
  ;; tag.
  (let* ((attributes (rgr-html-parse-tag-attributes))
	 (start (point))
	 (ignore-status-p nil)
	 (end (save-excursion
		(forward-sexp -1)
		(rgr-html-forward-markup 1 rgr-html-boilerplate-tag-re t)
		(forward-sexp -1)
		(point)))
	 (rescan-p nil))
    ;; start and end are now just inside the boilerplate construct.
    (delete-region start end)
    (goto-char start)	;; probably redundant.
    (push-mark start t)
    ;; Remember where the boilerplate ends so we can return there reliably
    ;; (actually, we remember the char after the end so we don't get screwed up
    ;; by inserting after markers).
    (setq end (set-marker (make-marker) (1+ (point))))
    ;; [use the nl operation for this.  -- rgr, 2-Dec-97.]
    ;; (insert "\n")
    (while attributes
      (let ((name (car (car attributes))) (value (cdr (car attributes)))) 
	(cond ((eq name 'rescan) (setq rescan-p t))
	      ;; (t (insert (format "%S -> %s" name value) "\n"))
	      ((eq name 'nl) (insert "\n"))
	      ((eq name 'insert-string)
	        ;; useful only for putting something in between.
	        (let ((pos 0))
		  (while (string-match "\\\\[tn\\\\]" value pos)
		    (let* ((match (aref value (1+ (match-beginning 0))))
			   (replacement (cond ((eq match ?n) "\n")
					      ((eq match ?t) "\t")
					      (t "\\"))))
		      (setq value (replace-match replacement t t value)))
		    (setq pos (1+ (match-beginning 0)))))
	        (insert value))
	      ((eq name 'insert-file)
		(forward-char (car (cdr (insert-file-contents value)))))
	      ((eq name 'eval)
	        (and (stringp value)
		     (eval (car (read-from-string value)))))
	      ((eq name 'ignore-status)
	        (setq ignore-status-p t))
	      ((eq name 'do)
	        (rgr-html-execute-do value nil ignore-status-p)
	        (setq ignore-status-p nil))
	      ((eq name 'insert-do)
	        (rgr-html-execute-do value t ignore-status-p)
	        (setq ignore-status-p nil))
	      (t
		(error "Unknown parameter '%s' in <!--boilerplate-->." name)))
	;; Restore point (eval may have changed it) and do the next one.
	(goto-char (1- end))
	(setq attributes (cdr attributes))))
    (if rescan-p
	;; Recursively update the stuff we just inserted, as if it was a buffer
	;; in its own right.  We must do this recursively and under restriction
	;; so that nesting works properly.
	(save-excursion
	  (save-restriction
	    (narrow-to-region start (point))
	    (rgr-html-update-all-boilerplate))))
    (pop-mark)
    ;; Move past the <!--/boilerplate-->.
    (forward-sexp)))

;;;###autoload
(defun rgr-html-update-buffer-boilerplate ()
  "Redoes all the boilerplate in the buffer (or the currently restricted
portion of it)."
  (interactive)
  (let ((start (point))
	(rgr-html-flush-transcript-p rgr-html-flush-transcript-p))
    ;; no save-excursion so we can display errors.
    (rgr-html-update-all-boilerplate)
    (goto-char start)))

;;;###autoload
(defun rgr-html-update-boilerplate ()
  "Update this (or the next) <!--boilerplate--> construct."
  (interactive)
  (let ((rgr-html-flush-transcript-p rgr-html-flush-transcript-p))
    (cond ((not (re-search-forward rgr-html-boilerplate-tag-re nil t))
	    (error "No more <!--boilerplate--> in this buffer."))
	  ((match-beginning 1)
	    ;; This is an end tag (after looking forward), so we're inside what we
	    ;; want to update.
	    (goto-char (match-beginning 0))
	    (or (re-search-backward rgr-html-boilerplate-tag-re nil t)
		(error "Unmatched <!--/boilerplate-->."))
	    (and (match-beginning 1)
		 (error "Duplicate <!--/boilerplate-->."))
	    (goto-char (match-end 0))
	    (rgr-html-update-boilerplate-internal))
	  (t
	    ;; at the start of the next one.
	    (rgr-html-update-boilerplate-internal)))))

;; [still working on this.  based on the batch-byte-compile and
;; batch-byte-compile-file functions.  -- rgr, 27-Jan-98.]
;;;###autoload
(defun rgr-html-batch-update-boilerplate ()
  (if (not noninteractive)
      (error "Use `rgr-html-batch-update-boilerplate' only with -batch"))
  (let ((exit-code 0)
	;; Don't try to ask version control questions.  [might want to shut off
	;; version control altogether . . . but somebody might want to do batch
	;; update of an editable source file.  -- rgr, 8-Apr-98.]
	(delete-old-versions t))
    (while command-line-args-left
      (let ((file (prog1 (car command-line-args-left)
		    (setq command-line-args-left (cdr command-line-args-left))))
	    (output-file-name
	      (and (equal (car command-line-args-left) "-o")
		   (prog1 (car (cdr command-line-args-left))
		     (setq command-line-args-left
			   (cdr (cdr command-line-args-left)))))))
	(condition-case err
	    (save-excursion
	      (set-buffer (find-file-noselect file))
	      (cond ((not buffer-read-only)
		      ;; usual case.
		      (rgr-html-update-buffer-boilerplate))
		    (output-file-name
		      ;; OK to modify if we're writing it someplace else.
		      (setq buffer-read-only nil)
		      (rgr-html-update-buffer-boilerplate))
		    (t
		      (message "%s is read-only; skipping." file)))
	      (cond ((not (buffer-modified-p))
		      ;; no boilerplate, or was read-only.
		      )
		    (output-file-name
		      (html-helper-update-timestamp)
		      (write-file output-file-name))
		    (t
		      (html-helper-update-timestamp)
		      (save-buffer))))
	  (error
	    (message "Got error %S" err)
	    (setq exit-code 1)))))
    ;; bug out of here.
    (kill-emacs exit-code)))

;;;###autoload
(defun rgr-html-delete-boilerplate ()
  ;; Remove all boilerplate without updating.
  (interactive)
  (let ((count 0))
    (goto-char (point-min))
    (while (re-search-forward rgr-html-boilerplate-tag-re nil t)
      (if (match-beginning 1)
	  (error "Unmatched <!--/boilerplate--> tag."))
      ;; (rgr-html-update-boilerplate-internal)
      (goto-char (match-beginning 0))
      (let ((start (save-excursion
		     (forward-sexp 1);; go to end of <!boilerplate...> tag.
		     (point))))
	;; move past matching <!--/boilerplate-->, then back before it.
	(rgr-html-forward-markup 1 rgr-html-boilerplate-tag-re t)
	(forward-sexp -1)
	;; start and (point) are now just inside the boilerplate construct.
	(delete-region start (point))
	;; and skip the <!boilerplate...> tag.
	(forward-sexp 1)
	(setq count (1+ count))))
    (message "Deleted %d top-level <!--boilerplate--> markup section%s."
	     count (if (= count 1) "" "s"))))

(provide 'rgr-html-boilerplate)
