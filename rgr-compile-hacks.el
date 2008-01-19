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
;;; [created, with the cdgrep command.  -- rgr, 4-Apr-94.]
;;;
;;; $Id$

(eval-when-compile
  (require 'rgr-lisp-hacks)
  (require 'compile))

(require 'compile)

(defvar compilation-buffer-compile-command nil)

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
	      (compilation-start compile-command))))
	;; Invoke compile afresh.
	(compile (if (or compilation-read-command current-prefix-arg)
		     (read-from-minibuffer "Compile command: "
					   compile-command nil nil
					   '(compile-history . 1))
		     compile-command)))))

;;;; Tag ends.

;;;###autoload
(defun rgr-compilation-mode-hook ()
  ;; [rgr-grep-annotate-current-buffer is getting to be a pain, as it insists on
  ;; reading all of the files that were hit, and doesn't always work very well
  ;; to begin with, and rgr-maybe-clean-c-compilation-buffer is moot, as I so
  ;; rarely code in C any more.  -- rgr, 7-Aug-03.]
  ;; [rgr-maybe-clean-c-compilation-buffer deleted.  -- rgr, 18-Jan-08.]
  '(mapcar (function (lambda (fn)
	    (or (member fn compilation-finish-functions)
		(setq compilation-finish-functions
		      (cons fn compilation-finish-functions)))))
	  '(rgr-grep-annotate-current-buffer))
  ;; Customize the window height.  This is 1/4 of the frame, but never less than
  ;; six lines.  [stolen from the rgr-rmail-mode-hook version.  -- rgr,
  ;; 10-Oct-00.]
  (setq compilation-window-height (max (/ (frame-height) 4) 6)))

(provide 'rgr-compile-hacks)
