;;;; Hacks for version control.
;;;
;;; [created.  -- rgr, 20-Jul-08.]
;;;
;;; $Id: rgr-cvs-hacks.el 420 2008-07-20 04:10:13Z rogers $

(require 'vc)

(eval-when-compile
  (require 'cl)
  (require 'vc)
  (require 'vc-dir)
  (require 'log-edit))

(if (< emacs-major-version 23)
    (error "Loading %S, which only works in Emacs 23 and later."
	   load-file-name))

(defvar vc-log-extra)
(defvar vc-log-fileset)

;;;###autoload
(defun vc-log-insert-fileset-skeleton ()
  (interactive)
  ;; [should check to see if some of these aren't already in the list, and
  ;; insert only new ones.  -- rgr, 20-Jul-08.]
  (vc-log--insert-file-names (with-current-buffer (current-buffer)
			       (log-edit-files)))
  (goto-char (point-min))
  (forward-line))

(defun vc-log--insert-file-names (file-names)
  (let* ((tail file-names)
	 (prefix (expand-file-name default-directory))
	 (prefix-len (length prefix)))
    (while tail
      (let ((file (car tail)))
	(insert "* "
		(if (and (> (length file) prefix-len)
			 (string-equal (substring file 0 prefix-len) prefix))
		    ;; Drop the common prefix.
		    (substring file prefix-len)
		    ;; Oops; it's not so common after all.
		    file)
		":\n"))
      (setq tail (cdr tail)))))

;;;###autoload
(defun vc-log-refresh-fileset-skeleton ()
  (interactive)
  (let ((files (log-edit-files)))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\* +" nil t)
	(while (looking-at "\\([^ (),\t\n:]+\\)")
	  (let ((file (match-string-no-properties 1)))
	    (cond ((member file files)
		    (setq files (delete file files)))
		  (t
		    (replace-match "" t t))))
	  (goto-char (match-end 0))
	  ;; Skip stuff in parens.
	  (while (looking-at "[ \t\n]*(")
	    (forward-sexp 1))
	  (skip-chars-forward ", \t\n")
	  ;; We should now be poised at either the start of the next file name,
	  ;; or the terminating ":".
	  ))

      ;; Clean up empty "* :" lines.
      (goto-char (point-min))
      (while (re-search-forward "^\\* +:\n*" nil t)
	(replace-match "")))
    (when files
      ;; What's left over must be new.
      (goto-char (point-max))
      (vc-log--insert-file-names files))))

(defun vc-log-update-fileset-from-skeleton ()
  "Set the log buffer's fileset files to those named in the skeleton."
  (interactive)
  (let ((fileset-files (rgr-vc-all-comment-files)))
    (set (make-local-variable 'vc-log-fileset) fileset-files)
    (message "Updated fileset to %d files %S."
	     (length fileset-files) fileset-files)))

(defvar vc-log-buffer-gensym-index 0
  "Index of the last log buffer created.")

(defun vc-create-new-log-buffer ()
  "Create a new log buffer with a generated name.
This allows comments for multiple commits to be written in parallel, as long
as the files affected do not overlap.  The drawback is that, if the
vc-delete-logbuf-window option is not set, these log buffers will keep piling
up."
  ;; [there also ought to be a way of recycling these, and for finding the best
  ;; current logbuf for a given file.  having a "committed-p" flag in each
  ;; buffer would help with both.  -- rgr, 27-May-06.]
  (let ((buffer-name nil))
    (while (progn
	     (setq vc-log-buffer-gensym-index (1+ vc-log-buffer-gensym-index))
	     (setq buffer-name (format "*VC-log-%d*"
				       vc-log-buffer-gensym-index))
	     (get-buffer buffer-name))
      ;; try the next index.
      )
    (get-buffer-create buffer-name)))

(defun vc-log-buffer-for-file (file-name)
  "Given a FILE-NAME (which must be fully expanded), try to find a log
buffer for a pending commit to this file.  [but note that we can't rule
out log buffers that have already been committed, which may cause
problems.  -- rgr, 29-May-06.]"
  (let ((result nil)
	(buffer-tail (buffer-list)))
    (while buffer-tail
      (if (with-current-buffer (car buffer-tail)
	    (and (boundp 'vc-log-fileset)
		 (member file-name vc-log-fileset)))
	  (setq result (car buffer-tail)
		buffer-tail nil)
	  (setq buffer-tail (cdr buffer-tail))))
    result))

;;;; Hacked code.

;; This hacks definitions from lisp/vc.el rev 1.697.
;; [updated to git be7ed8...dd92c4.  -- rgr, 27-Dec-11.]

(defun vc-deduce-fileset (&optional observer allow-unregistered
				    state-model-only-files)
  "Deduce a set of files and a backend to which to apply an operation.

Return (BACKEND FILESET FILESET-ONLY-FILES STATE CHECKOUT-MODEL).
If we're in VC-dir mode, the fileset is the list of marked files.
Otherwise, if we're in dired-mode, return a fileset with the marked files
for a read-only operation (the optional OBSERVER flag is non-nil), else
signal an error.
Otherwise, if we're looking at a buffer visiting a version-controlled file,
the fileset is a singleton containing this file.
If none of these conditions is met, but ALLOW_UNREGISTERED is on and the
visited file is not registered, return a singleton fileset containing it.
Otherwise, throw an error.

STATE-MODEL-ONLY-FILES if non-nil, means that the caller needs
the FILESET-ONLY-FILES STATE and MODEL info.  Otherwise, that
part may be skipped.
BEWARE: this function may change the
current buffer."
  ;; FIXME: OBSERVER is unused.  The name is not intuitive and is not
  ;; documented.  It's set to t when called from diff and print-log.
  (let (backend)
    (cond
     ((derived-mode-p 'vc-dir-mode)
      (vc-dir-deduce-fileset state-model-only-files))
     ((derived-mode-p 'dired-mode)
      (if observer
	  (vc-dired-deduce-fileset)
	(error "State changing VC operations not supported in `dired-mode'")))
     ((setq backend (vc-backend buffer-file-name))
      (if state-model-only-files
	(list backend (list buffer-file-name)
	      (list buffer-file-name)
	      (vc-state buffer-file-name)
	      (vc-checkout-model backend buffer-file-name))
	(list backend (list buffer-file-name))))
     ;; [begin hack.  -- rgr, 20-Jul-08.]
     ((and (boundp 'vc-log-fileset)
	   ;; This works in log-edit-mode buffers.
	   vc-log-fileset)
       (list (vc-responsible-backend default-directory)
	     vc-log-fileset vc-log-fileset
	     ;; [shouldn't we inherit the buffer state?  -- rgr, 20-Jul-08.]
	     (when state-model-only-files 'unregistered) nil))
     ;; [end hack.  -- rgr, 20-Jul-08.]
     ((and (buffer-live-p vc-parent-buffer)
           ;; FIXME: Why this test?  --Stef
           (or (buffer-file-name vc-parent-buffer)
				(with-current-buffer vc-parent-buffer
				  (derived-mode-p 'vc-dir-mode))))
      (progn                  ;FIXME: Why not `with-current-buffer'? --Stef.
	(set-buffer vc-parent-buffer)
	(vc-deduce-fileset observer allow-unregistered state-model-only-files)))
     ((not buffer-file-name)
       (error "Buffer %s is not associated with a file" (buffer-name)))
     ((and allow-unregistered (not (vc-registered buffer-file-name)))
      (if state-model-only-files
	  (list (vc-backend-for-registration (buffer-file-name))
		(list buffer-file-name)
		(list buffer-file-name)
		(when state-model-only-files 'unregistered)
		nil)
	(list (vc-backend-for-registration (buffer-file-name))
	      (list buffer-file-name))))
     (t (error "No fileset is available here")))))

;;;; vc-dir hacks.

(defvar vc-ewoc)

(defun ewoc-goto-next-prev-p (ewoc arg)
  ;; The sign of arg determines the direction.  Returns true if we moved.
  (let ((loc (ewoc-locate ewoc)))
    (cond ((= arg 0))
	  ((>= arg 0) (ewoc-goto-next ewoc 1))
	  (t (ewoc-goto-prev ewoc 1)))
    (not (eq (ewoc-locate ewoc) loc))))

(defun vc-dir-next-interesting-line (arg)
  ;; not finished.  -- rgr, 24-Nov-09.
  "Go to the next line with a modified file on it.
If a prefix argument is given, move by that many lines."
  (interactive "p")
  (let* ((direction (or arg 1))
	 (distance (abs direction)))
    (if (and (< direction 0)
	     (save-excursion
	       (skip-chars-forward " \t\n")
	       (eobp)))
	;; Deal with a curious asymmetry of ewoc:  You can go past the last
	;; line, but not before the first.  Accordingly, this moves us to the
	;; line of the last entry.
	(ewoc-goto-prev vc-ewoc 1))
    (while (and (> distance 0)
		(ewoc-goto-next-prev-p vc-ewoc direction))
      ;; Make sure we're on an interesting line, if possible.
      (while (let ((loc (ewoc-locate vc-ewoc)))
	       (and loc
		    (let ((data (ewoc-data loc)))
		      (member (vc-dir-fileinfo->state data)
			      '(nil unregistered ignored)))
		    (ewoc-goto-next-prev-p vc-ewoc direction)))
	)
      (setq distance (1- distance)))
    (vc-dir-move-to-goal-column)))

(defun vc-dir-previous-interesting-line (arg)
  "Go to the previous line.
If a prefix argument is given, move by that many lines."
  (interactive "p")
  (vc-dir-next-interesting-line (- (or arg 1))))

(defun vc-dir-fileinfo->extension (data)
  (let ((name (vc-dir-fileinfo->name data)))
    (if (string-match "\\.[^./]*$" name)
	(match-string 0 name)
	"")))

(defun vc-dir-current-extension ()
  (let* ((current (or (ewoc-locate vc-ewoc)
		      (error "No current file/directory.")))
	 (data (ewoc-data current)))
    (vc-dir-fileinfo->extension data)))

(defun vc-dir-map-matching-files (extension state function)
  ;; First check that no directory is marked, we can't mark files in that case.
  (ewoc-map #'(lambda (filearg)
		(when (and (vc-dir-fileinfo->directory filearg)
			   (vc-dir-fileinfo->marked filearg))
		  (error "Cannot mark all files, directory `%s' marked"
			 (vc-dir-fileinfo->name filearg))))
	    vc-ewoc)
  (ewoc-map #'(lambda (filearg)
		(if (and (or (null extension)
			     (equal extension
				    (vc-dir-fileinfo->extension filearg)))
			 (or (null state)
			     (eq state (vc-dir-fileinfo->state filearg))))
		    (funcall function filearg)))
	    vc-ewoc))

(defun vc-dir-mark-unmark-matching-files (mark-p &optional state-p)
  "Mark all files with the same extension as the current one."
  (let* ((current (or (ewoc-locate vc-ewoc)
		      (error "No current file/directory.")))
	 (data (ewoc-data current))
	 (state (vc-dir-fileinfo->state data))
	 (registered-p (not (eq state 'unregistered))))
    (vc-dir-map-matching-files
      (vc-dir-fileinfo->extension data)
      (and state-p state)
      (if mark-p
	  #'(lambda (filearg)
	      (unless (or (vc-dir-fileinfo->marked filearg)
			  ;; Don't mark unregistered files unless we start on
			  ;; one.
			  (and registered-p
			       (eq (vc-dir-fileinfo->state filearg)
				   'unregistered)))
		(setf (vc-dir-fileinfo->marked filearg) t)
		t))
	  #'(lambda (filearg)
	      (when (vc-dir-fileinfo->marked filearg)
		(setf (vc-dir-fileinfo->marked filearg) nil)
		t))))))

(defun vc-dir-mark-matching-files (state-p)
  "Mark all files with the same extension as the current one."
  (interactive "P")
  (vc-dir-mark-unmark-matching-files t state-p))

(defun vc-dir-unmark-matching-files (state-p)
  "Unmark all files with the same extension as the current one."
  (interactive "P")
  (vc-dir-mark-unmark-matching-files nil state-p))

;;;; Installation.

;;;###autoload
(defun new-vc-install-log-edit-mode-keys ()
  (define-key log-edit-mode-map "\C-xvK" 'vc-log-insert-fileset-skeleton)
  (define-key log-edit-mode-map "\C-xvU" 'vc-log-update-fileset-from-skeleton))

;;;###autoload
(defun rgr-new-vc-install-vc-dir-mode-keys ()
  (define-key vc-dir-mode-map "\C-cm" 'vc-dir-mark-matching-files)
  (define-key vc-dir-mode-map "\C-cu" 'vc-dir-unmark-matching-files)
  (define-key vc-dir-mode-map "n" 'vc-dir-next-interesting-line)
  (define-key vc-dir-mode-map "p" 'vc-dir-previous-interesting-line))

(provide 'rgr-new-vc-hacks)
