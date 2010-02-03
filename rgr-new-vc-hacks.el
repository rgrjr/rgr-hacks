;;;; Hacks for version control.
;;;
;;; [created.  -- rgr, 20-Jul-08.]
;;;
;;; $Id: rgr-cvs-hacks.el 420 2008-07-20 04:10:13Z rogers $

(require 'vc)

(eval-when-compile
  (require 'vc)
  (require 'vc-dir)
  (require 'log-edit))

(if (not (rgr-emacs-version-p 23))
    (error "Loading %S, which only works in Emacs 23.x." load-file-name))

(defvar vc-log-extra)
(defvar vc-log-fileset)

;;;###autoload
(defun vc-log-insert-fileset-skeleton ()
  (interactive)
  ;; [should check to see if some of these aren't already in the list, and
  ;; insert only new ones.  -- rgr, 20-Jul-08.]
  (let* ((tail (log-edit-files))
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
      (setq tail (cdr tail)))
    (goto-char (point-min))
    (forward-line)))

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

;;;###autoload
(defun vc-split-entry ()
  "Split a log edit buffer into two at point.
Lines before point stay in the current buffer, lines after point (including
the current line if not at BOL) go into a new buffer, and each of these can
be committed independently."
  (interactive)
  (widen)
  (beginning-of-line)
  (let* ((original-buffer (current-buffer))
	 (split-point (point))
	 (original-end (point-max))
	 (files-before (or (rgr-vc-all-comment-files nil split-point)
			   (error "No files before point.")))
	 (files-after (or (rgr-vc-all-comment-files split-point nil)
			  (error "No files after point."))))
    ;; take care of the new buffer first.
    (let ((new-buffer (vc-create-new-log-buffer))
	  (log-operation vc-log-operation)
	  ;; (log-file vc-log-file)
	  ;; (log-version vc-log-version)
	  (after-hook vc-log-after-operation-hook)
	  (tmp-vc-parent-buffer vc-parent-buffer)
	  (tmp-vc-log-extra vc-log-extra)
	  (tmp-vc-parent-buffer-name vc-parent-buffer-name))
      (pop-to-buffer new-buffer)
      ;; [based on vc-start-logentry; should consolidate.  -- rgr, 27-May-06.]
      (set (make-local-variable 'vc-parent-buffer) tmp-vc-parent-buffer)
      (set (make-local-variable 'vc-parent-buffer-name)
	   tmp-vc-parent-buffer-name)
      (set (make-local-variable 'vc-log-extra) tmp-vc-log-extra)
      (message "[new buffer extra is %S]" vc-log-extra)
      ;; (if log-file (vc-mode-line log-file))
      (vc-log-edit files-after)
      (message "[new buffer fileset is %S]" vc-log-fileset)
      (make-local-variable 'vc-log-after-operation-hook)
      (if after-hook
	  (setq vc-log-after-operation-hook after-hook))
      (setq vc-log-operation log-operation)
      ;; (setq vc-log-version log-version)
      (insert-buffer-substring original-buffer split-point original-end))
    ;; now update the old one.
    (save-excursion
      (set-buffer original-buffer)
      (delete-region split-point original-end)
      (setq vc-log-fileset files-before)
      ;; [would be cleaner for vc-log-edit to do this in the first place.  --
      ;; rgr, 20-Jul-08.]
      (setq log-edit-listfun '(lambda () vc-log-fileset))
      (message "[old buffer fileset is %S]" vc-log-fileset))))

;;;; Hacked code.

;; This hacks definitions from lisp/vc.el rev 1.697.

(defun vc-deduce-fileset (&optional observer allow-unregistered
				    state-model-only-files)
  "Deduce a set of files and a backend to which to apply an operation.

Return (BACKEND FILESET FILESET-ONLY-FILES STATE CHECKOUT-MODEL).
If we're in VC-dir mode, the fileset is the list of marked files.
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
				  (eq major-mode 'vc-dir-mode))))
      (progn                  ;FIXME: Why not `with-current-buffer'? --Stef.
	(set-buffer vc-parent-buffer)
	(vc-deduce-fileset observer allow-unregistered state-model-only-files)))
     ((not buffer-file-name)
       (error "Buffer %s is not associated with a file" (buffer-name)))
     ((and allow-unregistered (not (vc-registered buffer-file-name)))
      (if state-model-only-files
	  (list (vc-responsible-backend
		 (file-name-directory (buffer-file-name)))
		(list buffer-file-name)
		(list buffer-file-name)
		(when state-model-only-files 'unregistered)
		nil)
	(list (vc-responsible-backend
	       (file-name-directory (buffer-file-name)))
	      (list buffer-file-name))))
     (t (error "No fileset is available here.")))))

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

;;;; Installation.

;;;###autoload
(defun new-vc-install-log-edit-mode-keys ()
  (define-key log-edit-mode-map "\C-xvK" 'vc-log-insert-fileset-skeleton)
  (define-key log-edit-mode-map "\C-xvU" 'vc-log-update-fileset-from-skeleton))

;;;###autoload
(defun rgr-new-vc-install-vc-dir-mode-keys ()
  (define-key vc-dir-mode-map "n" 'vc-dir-next-interesting-line)
  (define-key vc-dir-mode-map "p" 'vc-dir-previous-interesting-line))

(provide 'rgr-new-vc-hacks)
