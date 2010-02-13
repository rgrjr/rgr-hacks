;;;****************************************************************************
;;;
;;;    The tags-grep command.
;;;
;;; [created.  -- rgr, 5-Feb-06.]
;;;
;;; $Id$

(defvar tags-grep-command nil)

(defun tags-grep-compute-defaults ()
  (unless grep-command
    (grep-compute-defaults))
  (unless tags-grep-command
    (setq tags-grep-command
	  (format "xargs %s" grep-command))))

(defun tags-grep-get-files ()
  ;; Initialize the list from the tags table.
  (let ((next-file-list nil))
    (save-excursion
      ;; Visit the tags table buffer to get its list of files.
      (visit-tags-table-buffer)
      ;; Copy the list so we can setcdr below, and expand the file
      ;; names while we are at it, in this buffer's default directory.
      (setq next-file-list (mapcar 'expand-file-name (tags-table-files)))
      ;; Iterate over all the tags table files, collecting
      ;; a complete list of referenced file names.
      (while (visit-tags-table-buffer t)
	;; Find the tail of the working list and chain on the new
	;; sublist for this tags table.
	(let ((tail next-file-list))
	  (while (cdr tail)
	    (setq tail (cdr tail)))
	  ;; Use a copy so the next loop iteration will not modify the
	  ;; list later returned by (tags-table-files).
	  (if tail
	      (setcdr tail (mapcar 'expand-file-name (tags-table-files)))
	    (setq next-file-list (mapcar 'expand-file-name
					 (tags-table-files)))))))
    next-file-list))

;;;###autoload
(defun tags-grep (command)
  (interactive
    (progn
      (tags-grep-compute-defaults)
      (list (read-from-minibuffer "Run grep on tags file (like this): "
				  tags-grep-command nil nil
				  'tags-grep-history))))
  (let ((files (tags-grep-get-files))
	(temp-file "tags-grep-files.tmp")
	(temp-buffer nil))
    (unwind-protect
	 (with-current-buffer (setq temp-buffer (generate-new-buffer temp-file))
	   (let ((tail files))
	     (while tail
	       ;; the file-readable-p test is necessary because etags seems to
	       ;; mess up the file names somehow.  -- rgr, 5-Feb-06.
	       (if (file-readable-p (car tail))
		   (insert (car tail) "\n"))
	       (setq tail (cdr tail))))
	   (write-region (point-min) (point-max) temp-file))
      (kill-buffer temp-buffer))
    (message "Got %s files." (length files))
    (grep (format "%s < %s" command temp-file))))

(provide 'tags-grep)
