;;;; Taking RPM files out of tripwire reports.
;;;
;;;    Modification history:
;;;
;;; created.  -- rgr, 24-Aug-02.
;;; use "\n\n" instead of forward/backward-paragraph cmds.  -- rgr, 12-Oct-02.
;;;

(defun rgr-make-rpm-file-hash (rpm-names)
  (let ((hash (make-vector 100 0)))
    (save-excursion
      (set-buffer (get-buffer-create "*rpm files*"))
      (buffer-disable-undo)
      (while rpm-names
	(let ((rpm (car rpm-names)))
	  (erase-buffer)
	  (call-process "rpm" nil t nil
			"-ql" rpm)
	  (goto-char (point-min))
	  (while (not (eobp))
	    (let ((line (buffer-substring (point)
					  (progn (end-of-line)
						 (point)))))
	      (set (intern line hash) t))
	    (forward-line))
	  (setq rpm-names (cdr rpm-names)))))
    hash))

(defun rgr-flush-rpm-differences (&optional packages)
  (let* (;; testing version
	 ;; (packages '("libpng" "libpng-devel"))
	 (hash (rgr-make-rpm-file-hash packages))
	 (n-added 0) (n-deleted 0) (n-changed 0) (n-changed-blocks 0))
    (while (not (eobp))
      (cond ((looking-at "^\\(added\\|changed\\|deleted\\):.* \\([^ ]+\\)$")
	      (let ((entry (intern-soft (match-string 2) hash))
		    (status (match-string 1)))
		(cond ((and entry
			    (boundp entry)
			    (symbol-value entry))
			;; (message "hit %s %s" status entry) (sit-for 1)
			(delete-region (point)
				       (progn (forward-line)
					      (point)))
		        (forward-line -1)
		        (cond ((equal status "added")
				(setq n-added (1+ n-added)))
			      ((equal status "changed")
				(setq n-changed (1+ n-changed)))
			      ((equal status "deleted")
				(setq n-deleted (1+ n-added))))))))
	    ((looking-at "^/[^ ]+$")
	      (let ((entry (intern-soft (match-string 0) hash)))
		(cond ((and entry
			    (boundp entry)
			    (symbol-value entry))
			(goto-char (match-beginning 0))
		        (let ((start (point)))
			  (re-search-forward "\n\n")
			  ;; (message "hit 'changed' %s" entry) (sit-for 1)
			  '(let ((end (point)))
			    (backward-paragraph 1)
			    (re-search-forward "\n\n")
			    (delete-region (point) end))
			  (delete-region start (point)))
		        (forward-line -1)
		        (setq n-changed-blocks (1+ n-changed-blocks)))))))
      (forward-line))
    (message "Done; found %d added, %d deleted, %d changed (%d blocks) total."
	     n-added n-deleted n-changed n-changed-blocks)))

;; (rgr-flush-rpm-differences '("glibc-devel" "glibc" "unzip" "gv" "tar"))
