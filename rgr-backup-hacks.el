;;;****************************************************************************
;;;
;;;    Hacks for making and verifying backups.
;;;
;;; [created.  -- rgr, 6-Sep-17.]
;;;

;;;###AUTOLOAD
(defun rgr-check-backup-md5sums ()
  (interactive)
  (let ((files-from-md5 nil)
	(n-errors 0))
    (save-excursion
      (while (looking-at "^$")
	(forward-line -1))
      (while (looking-at "^ *\\([0-9a-f]+\\)  \\(.*\\)$")
	(let* ((md5sum (match-string-no-properties 1))
	       (file (match-string-no-properties 2))
	       (entry (assoc md5sum files-from-md5)))
	  (unless entry
	    (setq entry (list md5sum))
	    (push entry files-from-md5))
	  (push file (cdr entry)))
	(forward-line -1))
      (dolist (set files-from-md5)
	(when (null (cddr set))
	  (message "Found singleton %S" (second set))
	  (incf n-errors)))
      (when (= n-errors 0)
	(message "No mismatches.")))))

(provide 'rgr-backup-hacks)
