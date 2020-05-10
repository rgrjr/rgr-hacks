;;;****************************************************************************
;;;
;;;    Hacks for making and verifying backups.
;;;
;;; [created.  -- rgr, 6-Sep-17.]
;;;

(require 'cl-lib)

;;;###AUTOLOAD
(defun rgr-check-backup-md5sums ()
  "With point at the end of md5sum output, make sure all are paired."
  (interactive)
  (let ((files-from-md5 nil)
	(n-errors 0))
    (save-excursion
      ;; Skip backward past blank and checksum lines.
      (while (and (not (bobp))
		  (looking-at "^ *\\($\\|[0-9a-f]\\)"))
	(forward-line -1))
      ;; Move forward past blank lines.
      (while (looking-at "^$")
	(forward-line 1))
      ;; Scarf checksum-and-file lines.
      (while (looking-at "^ *\\([0-9a-f]+\\)  \\(.*\\)$")
	(let* ((md5sum (match-string-no-properties 1))
	       (file (match-string-no-properties 2))
	       (entry (assoc md5sum files-from-md5)))
	  (unless entry
	    (setq entry (list md5sum))
	    (push entry files-from-md5))
	  (push file (cdr entry)))
	(forward-line 1))
      (dolist (set files-from-md5)
	(when (null (cddr set))
	  (message "Found singleton %S" (car (cdr set)))
	  (cl-incf n-errors)))
      (if (= n-errors 0)
	  (message "No mismatches.")
	  (message "Found %d mismatches, check *Messages* for singletons."
		   n-errors)))))

(defvar rgr-backup-star-line
  (let ((digit "[0-9]"))
    (concat "^ \\([ *]\\) *" digit "+ \\([^ .]+\\)-l\\(" digit "\\)"))
  "Match the first part of a line of show-backups.pl output.")

;;;###AUTOLOAD
(defun rgr-update-backup-stars ()
  ;; [this isn't used much any more.  -- rgr, 6-Sep-17.]
  "Update the '*' prefixes in show-backups.pl output.
Starts from point and ends when we run out of backup description lines."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((current-level 10)
	  (current-backup ""))
      (while (looking-at rgr-backup-star-line)
	(let* ((star-p (equal (match-string 1) "*"))
	       (backup-name (match-string 2))
	       (level (string-to-number (match-string 3)))
	       (current-p
		 (or (equal current-backup backup-name)
		     (< level current-level))))
	  (if (not (eq current-p star-p))
	      (replace-match (if current-p "*" " ") t t nil 1))
	  (if current-p
	      (setq current-level level
		    current-backup backup-name))
	  ;; (message "current-p %S" current-p) (sit-for 1)
	  (forward-line))))))

(provide 'rgr-backup-hacks)
