;;;; Hacking backups.
;;;
;;; [created.  -- rgr, 1-May-04.]
;;;
;;; $Id$:

(defvar rgr-backup-log-file-name
        (expand-file-name "~/projects/linux/backup/backups.text")
  "*Name of file for recording backups.")

(defun rgr-backup-change-stars-to-dots (level)
  ;; change "*" to "." for backups obsoleted by this one.
  (let ((other-level nil))
    (beginning-of-line)
    (while (and (looking-at "^[ \t]*\\([^ \t\n]\\)[ \t]*\\([0-9]\\) ")
		(setq other-level (string-to-int (match-string 2)))
		(<= level other-level))
      ;; [normally, the flag char is one of ".*x".  -- rgr, 1-Jun-05.]
      ;; (message "[other-level %s, flag char %S]" other-level (match-string 1))
      (if (equal (match-string 1) "*")
	  (replace-match "." t t nil 1))
      (forward-line -1))))

;;;###autoload
(defun rgr-add-backup-log-entry ()
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward "^Backing up \\(/dev/[a-z0-9]+ ([^ ()]+)\\)" nil t)
    (let ((partition (match-string 1))
	  level date size)
      (forward-line 1)
      (and (looking-at "^  DUMP: WARNING: ")
	   ;; creating a new /etc/dumpdates file.
	   (forward-line 1))
      (and (looking-at " *DUMP: Date of this level \\([0-9]\\) dump: \\(.*\\)")
	   (setq level (string-to-int (match-string 1))
		 date (match-string 2)))
      (and (save-excursion
	     (re-search-forward "blocks (\\([0-9.]+\\)MB) on " nil t))
	   (setq size (car (read-from-string (match-string 1)))))
      '(message "[got level %S date %S size %S for part %S]"
	       level date size partition)
      (save-excursion
	(set-buffer (find-file-noselect rgr-backup-log-file-name))
	(goto-char (point-min))
	(or (re-search-forward (concat "^" partition ":$") nil t)
	    (error "Can't find partition %S in file %S."
		   partition rgr-backup-log-file-name))
	(forward-paragraph)
	(let ((rounded-size (cond ((>= (round size) 10)
				    (format "%dMB" (round size)))
				  ((>= (round size) 1)
				    (format "%.1fMB" size))
				  (t
				    (format "%dKB" (round (* size 1000)))))))
	 (insert (make-string (+ level 2) ?\ )
		 "*" (make-string (- 11 level) ?\ )
		 (format "%d" level) " " date " (" rounded-size ")\n"))
	(forward-line -2)
	(rgr-backup-change-stars-to-dots level)))))

(provide 'rgr-backup)

;; (setq debug-on-error t)
