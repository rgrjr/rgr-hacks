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
		(setq other-level (string-to-number (match-string 2)))
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
  (while (re-search-forward "^Backing up \\(/dev/[-a-z/0-9]+ ([^ ()]+)\\)" nil t)
    (let ((partition (match-string 1))
	  level date size)
      (forward-line 1)
      (and (looking-at "^  DUMP: WARNING: ")
	   ;; creating a new /etc/dumpdates file.
	   (forward-line 1))
      (and (looking-at " *DUMP: Date of this level \\([0-9]\\) dump: \\(.*\\)")
	   (setq level (string-to-number (match-string 1))
		 date (match-string 2)))
      (if (save-excursion
	    (re-search-forward "blocks (\\([0-9.]+\\)MB) on " nil t))
	  (setq size (car (read-from-string (match-string 1))))
	  (error "Can't find backup size; did it complete successfully?"))
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

(defvar rgr-backup-line-prefix-regexp
  (concat "^ \\([ .*]\\) *[0-9]+ \\([^ ]+-l\\([0-9]\\)\\)"
	  ;; This is to prove that it's about a backup file.
	  "\\(-cat\\)?\\."))

;;;###autoload
(defun rgr-backup-update-stars ()
  "Change \"*\" flags to \" \" for backups obsoleted by this one.
This is intended to work on the output of the show-backups.pl script; after
pasting in a new set of dumps, stars on some old ones must be changed."
  (interactive)
  (let ((last-prefix nil)
	(last-level nil)
	(n-lines-changed 0))
    (save-excursion
      (beginning-of-line)
      (while (looking-at rgr-backup-line-prefix-regexp)
	(let* ((prefix (match-string 2))
	       (level (string-to-number (match-string 3)))
	       (current-p (cond ((null last-level))
				((string-equal last-prefix prefix)
				  (<= level last-level))
				(t (< level last-level))))
	       (replacement (if current-p "*" " ")))
	  (cond ((not (equal (match-string 1) replacement))
		  (setq n-lines-changed (1+ n-lines-changed))
		  (replace-match replacement t t nil 1)))
	  (if current-p
	      (setq last-prefix prefix last-level level)))
	(forward-line 1))
      (message "Done; %d line%s changed."
	       n-lines-changed
	       (if (= n-lines-changed 1) "" "s"))
      (sit-for 1))))

(provide 'rgr-backup)

;; (setq debug-on-error t)
