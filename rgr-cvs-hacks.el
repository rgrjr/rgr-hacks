;;;; Hacks for CVS.
;;;
;;; [created.  -- rgr, 4-Dec-03.]
;;;
;;; $Id$

;;;###autoload
(defun rgr-cvs-recent-changes (&optional number-of-days)
  "Show a summary of 'cvs log' output for the last three days.  If you
give a C-u, it shows the last week's worth; if C-u C-u, then the last
month (30 days, actually).  Any other numeric argument shows the log for
that many days."
  (interactive "P")
  (require 'discus)
  (let* ((number-of-days
	   (cond ((integerp number-of-days) number-of-days)
		 ((not number-of-days) 3)
		 ((equal number-of-days '(4))
		   ;; control-U
		   7)
		 ((equal number-of-days '(16))
		   ;; control-U control-U
		   30)
		 ((consp number-of-days) (car number-of-days))
		 (t (error "got number-of-days %S" number-of-days))))
	 (today (car (discus-parse-date (current-time-string))))
	 (n-days-ago (discus-print-date (cons (- today number-of-days) nil))))
    ;; (error "Date '%s'." n-days-ago)
    (shell-command (format "cvs -q log -d '>%s' | cvs-chrono-log.pl"
			   n-days-ago))))

;;;###autoload
(defun rgr-cvs-insert-log-skeleton ()
  "Insert a '* filename:' line for each 'Index: filename' line in shell output.
This goes through the '*Shell Command Output*' buffer, which is assumed
to be the output of 'cvs diff', inserting lines at the end of the
current buffer, which is assumed to be something like a CVS log comment.
Files that are being added or deleted are noted as such."
  (interactive)
  (let ((original-buffer (current-buffer))
	(other-buffer
	  (or (get-buffer "*Shell Command Output*")
	      (error "Can't find '*Shell Command Output*' buffer.")))
	(match-re (concat "^Index: \\([^ \t\n]*\\)$"
			  "\\|^cvs \\(diff\\|server\\): \\([^ \t\n]*\\) "
			  "\\(is a new entry\\|was removed\\)")))
    (save-excursion
      (set-buffer other-buffer)
      (goto-char (point-min))
      (while (re-search-forward match-re nil t)
	(let ((file-name (or (match-string 1) (match-string 3)))
	      (extra (match-string 4)))
	  (message "found %S" file-name)
	  (save-excursion
	    (set-buffer original-buffer)
	    (goto-char (point-max))
	    (insert "* " file-name
		    (cond ((equal extra "was removed") " (deleted)")
			  (extra " (added)")
			  (t ""))
		    ":\n")))))
    ;; move point to start adding comments.
    (or (eobp)
	(forward-line 1))))

(defun rgr-cvs-plus ()
  "Insert a '  + ' at the beginning of the current line."
  (interactive)
  (cond ((not (bolp))
	  (save-excursion
	    (beginning-of-line)
	    (rgr-cvs-plus)))
	(t
	  (cond ((looking-at "^ *\\* ")
		  (insert "\n")
		  (forward-line -1)))
	  (insert "   + "))))

(define-key text-mode-map "\C-c+" 'rgr-cvs-plus)

(provide 'rgr-cvs-hacks)
