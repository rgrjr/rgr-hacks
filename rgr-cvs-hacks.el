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
    (shell-command (format "cvs log -d '>%s' | cvs-chrono-log.pl"
			   n-days-ago))))

(provide 'rgr-cvs-hacks)
