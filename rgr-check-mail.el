;;;****************************************************************************
;;;
;;;    Mail checker.
;;;
;;;    rgr-check-mail can be invoked interactively, or may be put on a timer to
;;; be run in the background periodically.  See the function documentation for
;;; more info.
;;;
;;;    Modification history:
;;;
;;; rgr-check-mail: new hack.  -- rgr, 7-Feb-96.
;;; fix bug in rgr-default-mailbox default, improve doc.  -- rgr, 8-Feb-96.
;;; better rgr-default-mailbox default.  -- rgr, 26-Mar-96.
;;;

(require 'discus-date)

(defvar rgr-default-mailbox (concat rmail-spool-directory
				    (or (getenv "LOGNAME")
					(getenv "USER")))
  ;; rmail-spool-directory should be the right place whether or not the user
  ;; uses rmail.  -- rgr, 26-Mar-96.
  "*Default mailbox for rgr-check-mail to look at.")

(defconst rgr-unix-day-0 (car (discus-parse-date "Jan 1 1970"))
  "Figurative constant for converting Unix dates to Discus format.")

(defun rgr-convert-unix-date (unix-date)
  ;; Convert a Unix date (as reported by file-attributes or current-time) into
  ;; Discus format (which itself is a variation on Common Lisp "universal time"
  ;; format).  This function requires floating point arithmetic, so it can't
  ;; work in emacs 18.
  (let* ((seconds (+ (* (car unix-date) 65536.0)
		     (car (cdr unix-date))
		     ;; [timezone fudge factor.  this is only because discus
		     ;; can't deal with timezones.  -- rgr, 7-Feb-96.]
		     (or (car (current-time-zone unix-date)) 0)))
	 (day (floor (/ seconds 86400.0)))
	 (secs (round (mod seconds 86400.0))))
    (cons (+ day rgr-unix-day-0) secs)))

;;;###autoload
(defun rgr-check-mail (&optional mailbox-file-names)
  "Check to see if there is new mail.
Interactively, the default mailbox is checked (in the rgr-default-mailbox
variable); a numeric argument prompts for a new file name, but the default
is not changed.  The function itself takes an optional list of inbox file
names, and can be run periodically via the run-at-time function:

   (run-at-time \"10 sec\" 300 'rgr-check-mail)

This starts checking the default mailbox file ten seconds from now,
allowing your .emacs file to finish loading, and every 5 minutes (300
seconds) thereafter.  Note that by defaulting the file names argument,
rgr-check-mail picks up the default from rgr-default-mailbox, which must
be a single mailbox, each time it runs.  For multiple mailboxes, do

   (run-at-time \"10 sec\" 300 'rgr-check-mail
		'(\"/usr/spool/mail/rogers\" \"/usr/spool/mail/psa\"))

Either form can be halted by invoking:

   (cancel-function-timers 'rgr-check-mail)"
  (interactive
    (list (if current-prefix-arg
	      (list (read-file-name "Mailbox file name: "
				    (file-name-directory rgr-default-mailbox)
				    rgr-default-mailbox))
	      (list rgr-default-mailbox))))
  (or mailbox-file-names
      (setq mailbox-file-names (list rgr-default-mailbox)))
  (let ((new nil) (pathname nil) (count 0)
	(tail mailbox-file-names))
    (while tail
      (let* ((attributes (file-attributes (car tail)))
	     ;; this is the modification time.
	     (date (nth 5 attributes)))
	(cond ((and attributes
		    ;; check length
		    (> (nth 7 attributes) 0))
		(setq new date)
		(setq pathname (car tail))
		(setq count (1+ count)))))
      (setq tail (cdr tail)))
    '(if (> count 1)
	(setq new count));; for checking
    (cond ((eq new ':error)
	    (message "Error checking mail file write date."))
	  ((null new)
	    (if (interactive-p)
		(message "No new mail.")))
	  ((> count 1)
	    (message "Mail in %d mailboxes" count))
	  (t
	    (message "Mail for %s at %s"
		     (file-name-nondirectory pathname)
		     (discus-print-date (rgr-convert-unix-date new)))))
    (and new
	 (sit-for 2))))

(provide 'rgr-check-mail)

;; Debugging hacks.
;; (cancel-function-timers 'rgr-check-mail)
;; (rgr-check-mail '("/usr/spool/mail/rogers" "/usr/spool/mail/psa"))
;; Also tried "/usr/spool/mail/psa", but that turned out to be too cumbersome.
;; (run-at-time "10 sec" 120 'rgr-check-mail '("/usr/spool/mail/rogers"))
