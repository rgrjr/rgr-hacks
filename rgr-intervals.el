;;;*****************************************************************************
;;;
;;;;   Miscellaneous interval commands.
;;;
;;;    [old] Modification history:
;;;
;;; added rgr-print-interval, rgr-parse-date.  -- rgr, 2-Mar-94.
;;; make rgr-insert-computed-interval use discus-parse-date.  -- rgr, 28-Mar-94.
;;; rgr-compute-interval: newer rgr-insert-computed-interval.  -- rgr, 4-Nov-94.
;;; rgr-compute-interval: handle "via <term>" clause.  -- rgr, 24-Jan-95.
;;; rgr-compute-interval: allow "effectively in".  -- rgr, 31-May-95.
;;; split out of ./rgr-hacks.el file.  -- rgr, 18-Mar-96.
;;; rgr-compute-interval: better nesting, "out w/o in" err.  -- rgr, 24-Apr-96.
;;; rgr-relabel-time-intervals: new hack.  -- rgr, 16-Feb-97.
;;; rgr-add-times: new.  -- rgr, 15-Mar-98.
;;; rgr-compute-region-interval: new.  -- rgr, 17-Jan-00.
;;; fix compiler warning.  -- rgr, 18-Jan-00.
;;; rgr-weekly-intervals, plus related reorg.  -- rgr, 27-Mar-01.
;;; rgr-sum-region-intervals: new.  -- rgr, 15-Apr-01.
;;; rgr-date-lines-today: new fn.  -- rgr, 12-Dec-02.
;;; rgr-date-lines-today: add autoload.  -- rgr, 9-Jan-03.
;;;
;;; $Id$

(defun rgr-make-interval-field (value)
  ;; returns a 2-digit string.  value must be < 100!
  (concat (list (+ (/ value 10) ?0) (+ (% value 10) ?0))))

(defun rgr-print-interval (seconds &optional force-hours)
  "This also gives you HH:MM if you give it a value in minutes, as
long as hours < 60."
  (let* ((mins (/ seconds 60))
	 (secs (% seconds 60))
	 (hours (+ (/ mins 60)
		   (if (numberp force-hours)
		       ;; this avoids emacs integer overflow.
		       force-hours
		       0)))
	 (mins (% mins 60)))
    ;; (message "%S -> %S, %S, %S" seconds hours mins secs)
    (if (or force-hours (> hours 0))
	(concat (format "%s:" hours)
		(rgr-make-interval-field mins)
		":"
		(rgr-make-interval-field secs))
	(concat (format "%s:" mins)
		(rgr-make-interval-field secs)))))

(defun rgr-print-time (seconds &optional suppress-seconds-p)
  "Print the time as h:mm:ss, with optional minus sign."
  (let ((minus-p (< seconds 0)))
    (if minus-p
	(setq seconds (- seconds)))
    (let* ((mins (/ seconds 60))
	   (secs (% seconds 60))
	   (hours (/ mins 60))
	   (mins (% mins 60)))
      (concat (if minus-p "-" "")
	      (format "%d" hours)
	      ":"
	      (rgr-make-interval-field mins)
	      (if (not suppress-seconds-p)
		  (concat ":" (rgr-make-interval-field secs)))))))

(defun add-time (t1 t2)
  "Add two internal times."
  (let* ((lsw-sum (+ (car (cdr t1)) (car (cdr t2))))
	 (lsw (mod lsw-sum 65536))
	 (carry (/ lsw-sum 65536)))
    (list (+ (car t1) (car t2) carry) lsw)))

;;;###autoload
(defun rgr-compute-region-interval (start end &optional insert-p)
  ;; helper for rgr-weekly-intervals & rgr-compute-interval fns.
  (interactive "r\nP")
  (require 'time-date)
  ;; This operates as a state machine with two (effectively infinite) parallel
  ;; series of states, [I0, I1, I2, I3, ...] and [O0, O1, O2, O3 ...].  The I
  ;; states corresond to "effectively in" and the O states to "effectively out".
  ;; The numeric part of the state is carried by the sigma-signs counter; it
  ;; reflects the number of concurrent logins at a given point.  If two or more
  ;; ordinary "in" or "out" transitions in a row happen for the same host and
  ;; terminal within 10 seconds, all but the first are considered redundant and
  ;; are ignored.  The "redundancy" processing is necessary because KDE login
  ;; seems to run the .bash_login file multiple times; different KDE versions
  ;; stutter to different extents.
  ;;
  ;; The state transition logic is as follows:
  ;;
  ;;    1.  Start in the I0 state.
  ;;
  ;;    2.  Ordinary login increments the numeric portion without affecting the
  ;; in/out bit.  The transition from I0 to I1 causes the associated event time
  ;; to be recorded in pending-login-time.
  ;;
  ;;    3.  Similarly for ordinary logout, the numeric portion is decremented,
  ;; and the transition from I1 to I0 causes the interval between
  ;; pending-login-time and the event time to be accumulated in total.
  ;;
  ;;    4.  Logging "effectively out" transitions between I_n to O_n, for any n,
  ;; and accumulates the pending interval if (plusp n).  It is an error if we
  ;; are already in an O state (since we don't keep track of the "effective"
  ;; depth).
  ;;
  ;;    5.  Finally, logging "effectively in" transitions between O_n to I_n,
  ;; for any n, and records the event time in pending-login-time if (plusp n).
  ;;
  ;;    6a:  If we end in the I0 state, or any O state, then we report the exact
  ;; accumulated interval.
  ;;
  ;;    6b.  Otherwise, the final state is I_n where (plusp n); accumulate the
  ;; interval between pending-login-time and now, and mention that in the
  ;; generated message.
  (let ((sigma-signs 0) (last nil) (effectively-out nil)
	(total nil) (pending-login-time nil))
    ;; No save-excursion so that if we get a syntax error, the user gets to
    ;; see where (but still has to guess within dates.)
    (goto-char start)
    (while (re-search-forward "^Logged " end t)
      (let* ((effectively-p nil)
	     (sign
	      (let ((direction (read (current-buffer))))
		(cond ((eq direction 'effectively)
			(setq effectively-p t)
		        (setq direction (read (current-buffer)))))
		(cond ((eq direction 'in)
			+1)
		      ((not (eq direction 'out))
			(error "Expected 'in' or 'out', but got '%s'."
			       direction))
		      ((= sigma-signs 0)
			(error "Logout without matching login."))
		      (t
			-1)))))
	(cond (effectively-p
		;; check consistency.
		(if (and (= sign 1) (not effectively-out))
		    (error "'Effectively in' without prior 'effectively out'."))
		(if (and (= sign -1) effectively-out)
		    (error "Duplicate 'effectively out'."))))
	(if (not (eq (read (current-buffer)) 'on))
	    (error "Expected 'on'."))
	(let ((machine (read (current-buffer)))
	      (at (read (current-buffer)))
	      (terminal nil) (day nil) (time nil)
	      (accumulate-p nil) (this nil))
	  (if (eq at 'via)
	      (setq terminal (read (current-buffer))
		    at (read (current-buffer))))
	  (if (not (eq at 'at))
	      (error "Expected 'at'."))
	  (setq day (read (current-buffer)))
	  (setq time (date-to-time
		       (buffer-substring
		         (point)
		         (progn (end-of-line) (point)))))
	  '(message "Got %s%s %s on %s"
		   sign (if effectively-p " (eff)" "") time machine)
	  (or effectively-p
	      (setq this (list time sign machine terminal)))
	  ;; Perform state transition.  We only count times if transitioning
	  ;; between I0 and I1, or I_n and O_n for n>0.  (Note that the
	  ;; illegal zero-to-minus-one case has already been taken care of;
	  ;; sigma-signs is never negative.)  -- rgr, 24-Apr-96.  [changed
	  ;; from "never positive."  -- rgr, 16-Feb-05.]
	  (cond (effectively-p
		  (if (and (> sigma-signs 0) (= sign +1))
		      ;; O_n => I_n for n>0.
		      (setq pending-login-time time))
		  (setq effectively-out (not effectively-out))
		  ;; check for I_n => O_n for n>0.
		  (setq accumulate-p (and (= sign -1) (> sigma-signs 0))))
		((and last
		      (equal (cdr this) (cdr last))
		      (< (float-time
			   (subtract-time (car this) (car last)))
			 10))
		  ;; Ignore redundant entries; KDE generates these on login.
		  ;; (message "Redundant entry %S within 10s of %S" this last)
		  )
		(t
		  (if (zerop sigma-signs)
		      ;; I0 => I1.
		      (setq pending-login-time time))
		  (setq sigma-signs (+ sign sigma-signs))
		  (cond ((not (zerop sigma-signs)))
			(effectively-out
			  ;; special case:  full logout when effectively out;
			  ;; this means we shouldn't expect an "effectively in",
			  ;; so we can just reset.
			  (setq effectively-out nil))
			(t
			  ;; I1 => I0.
			  (setq accumulate-p t)))))
	  (if accumulate-p
	      ;; Accumulate this interval.
	      (let ((delta (subtract-time time pending-login-time)))
		(setq total (if total (add-time total delta) delta))
		(setq pending-login-time nil)))
	  ;; (message "%S => %S total" time total)
	  (setq last this))))
    (goto-char end)
    (cond ((not (or total pending-login-time))
	    (error "No intervals."))
	  ((zerop sigma-signs))
	  (pending-login-time
	    ;; Assume the current time.
	    (let ((delta (subtract-time (current-time) pending-login-time)))
	      (setq total (if total (add-time total delta) delta))))
	  ((not effectively-out)
	    (message "[oops; no %S or %S, but %S is %S.]"
		     'pending-login-time 'effectively-out
		     'sigma-signs sigma-signs)
	    (sit-for 2)))
    ;; (message "total %S" total)
    (let ((interval (rgr-print-interval (round (float-time total)))))
      (message "%s%s" interval
	       (cond ((zerop sigma-signs) "")
		     (effectively-out " (effectively)")
		     (t " (assuming the last interval ends now).")))
      (and insert-p (insert interval))
      interval)))

;;;###autoload
(defun rgr-compute-interval (insert-p)
  "Compute the total time from the login history in the preceding
paragraph.  Physically nested intervals (whether or not they are exactly
concurrent) are not double-counted.  With a numeric argument, inserts
the result.  Assumes that an 'open' interval ends now, but advises of
that fact."
  (interactive "P")
  (let ((end (point)))
    (backward-paragraph)
    (rgr-compute-region-interval (point) end insert-p)))

;;;; Summing time estimates.

;;;###autoload
(defun rgr-add-times (&optional insert-p)
  (interactive "P")
  (let ((original-point (point))
	(min-time 0) (max-time 0))
    (forward-paragraph -1)
    (while (re-search-forward "\t\\([0-9.]+\\)\\(-[0-9.]+\\)?\\([()?]+\\)?$"
			      original-point 'move)
      (let* ((min (car (read-from-string (match-string 1))))
	     (max (if (match-beginning 2)
		      (car (read-from-string (match-string 2) 1))
		      min)))
	(setq min-time (+ min-time min))
	(setq max-time (+ max-time max))))
    (let ((total-string (if (= min-time max-time)
			    (format "%s" min-time)
			    (format "%s-%s" min-time max-time))))
      (if insert-p
	  (insert total-string))
      (message "Total is %s" total-string))))

;;;; Date tags (e.g. on ~rogers/projects/random/bills.text lines).

;;;###autoload
(defun rgr-date-lines (date-string)
  "Put a date string at the start of each line from the current line to
the end of the paragraph.  The date is of the form 'MMDD' with leading
zeros, and defaults to today's date.  The lines must have leading
whitespace; this whitespace is tweaked so as not to disturb the
indentation of the rest of the line."
  (interactive
    (let* ((time-tail (nthcdr 3 (decode-time)))
	   (default (concat (rgr-make-interval-field (car (cdr time-tail)))
			    (rgr-make-interval-field (car time-tail))))
	   (date (read-string "Date string: " default default)))
      (list date)))
  (let ((n-lines 0))
    (beginning-of-line)
    (while (looking-at "^\\(    \\|\t\\)")
      (if (= (- (match-end 0) (match-beginning 0)) 4)
	  (replace-match date-string t t)
	  (insert date-string))
      (setq n-lines (1+ n-lines))
      (forward-line))
    (message "Dated %d line%s." n-lines (if (= n-lines 1) "" "s"))))

(provide 'rgr-intervals)
