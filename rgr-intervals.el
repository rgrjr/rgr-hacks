;;;*****************************************************************************
;;;
;;;;   Miscellaneous interval commands.
;;;
;;;    Modification history:
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

(defun rgr-make-interval-field (value)
  ;; returns a 2-digit string.  value must be < 100!
  (concat (list (+ (/ value 10) ?0) (+ (% value 10) ?0))))

(defun rgr-print-interval (seconds &optional force-hours)
  ;; This also gives you HH:MM if you give it a value in minutes, as
  ;; long as hours < 60.
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
  (let ((sigma-signs 0)
	(total nil) (pending-login-time nil))
    ;; No save-excursion so that if we get a syntax error, the user gets to
    ;; see where (but still has to guess within dates.)
    (goto-char start)
    (while (re-search-forward "^Logged " end t)
      (let ((sign
	      (let ((direction (read (current-buffer))))
		(if (eq direction 'effectively)
		    (setq direction (read (current-buffer))))
		(cond ((eq direction 'in)
			+1)
		      ((eq direction 'out)
			(if (= sigma-signs 0)
			    (error "Logout without matching login."))
			-1)
		      (t
			(error "Expected 'in' or 'out', but got '%s'."
			       direction))))))
	(if (not (eq (read (current-buffer)) 'on))
	    (error "Expected 'on'."))
	(let ((machine (read (current-buffer)))
	      (at (read (current-buffer)))
	      (terminal nil) (day nil))
	  (if (eq at 'via)
	      (setq terminal (read (current-buffer))
		    at (read (current-buffer))))
	  (if (not (eq at 'at))
	      (error "Expected 'at'."))
	  (setq day (read (current-buffer)))
	  (let ((time (date-to-time
			(buffer-substring
			  (point)
			  (progn (end-of-line) (point))))))
	    ;(message "Got %s %s on %s" sign time machine)
	    ;; Only count times if transitioning to/from 0.  (Note that the
	    ;; illegal zero-to-minus-one case has already been taken care of;
	    ;; sigma-signs is never negative.)  -- rgr, 24-Apr-96.  [changed
	    ;; from "never positive."  -- rgr, 16-Feb-05.]
	    (if (zerop sigma-signs)
		(setq pending-login-time time))
	    (setq sigma-signs (+ sign sigma-signs))
	    (if (zerop sigma-signs)
		;; Accumulate this interval.
		(let ((delta (subtract-time time pending-login-time)))
		  (setq total (if total (add-time total delta) delta))
		  (setq pending-login-time nil)))
	    ;; (message "%S => %S total" time total)
	    ))))
    (goto-char end)
    (cond ((not (or total pending-login-time))
	    (error "No intervals."))
	  ((not (zerop sigma-signs))
	    ;; Assume the current time.
	    (let ((delta (subtract-time (current-time) pending-login-time)))
	      (setq total (if total (add-time total delta) delta)))))
    ;; (message "total %S" total)
    (let ((interval (rgr-print-interval (round (time-to-seconds total)))))
      (message "%s%s" interval
	       (if (zerop sigma-signs)
		   ""
		   " (assuming the last interval ends now)."))
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

;;;;

(defvar rgr-time-regexp
  (let ((digits "\\([0-9]+\\)"))
    (concat "-?" digits ":" digits "\\(:" digits "\\)?")))

(defun rgr-match-integer (field)
  (string-to-int (buffer-substring (match-beginning field) (match-end field))))

(defun rgr-extract-time ()
  (let* ((minus-p (eq (char-after (match-beginning 0)) ?-))
	 (hours (rgr-match-integer 1))
	 (mins (+ (* hours 60) (rgr-match-integer 2)))
	 (secs-p (match-beginning 3))
	 (secs (+ (* mins 60)
		  (if secs-p
		      (rgr-match-integer 4)
		      0))))
    (if minus-p (- secs) secs)))

(defun rgr-print-time (seconds &optional suppress-seconds-p)
  ;; Print the time as h:mm:ss, with optional minus sign.
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

(defun rgr-unlabel-time-intervals ()
  (interactive)
  (let ((regexp (concat "^\\(" rgr-time-regexp "\t\\)" rgr-time-regexp)))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
	(delete-region (match-beginning 1) (match-end 1))))))

(defun rgr-relabel-time-intervals ()
  (interactive)
  (let ((start-time nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward rgr-time-regexp nil t)
	(let ((secs-p (match-beginning 3))
	      (time (rgr-extract-time)))
	  (or start-time
	      (setq start-time time))
	  (let ((delta-time (- time start-time)))
	    '(message "%s (%d secs)."
		     (rgr-print-time delta-time) delta-time)
	    '(sit-for 1)
	    (cond ((not (eq (char-after (point)) ?/)))
		  ((progn (forward-char)
			  (not (looking-at rgr-time-regexp)))
		    (message "Warning:  found / but no resynch time?")
		    (sit-for 2))
		  (t
		    (setq start-time (- (rgr-extract-time) delta-time))))
	    (beginning-of-line)
	    (insert (rgr-print-time delta-time) "\t")
	    (forward-line)))))))

;; Summing printed intervals.

(defun rgr-sum-region-intervals (start end)
  "Sum and insert intervals reflecting 8-hour days."
  (interactive "r")
  (let* ((digits "\\([0-9]+\\)")
	 (interval-regexp (concat digits ":" digits ":" digits))
	 (secs-per-day (* 24 60 60))
	 (sum nil))
    (save-excursion
      (goto-char start)
      (while (re-search-forward interval-regexp end t)
	(let ((time (discus-parse-time (match-string 0))))
	  (setq sum (if sum
			(discus-date+ sum time)
			(cons (floor time secs-per-day)
			      (mod time secs-per-day)))))))
    (cond ((null sum)
	    (message "No intervals in the region."))
	  (t
	    (let ((secs-per-day (* 8 60 60))
		  (days (car sum))
		  (secs (cdr sum)))
	      (setq days (* days 3))
	      (if (> secs secs-per-day)
		  (setq days (+ days (floor secs secs-per-day))
			secs (mod secs secs-per-day)))
	      (insert (if (> days 0)
			  (format "%d-" days)
			  "")
		      (discus-print-time secs)))))))

(provide 'rgr-intervals)
