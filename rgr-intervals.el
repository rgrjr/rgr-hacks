;;;*****************************************************************************
;;;
;;;;   Miscellaneous interval commands.
;;;
;;;    Modification history:
;;;
;;; added rgr-print-interval, rgr-parse-date.  -- rgr, 2-Mar-94.
;;; make rgr-insert-computed-interval use discus-parse-date.  -- rgr, 28-Mar-94.
;;; rgr-compute-t-fares .  -- rgr, 30-Mar-94.
;;; better rgr-compute-t-fares cost breakdown.  -- rgr, 1-Jul-94.
;;; rgr-compute-t-fares: generalized syntax.  -- rgr, 27-Jul-94.
;;; rgr-compute-interval: newer rgr-insert-computed-interval.  -- rgr, 4-Nov-94.
;;; rgr-compute-interval: handle "via <term>" clause.  -- rgr, 24-Jan-95.
;;; rgr-compute-interval: allow "effectively in".  -- rgr, 31-May-95.
;;; split out of ./rgr-hacks.el file.  -- rgr, 18-Mar-96.
;;; rgr-compute-t-fares: accept t for token.  -- rgr, 28-Mar-96.
;;; rgr-compute-interval: better nesting, "out w/o in" err.  -- rgr, 24-Apr-96.
;;; rgr-relabel-time-intervals: new hack.  -- rgr, 16-Feb-97.
;;; rgr-add-times: new.  -- rgr, 15-Mar-98.
;;; rgr-compute-region-t-fares: split out as separate cmd.  -- rgr, 2-Feb-99.
;;; rgr-compute-region-t-fares: extended to handle "#t" as commuter rail zone #.
;;;	(the fare table is incomplete, though.)  -- rgr, 31-Oct-99.
;;; rgr-compute-region-interval: new.  -- rgr, 17-Jan-00.
;;; fix compiler warning.  -- rgr, 18-Jan-00.
;;; rgr-compute-region-t-fares: new fares.  -- rgr, 5-Dec-00.
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
    (if (or force-hours (> hours 0))
	(concat (format "%s:" hours)
		(rgr-make-interval-field mins)
		":"
		(rgr-make-interval-field secs))
	(concat (format "%s:" mins)
		(rgr-make-interval-field secs)))))

(defun rgr-print-hours-and-tenths (seconds &optional force-hours)
  ;; Alternative format to rgr-print-interval (above).
  (let ((tenth-hours (+ (/ seconds 360)
			(if (numberp force-hours)
			    ;; this avoids emacs integer overflow.
			    (* force-hours 10)
			    0))))
    (format "%.1f" (/ tenth-hours 10.0))))

(defun rgr-princ-interval-padded (tag days secs pad-width)
  ;; helper for rgr-compute-region-interval
  (if (or (not (zerop days))
	  (not (zerop secs)))
      (let* ((string (rgr-print-interval secs (* 24 days)))
	     (pad (max (- pad-width (length string)) 0)))
	(princ (format "%s: %s%s\n"
		       tag (substring "           " 0 pad)
		       string))
	t)))

;; To keep the compiler happy.  Do (setq week-days 0 week-secs 0) for
;; interactive debugging.
(defvar week-days)
(defvar week-secs)

(defun psa-interval-finish-week ()
  ;; helper for rgr-compute-region-interval
  (and (rgr-princ-interval-padded "Week" week-days week-secs 11)
       (princ "\n"))
  (setq week-days 0 week-secs 0))

(defun psa-interval-finish-day (today days secs)
  ;; helper for rgr-compute-region-interval
  (rgr-princ-interval-padded today days secs 12)
  (setq week-days (+ week-days days))
  (setq week-secs (+ week-secs secs)))

;;;###autoload
(defun rgr-compute-region-interval (start end &optional insert-p print-daily-p)
  ;; helper for rgr-weekly-intervals & rgr-compute-interval fns.
  (interactive "r\nP")
  (let ((sigma-signs 0)
	(today nil)
	(days 0) (secs 0)
	(week-days 0) (week-secs 0)
	(total-days 0) (total-secs 0))
    ;; No save-excursion so that if we get a syntax error, the user gets to
    ;; see where (but still has to guess within dates.)
    (goto-char start)
    (while (re-search-forward "^Logged " end t)
      (let ((sign
	      (let ((direction (read (current-buffer))))
		(if (eq direction 'effectively)
		    (setq direction (read (current-buffer))))
		(cond ((eq direction 'in)
			-1)
		      ((eq direction 'out)
			(if (= sigma-signs 0)
			    (error "Logout without matching login."))
			+1)
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
	  (let ((time (discus-parse-date
			(buffer-substring
			  (point)
			  (progn (end-of-line) (point))))))
	    '(message "Got %s %s on %s" sign time machine)
	    ;; Only count times if transitioning to/from 0.  (Note that the
	    ;; illegal zero-to-one case has already been taken care of;
	    ;; sigma-signs is never positive.)  -- rgr, 24-Apr-96.
	    (cond ((or (zerop sigma-signs) (zerop (+ sign sigma-signs)))
		    ;; Maybe finish up on the previous interval.
		    (cond ((not print-daily-p))
			  ((null today)
			    ;; First complete interval on the first day.
			    (setq today day))
			  ((and (< sign 0)
				(not (eq day today)))
			    ;; Starting the first interval of a new day.
			    (psa-interval-finish-day today days secs)
			    (setq days 0 secs 0)
			    (if (not (memq day (memq today '(Mon Tue Wed Thu Fri Sat Sun))))
				;; new week.
				(psa-interval-finish-week))
			    (setq today day)))
		    ;; Accumulate this interval.
		    (setq days (+ days (* sign (car time))))
		    (setq secs (+ secs (* sign (cdr time))))
		    (setq total-days (+ total-days (* sign (car time))))
		    (setq total-secs (+ total-secs (* sign (cdr time))))))
	    (setq sigma-signs (+ sign sigma-signs))))))
    (goto-char end)
    (cond ((= sigma-signs -1)
	    ;; Assume the current time.
	    (let ((time (discus-parse-date (current-time-string))))
	      (setq days (+ days (car time)))
	      (setq secs (+ secs (cdr time)))
	      (setq total-days (+ total-days (car time)))
	      (setq total-secs (+ total-secs (cdr time)))))
	  ((not (zerop sigma-signs))
	    (error "Mismatched in/out lines; parity=%s." sigma-signs)))
    ;; clean up.
    (cond (print-daily-p
	    (psa-interval-finish-day today days secs)
	    (psa-interval-finish-week)
	    (rgr-princ-interval-padded "Total" total-days total-secs 10)))
    (let ((interval (rgr-print-interval total-secs (* 24 total-days))))
      (message "%s%s" interval
	       (if (= sigma-signs -1)
		   " (assuming the last interval ends now)."
		   ""))
      (and insert-p (insert interval))
      interval)))

;;;###autoload
(defun rgr-weekly-intervals (start end)
  "Compute daily and weekly totals from the login history in the region."
  (interactive "r")
  (with-output-to-temp-buffer "*daily-intervals*"
    (rgr-compute-region-interval start end nil t)))

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

;;;; Computing T fares.
;; This relies on the fact that I annotate my login history with (bs,sb) forms.

(defvar rgr-t-commuter-rail-zone-fares
	(vector nil nil 225 nil nil nil nil nil nil nil nil)
  "Units are pennies (historic artifact of emacs 18), using the zone as
the key, or nil for unknown.  (Really, I should look these fares up on
http://www.mbta.com/ and finish the list.)")

(defvar rgr-t-bus-fare 75
  "Bus fare per trip, in pennies.")

(defvar rgr-t-subway-fare 100
  "Subway fare per trip, in pennies.")

(defun rgr-format-dollars (pennies)
  (format "$%d.%s" (/ pennies 100) (rgr-make-interval-field (% pennies 100))))

;;;###autoload
(defun rgr-compute-region-t-fares (start end &optional insert-p)
  "Compute the equivalent total (ignoring passes) spent on T fares in
the region.  With a numeric argument, insert the message at point."
  (interactive "r\nP")
  (let ((total-bus 0) (total-subway 0) (total-commuter-rail 0))
    ;; Count the number of trips.
    (save-excursion
      (goto-char start)
      (while (re-search-forward "([0-9sbt,]+)" end t)
	(goto-char (1+ (match-beginning 0)))
	(let ((prefix nil) (char nil))
	  (while (not (eq (setq char (char-after (point))) ?\)))
	    (cond ((eq char ?b)
		    (setq total-bus (+ total-bus (or prefix 1))))
		  ((eq char ?s)
		    ;; Subway pass or token fare.
		    (setq total-subway (+ total-subway (or prefix 1))))
		  ((eq char ?t)
		    ;; Commuter rail fare; the prefix is the zone.
		    (or (and (numberp prefix)
			     (<= 1 prefix) (<= prefix 10))
			(error "Missing zone for commuter rail ('t') fare."))
		    (setq total-commuter-rail
			  (+ total-commuter-rail
			     (or (aref rgr-t-commuter-rail-zone-fares prefix)
				 (error "Fare %S is undefined." prefix))))))
	    (setq prefix
		  (if (and (<= ?0 char) (<= char ?9))
		      ;; digits accumulate . . .
		      (+ (* (or prefix 0) 10) (- char ?0))
		      ;; non-digits clear.
		      nil))
	    (forward-char)))))
    ;; Compute & print totals.
    (let* ((bus-pennies (* rgr-t-bus-fare total-bus))
	   (subway-pennies (* rgr-t-subway-fare total-subway))
	   (message-string
	    (message "%d bus trips (%s), %d subway trips (%s)%s, %s total fare."
		     total-bus (rgr-format-dollars bus-pennies)
		     total-subway (rgr-format-dollars subway-pennies)
		     (if (> total-commuter-rail 0)
			 (concat ", "
				 (rgr-format-dollars total-commuter-rail)
				 " commuter rail")
			 "")
		     (rgr-format-dollars (+ bus-pennies subway-pennies
					    total-commuter-rail)))))
      (if insert-p
	  (insert message-string "\n"))
      ;; for backward compatibility
      message-string)))

;;;###autoload
(defun rgr-compute-t-fares (&optional insert-p)
  "Compute the equivalent total (ignoring passes) spent on T fares from
point to the end of the buffer.  With a numeric argument, insert the
message at point."
  (interactive "P")
  (rgr-compute-region-t-fares (point) (point-max) insert-p))

;;;; Date tags (e.g. on ~rogers/projects/random/bills.text lines).

;;;###autoload
(defun rgr-date-lines-today ()
  "Put today's date at the start of each line from the current line to
the end of the paragraph.  The date is of the form 'MMDD' with leading
zeros.  The lines must have leading whitespace; this whitespace is
tweaked so as not to disturb the indentation of the rest of the line."
  (interactive)
  (let* ((n-lines 0)
	 (time-tail (nthcdr 3 (decode-time)))
	 (date-string (concat (rgr-make-interval-field (car (cdr time-tail)))
			      (rgr-make-interval-field (car time-tail)))))
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
