;;; Hacking Squid logs.
;;;
;;;    Modification history:
;;;
;;; created.  -- rgr, 1-Dec-01.
;;;

(require 'discus-date)

(defconst rgr-unix-day-0 (car (discus-parse-date "Jan 1 1970"))
  "Figurative constant for converting Unix dates to Discus format.")

(defun rgr-show-log-date ()
  (interactive)
  (let ((date-string nil) (match-seconds nil))
    (save-excursion
      (beginning-of-line)
      (or (looking-at "^\\([0-9]+\\)\\.\\([0-9]+\\)")
	  (error "Not on a log line."))
      (setq date-string (match-string 1))
      (setq milliseconds (string-to-int (match-string 2))))
    (let* ((low-start (- (length date-string) 6))
	   (millions (string-to-int (substring date-string 0 low-start)))
	   (units (string-to-int (substring date-string low-start)))
	   ;; The millions are units of 11 days and 49600 seconds (13:46:40);
	   ;; recent dates will have roughly 1000 of these.  -- rgr, 1-Dec-01.
	   (raw-secs (+ (* millions 49600) units))
	   (secs (mod raw-secs 86400))
	   (days (+ (/ raw-secs 86400) (* millions 11)))
	   (discus-date (cons (+ days rgr-unix-day-0) secs)))
      (message "%s" (discus-print-date discus-date)))))

; (require 'discus-date)
; (discus-print-time 49600) 
; (/ (mod 1000000 (* 24 60 60)) (* 60 60.0))

(provide 'rgr-squid-log)
