;;; Hacking Squid logs.
;;;
;;; [created.  -- rgr, 1-Dec-01.]
;;;
;;; $Id$

;;;###AUTOLOAD
(defun rgr-show-squid-log-date ()
  (interactive)
  (let ((date-string nil))
    (save-excursion
      (beginning-of-line)
      (or (looking-at "^\\([0-9]+\\)\\.\\([0-9]+\\)")
	  (error "Not on a log line."))
      ;; (setq milliseconds (string-to-int (match-string 2)))
      (setq date-string (match-string 1)))
    (let* ((low-start (- (length date-string) 6))
	   (millions (string-to-int (substring date-string 0 low-start)))
	   (units (string-to-int (substring date-string low-start)))
	   ;; The millions are units of 15 LSPs plus 16960 seconds (04:42:40).
	   (raw-LSP (+ (* millions 16960) units))
	   (LSP (mod raw-LSP 65536))
	   (MSP (+ (/ raw-LSP 65536) (* millions 15)))
	   (date (list MSP LSP)))
      (message "%s" (format-time-string "%Y-%m-%d %T" date))
      date)))

(provide 'rgr-squid-log)
