;;; Hacking Squid logs.
;;;
;;; [created (as rgr-squid-log.el).  -- rgr, 1-Dec-01.]
;;;
;;; $Id: rgr-squid-log.el 324 2006-11-27 02:12:12Z rogers $

(require 'time-date)	;; for seconds-to-time

(defun rgr-tai64-date-at-point ()
  (cond ((looking-at "@\\(4000[0-9a-f]*\\)")
	  (match-string 1))))

;;;###AUTOLOAD
(defun rgr-show-tai64-date ()
  (interactive)
  (let* ((date-string (or (rgr-tai64-date-at-point)
			  (error "No tai64 date at point.")))
	 (emacs-date (rgr-tai64-parse-date date-string)))
    (message "%s" (rgr-tai64-format-date emacs-date))))

(defun rgr-tai64-format-date (emacs-date)
  (let ((result (format-time-string "%Y-%m-%d %T %z" emacs-date))
	(usec (nth 2 emacs-date)))
    (if usec
	;; We rely on knowing that %z is always five characters long, plus one
	;; for the space.
	(concat (substring result 0 -6)
		(format ".%06d" usec)
		(substring result -6))
	result)))

(defun rgr-tai64-parse-date (date-string)
  (let ((raw-len (length date-string))
	(nanoseconds nil))
    ;; (message "[raw date %S]" date-string)
    (cond ((member raw-len '(24 32))
	    ;; tai64n (with nanoseconds) or tai64na (ns plus attoseconds).
	    ;; We can only record microseconds in the Emacs time format.
	    (setq nanoseconds (substring date-string 16 24))
	    (setq date-string (substring date-string 0 16)))
	  ((not (= raw-len 16))
	    (error "%S (len %d) is not a valid tai64 date."
		   date-string raw-len)))
    (let* ((date-hex (if (string-match "^40*" date-string)
			 (substring date-string (match-end 0))
			 (error "%S (len %d) is not a valid tai64 date."
				date-string raw-len)))
	   (date (seconds-to-time (string-to-number date-hex 16))))
      (if (and nanoseconds (nthcdr 2 date))
	  (rplaca (nthcdr 2 date)
		  (floor (string-to-number nanoseconds 16) 1000)))
      date)))

(provide 'rgr-date)
