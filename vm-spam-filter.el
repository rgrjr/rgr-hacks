;;;; Simple anti-spam hacks for VM.
;;;
;;;    Modification history:
;;;
;;; created.  -- rgr, 2-Sep-01.
;;;

(defun rgr-vm-check-current-message-for-spam (message)
  (cond ((if (vm-mime-layout-of message)
	     (let ((mm (vm-mime-layout-of message)))
	       (cond ((vm-mime-types-match (car (vm-mm-layout-type mm))
					   "text/html")
		       (message "Message %s is text/html . . ."
				(vm-number-of message))
		       (sit-for 1)
		       t))))
	   ;; delete it.
	   (vm-set-deleted-flag message t)
	   t)))

(defun rgr-vm-check-arrived-message-for-spam-hook ()
  ;; put this on vm-arrived-message-hook, at which point message will be bound
  ;; to the message structure.
  (if message
      (rgr-vm-check-current-message-for-spam message)))

(defun rgr-vm-check-message-for-spam (&optional count)
  "Check the current message for possible spam."
  (interactive "p")
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (let ((n-deleted 0)
	(tail (vm-select-marked-or-prefixed-messages count)))
    (while tail
      (if (rgr-vm-check-current-message-for-spam (car tail))
	  (setq n-deleted (1+ n-deleted)))
      (setq tail (cdr tail)))
    (cond ((= n-deleted 0)
	     (message "No spam found."))
	  (t
	     (if count
		 (message "%d spam message%s found and deleted."
			  n-deleted (if (eql n-deleted 1) "" "s")))
	     (vm-display nil nil '(rgr-vm-check-message-for-spam)
			 (list this-command))
	     (vm-update-summary-and-mode-line)))))

