;;;; Querying NIC whois servers.
;;;
;;; [split out of the unauth.el file.  -- rgr, 23-Aug-03.]
;;;
;;; $Id$

(defvar rgr-unauth-whois-servers
	'(("whois.arin.net" "(\\(NET\\(BLK\\)?-[^ \t\n\)]+\\))")
	  ("whois.ripe.net" "(\\(NETBLK-[^ \t\n\)]+\\))") ;; probably wrong
	  ;; [apnic is more verbose, so re-querying is actually redundant for
	  ;; our purposes.  -- rgr, 12-Dec-02.]
	  ;; ("whois.apnic.net" "^netname *: +\\([^ \t\n]+\\)")
	  ))

(defun rgr-unauth-query-arin-internal (netblk query-host &optional buffer)
  ;; performs the actual based on the shell-command fn.
  (let* ((jpnic-p (equal query-host "whois.nic.ad.jp"))
	 ;; decide how to decorate the query ("!" or "/e" or neither) so that
	 ;; the server tells us what we want to know.
	 (query (cond (jpnic-p
			;; (concat netblk "/e")
			netblk)
		      ((string-match rgr-unauth-ip-address-regexp netblk)
		        netblk)
		      (t (concat "\\!" netblk))))
	 (command (concat "whois -h " query-host " " query)))
    (message "Querying for %s at %s..." netblk query-host)
    (sit-for 0)
    (save-excursion
      (set-buffer (get-buffer-create (or buffer " arin tmp")))
      (buffer-disable-undo)
      (erase-buffer)
      (insert "% " command "\n")
      ;; (shell-command command buffer)
      (call-process shell-file-name nil t nil
		    shell-command-switch command)
      (buffer-substring (point-min) (point-max)))))

(defun rgr-unauth-query-arin (host-ip &optional query-host)
  ;; based on the shell-command fn.  fills and displays a buffer with output
  ;; from the query host, and returns the buffer.
  (let* ((query-host (or query-host "whois.arin.net"))
	 (jpnic-p (equal query-host "whois.nic.ad.jp"))
	 (buffer (get-buffer-create (concat "*" query-host "*")))
	 (netblk-regexp (car (cdr (assoc query-host rgr-unauth-whois-servers))))
	 (command (concat "whois -h " query-host " " host-ip
			  ;; jpnic uses this idiosyncratic syntax to select
			  ;; english-only output.  [not any longer, it apepars.
			  ;; -- rgr, 31-May-03.]
			  ;; (if jpnic-p "/e" "")
			  ""))
	 (netblks nil))
    (shell-command command buffer)
    (save-excursion
      (save-window-excursion
	(set-buffer buffer)
	;; we assume that delegation to other name authorities for a given IP is
	;; complete and unidirectional, e.g. always from ARIN => APNIC => JPNIC,
	;; and never the other way.  So we only need to worry about remembering
	;; the last query host.
	(make-local-variable 'rgr-unauth-last-query-host)
	(setq rgr-unauth-last-query-host query-host)
	(goto-char (point-min))
	(insert "% " command "\n")
	;; now look for subordinate NETBLK entries that we haven't already seen.
	(while (and netblk-regexp
		    (re-search-forward netblk-regexp nil t))
	  (let ((netblk (match-string 1)))
	    (if (not (member netblk netblks))
		(save-excursion
		  (goto-char (point-max))
		  (setq rgr-unauth-last-query-host
			(cond ((string-match "^NETBLK-APNIC-" netblk)
				"whois.apnic.net")
			      ((string-match "^KRNIC" netblk)
				"whois.nic.or.kr")
			      ((string-match "^NET\\(BLK\\)?-RIPE-\\|-RIPE$"
					     netblk)
				"whois.ripe.net")
			      (t
				query-host)))
		  (insert (rgr-unauth-query-arin-internal
			    netblk rgr-unauth-last-query-host))
		  (setq netblks (cons netblk netblks))))))
	;; kludge to list technical contacts and "sub allocations", which aren't
	;; inlined for queries to jpnic.
	(while (and jpnic-p
		    (re-search-forward
		      (concat "\\[\\(Technical Contact\\|Sub Allocation\\)\\] +"
			      "\\([^ \t\n]+\\)")
		      nil t))
	  (let ((handle (match-string 2)))
	    (save-excursion
	      (goto-char (point-max))
	      (insert (rgr-unauth-query-arin-internal handle query-host)))))
	buffer))))

(provide 'rgr-unauth-query)
