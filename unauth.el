;;;; Generating email reporting unauthorized network connection attempts.
;;;
;;;    Do M-x rgr-unauthorized-connection in a check-logs.pl report.  For hosts
;;; with rejected or denied connection attempts, a complaint message will be
;;; composed, and you will be left in mail mode to touch up and send the
;;; message.  Repeat for each offending host.
;;;
;;;    Modification history:
;;;
;;; created.  -- rgr, 6-Jun-00.
;;; major rewrite.  -- rgr, 30-Jul-00.
;;; rgr-unauth-protocol-disposition-line-regexp: allow pop-3.  -- rgr, 3-Aug-00.
;;; rgr-unauth-send-complaint: add ARIN query.  -- rgr, 5-Aug-00.
;;; rgr-unauth-insert-stuff asp query, make rgr-unauth-query-arin recognize
;;;	apnic blocks.  -- rgr, 11-Aug-00.
;;; rgr-unauth-query-arin: add ripe (or try to).  -- rgr, 18-Aug-00.
;;; rgr-unauth-query-arin: fix ripe regexp.  -- rgr, 16-Oct-00.
;;; rgr-unauth-query-arin: oops; need both, generalized.  -- rgr, 18-Oct-00.
;;; rgr-unauth-scarf-attempt-alist: un-name asp/tcp.  -- rgr, 23-Oct-00.
;;; rgr-unauth-query-arin apnic kludge, kludge timezone to EST, skip private
;;;	addresses.  -- rgr, 8-Nov-00.
;;; rgr-unauth-query-arin: unsuccessful nic.or.kr fwd'ing.  -- rgr, 13-Nov-00.
;;; rgr-unauth-insert-stuff: fix host name/IP/TZ kludges.  -- rgr, 15-Nov-00.
;;; rgr-unauth-scarf-attempt-alist: don't include ftp/tcp.  -- rgr, 9-Dec-00.
;;; rgr-unauth-find-subnet-abuse-address and support.  -- rgr, 16-Dec-00.
;;; abuse@bbnplanet.com.  -- rgr, 19-Dec-00.
;;; rgr-unauth-parse-ip-subnet: add subrange format.  -- rgr, 24-Dec-00.
;;; rgr-unauth-whois-servers: add :whois option.  -- rgr, 31-Dec-00.
;;; rgr-unauth-find-subnet: allow subnet to be a list.  -- rgr, 2-Jan-01.
;;; rgr-unauth-protocol-disposition-line-regexp: b'cast.  -- rgr, 15-Jan-01.
;;; added 169.254.0.0/16 as private addresses.  -- rgr, 2-Feb-01.
;;; rgr-unauth-subnet-match-bounds-tail-p: fix bug with full containment, no
;;;	whitespace in rgr-unauth-whois-servers.  -- rgr, 11-Feb-01.
;;; rgr-unauth-query-arin-internal: don't use \! for IP.  -- rgr, 14-Feb-01.
;;; rgr-unauth-find-all-subnets: deal with overlaps.  -- rgr, 19-Feb-01.
;;; whois.registro.br.  -- rgr, 7-Mar-01.
;;; rgr-unauth-send-complaint: use rgr-unauth-make-uid.  -- rgr, 9-Mar-01.
;;; rgr-unauth-abuse-addresses: remove 62.224.0.0/14.  -- rgr, 22-Mar-01.
;;; rgr-unauth-insert-stuff: also check /var/log/messages.1.  -- rgr, 1-Apr-01.
;;; ...
;;; rgr-unauth-insert-local-hostname error check.  -- rgr, 21-Oct-01.
;;; rgr-unauth-insert-stuff: TIA before sig.  -- rgr, 22-Oct-01.
;;; fudge host name for reports to mediaone.  -- rgr, 28-Oct-01.
;;; rgr-unauth-insert-stuff: include numeric timezone.  -- rgr, 1-Nov-01.
;;; rgr-unauth-scarf-attempt-alist: also scarf ssh/tcp.  -- rgr, 13-Dec-01.
;;; mediaone.net -> attbi.com.  -- rgr, 19-Feb-02.
;;; attbi.net -> attbi.com, new netblock, fix my DNS name.  -- rgr, 14-Mar-02.
;;; "CHINANET Hebei province network".  -- rgr, 24-Mar-02.
;;; rgr-unauth-make-abuse-address: support ".ad.jp".  -- rgr, 13-Apr-02.
;;; rgr-unauth-insert-stuff: mention NTP in time blurb.  -- rgr, 7-May-02.
;;; rgr-unauth-insert-stuff: extract previous attempts.  -- rgr, 13-May-02.
;;; bbnplanet.com -> genuity.net.  -- rgr, 4-Jul-02.
;;; rgr-unauth-benign-protocol-regexp, skip 1433 ("Spida" worm), correct gte.net
;;;	entry, more concentric.net, thrunet.com.  -- rgr, 5-Jul-02.
;;; rgr-unauth-class-a-entries interface to bucketized address DB, plus the
;;;	complete "CHINANET Sichuan province network".  -- rgr, 21-Jul-02.
;;; rgr-unauth-benign-protocol-regexp: added KaZaA.  -- rgr, 24-Jul-02.
;;; "abuse@jsinfo.net" entry.  -- rgr, 23-Aug-02.
;;; security@telesp.net.br, more krnic.  -- rgr, 25-Aug-02.
;;; rgr-unauth-previous-month-log-file-name search.  -- rgr, 27-Aug-02.
;;; more chinanet.  -- rgr, 31-Aug-02.
;;; more kornet.net.  -- rgr, 7-Sep-02.
;;; new whois.lacnic.net registry.  -- rgr, 3-Oct-02.
;;; rgr-unauth-insert-stuff: change reverse DNS address.  -- rgr, 27-Oct-02.
;;; JPNIC support, clean "added subnet" comments.  -- rgr, 12-Dec-02.
;;; rgr-unauth-query-arin-inserting-headers: new.  -- rgr, 16-Dec-02.
;;; rgr-unauth-whois-server-regexps: lacnic & apnic stubs.  -- rgr, 17-Dec-02.
;;; rgr-unauth-benign-protocol-regexp: skip 445 probes.  -- rgr, 18-Dec-02.
;;; moved subnet database to rgr-unauth-db.el file.  -- rgr, 23-Dec-02.
;;; rgr-unauth-query-arin-inserting-headers: look for an ARIN abuse POC.
;;;	-- rgr, 28-Dec-02.
;;; code for handling ripe tech POC lookups.  -- rgr, 29-Dec-02.
;;; tech POCs for lacnic and apnic, bug fixes, better msgs.  -- rgr, 30-Dec-02.
;;; rgr-unauth-whois-server-regexps: "whois.nic.ad.jp" entry.  -- rgr, 1-Jan-03.
;;; use firewall report date to make "yesterday" phrase & consolidate old log
;;;	entries, recursively list JPNIC "Sub Allocations."  -- rgr, 10-Jan-03.
;;; rgr-unauth-whois-server-regexps: registro.br.  -- rgr, 13-Jan-03.
;;; rgr-unauth-insert-poc-headers: "abuse" POC as default.  -- rgr, 16-Jan-03.
;;; new rgr-unauth-query-whois-server cmd, refactored query code to make this
;;;	available.  -- rgr, 19-Jan-03.
;;; rgr-unauth-query-whois-server: make ARIN default server.  -- rgr, 9-Feb-03.
;;; rgr-unauth-whois-server-regexps: oops; don't allow \n in ARIN TechEmail
;;;	regexp; the field is not always omitted if blank.  -- rgr, 12-Apr-03.
;;; starting to use the log to hack followup reports.  -- rgr, 15-Apr-03.
;;;

(require 'rgr-unauth-db)

(defvar rgr-unauth-use-log-report-date-p nil
  "*If nil, include all unreported log events.  If non-nil, use only
those log events generated since the start of the report period.")

(defvar rgr-unauth-last-query-host nil
  "String naming the last server queried for POCs, e.g. \"whois.ripe.net\".
This is made buffer-local to the *whois.ripe.net* buffer.")

(defvar rgr-unauth-whois-server-regexps
	'(("whois.arin.net"
	   "^NetRange: +\\([0-9.]+ *- *[0-9.]+\\)" 
	   ;; there is usually a trailing space on these.
	   (every "^TechEmail: +\\([^ \t\n]+\\)")
	   (first "^OrgAbuseEmail: +\\([^ \t\n]+\\)"
	          "^AbuseEmail: +\\([^ \t\n]+\\)"))
	  ("whois.ripe.net"
	   "^inetnum: +\\([0-9.]+ *- *[0-9.]+\\)" 
	   ;; the field is just called "e-mail", but we have to restrict this to
	   ;; pick only entries with a "nic-hdl" listed as being a technical
	   ;; contact.  see rgr-unauth-scarf-ripe-data for gory details.
	   (every (ripe "^tech-c: *\\([^ \t\n]+\\)"
			"^nic-hdl: *"
			"^e-mail: *\\([^ \t\n]+\\)")))
	  ("whois.lacnic.net"
	   "^inetnum: +\\([0-9.]+ *- *[0-9.]+\\|[0-9./]+\\)"
	   ;; lacnic also defines an "owner contact" field, which is often the
	   ;; only thing available.
	   (every (ripe (first "tech-c: *\\([^ \t\n]+\\)"
			       "owner-c: *\\([^ \t\n]+\\)")
			"^nic-hdl: *"
			"^e-mail: *\\([^ \t\n]+\\)")))
	  ("whois.registro.br"
	   ;; these are always CIDR format.
	   "^inetnum: +\\([0-9./]+\\)"
	   ;; the field is just called "e-mail", but we have to restrict this to
	   ;; pick only entries with a "nic-hdl" listed as being a technical
	   ;; contact.  see rgr-unauth-scarf-ripe-data for gory details.
	   (every (ripe "^tech-c: *\\([^ \t\n]+\\)"
			"^nic-hdl-br: *"
			"^e-mail: *\\([^ \t\n]+\\)"))
	   (ripe "^abuse-c: *\\([^ \t\n]+\\)"
		 "^nic-hdl-br: *"
		 "^e-mail: *\\([^ \t\n]+\\)"))
	  ("whois.apnic.net"
	   "^inetnum: +\\([0-9.]+ *- *[0-9.]+\\)" 
	   (every (ripe "tech-c: *\\([^ \t\n]+\\)"
			"^nic-hdl: *"
			"^e-mail: *\\([^ \t\n]+\\)")))
	  ("whois.nic.or.kr"
	   "^IP Address *: *\\([0-9.]+ *- *[0-9.]+\\)"
	   (every (paragraph "Tech Contact Information"
			     "E-Mail *: *\\([^ \t\n]+\\)"))
	   (paragraph "Abuse Contact Information"
		      "E-Mail *: *\\([^ \t\n]+\\)"))
	  ("whois.nic.ad.jp"
	   "\\[Network Number\\] *\\([0-9.]+ *- *[0-9.]+\\)"
	   (every (ripe "\\[Technical Contact\\] *\\([^ \t\n]+\\)"
			"\\[JPNIC Handle\\] +"
			"\\[E-Mail\\] +\\([^ \t\n]+\\)"))))
  "Alist mapping whois servers to information that helps us grok their
output format.  Each entry is (whois-server-name ip-address-finder
tech-poc-finder abuse-address-finder).  The finders can be strings,
which are used as regexps where (match-string 1) returns the value, but
can also be lists that start with each, first, every, paragraph, and
match-string (to select other than the first), and contain various
kinds of finder subspecs.  [see the
rgr-unauth-scarf-whois-data fn for details.  -- rgr, 29-Dec-02.")

(defvar rgr-unauth-dotted-quad-regexp
	(let (;; If I don't do something this explicit, I tend to get hits to
	      ;; message ID's generated by the vm mailer.
	      (octet "[0-2]?[0-9][0-9]?") (dot "\\."))
	  (concat octet dot octet dot octet dot octet)))

(defvar rgr-unauth-protocol-disposition-line-regexp
        (let ((identifier "\\([-a-zA-Z0-9/]+\\)"))
	  ;; the disposition is uppercase-only, the protocol/transport (first
	  ;; use) is lowercase-only with a slash (and sometimes a dash, as in
	  ;; pop-3), but I'm too lazy to use more specific regexps, and besides,
	  ;; I might someday want to change the check-logs.pl output format.  --
	  ;; rgr, 30-Jul-00.
	  (concat "^ *" identifier
		  " (\\([0-9]+\\) \\(broadcast \\)?attempts, disp "
		  identifier ")"))
  "Regexp that recognizes the protocol/attempt count/disposition line.")

(defvar rgr-unauth-benign-protocol-regexp
        (concat "^\\(ftp\\|143[34]\\|ms-sql-[sm]\\|epmap"
		"\\|6346\\|1214\\|445\\|microsoft-ds\\)/tcp$"
		"\\|^\\(epmap\\|ms-sql-m\\)/udp$")
  ;; 1433 is for the Microsoft SQL Server; see the "Spida" worm advisory on the
  ;; http://www.iss.net/security_center/alerts/advise118.php page.  I no longer
  ;; report these.  And 6346 is gnutella, and 1214 is KaZaA, both of which are
  ;; too ambiguous to report.  [445 aka "microsoft-ds" is for Windows file
  ;; service; there seems to be a new worm propagating.  -- rgr, 18-Dec-02.]
  "Regexp that matches protocol names (e.g. 'ftp/tcp') for which we
should never complain even when somebody attempts to connect.  For
instance, FTP connections are benign, because I do run a Web server, so
somebody might legitimately suppose I was also running an FTP server.")

;;; Utility functions.

(defvar rgr-unauth-uid-date-prefix "")
(defvar rgr-unauth-uid-gensym-index 0)

(defun rgr-unauth-make-uid ()
  "Construct a unique 8-digit identifier based on the date and an
arbitrary sequence number.  This will only be globally unique if you
start at most one emacs per day."
  (let* ((time (decode-time))
	 (day (nth 3 time))
	 (month (nth 4 time))
	 (year (nth 5 time))
	 (prefix (format "%02d%02d%02d" (mod year 100) month day)))
    (cond ((not (equal prefix rgr-unauth-uid-date-prefix))
	    (setq rgr-unauth-uid-gensym-index 0)
	    (setq rgr-unauth-uid-date-prefix prefix)))
    (setq rgr-unauth-uid-gensym-index (1+ rgr-unauth-uid-gensym-index))
    (format "%s%02d" prefix rgr-unauth-uid-gensym-index)))

(defun rgr-find-if (func list)
  ;; Return the first non-nil result from calling func on the list elements in
  ;; turn, or nil if all return nil or the list is empty.  [maybe this is
  ;; already in cl.el ?  -- rgr, 19-Feb-01.]
  (let ((tail list) (result nil))
    (while tail
      (if (setq result (funcall func (car tail)))
	  (setq tail nil)
	  (setq tail (cdr tail))))
    result))

;;; Querying NIC whois servers.

(defun rgr-unauth-query-arin-internal (netblk query-host &optional buffer)
  ;; based on shell-command
  (let* ((jpnic-p (equal query-host "whois.nic.ad.jp"))
	 ;; decide how to decorate the query ("!" or "/e" or neither) so that
	 ;; the server tells us what we want to know.
	 (query (cond (jpnic-p (concat netblk "/e"))
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

(defvar rgr-unauth-whois-servers
	'(("whois.arin.net" "(\\(NET\\(BLK\\)?-[^ \t\n\)]+\\))")
	  ("whois.ripe.net" "(\\(NETBLK-[^ \t\n\)]+\\))") ;; probably wrong
	  ;; [apnic is more verbose, so re-querying is actually redundant for
	  ;; our purposes.  -- rgr, 12-Dec-02.]
	  ;; ("whois.apnic.net" "^netname *: +\\([^ \t\n]+\\)")
	  ))

(defun rgr-unauth-query-arin (host-ip &optional query-host)
  ;; based on the shell-command fn.  fills and displays a buffer with output
  ;; from the query host, and returns the buffer.
  (let* ((query-host (or query-host "whois.arin.net"))
	 (jpnic-p (equal query-host "whois.nic.ad.jp"))
	 (buffer (get-buffer-create (concat "*" query-host "*")))
	 (netblk-regexp (car (cdr (assoc query-host rgr-unauth-whois-servers))))
	 (command (concat "whois -h " query-host " " host-ip
			  ;; jpnic uses this idiosyncratic syntax to select
			  ;; english-only output.
			  (if jpnic-p "/e" "")))
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

;;; Generating the message.

(defvar rgr-unauth-protocol-and-ports-regexp
        (let* ((digits "\\([0-9]+\\)")
	       (ip-address-colon-port (concat "\\([0-9.]+\\):" digits)))
	  (concat "PROTO=" digits " " ip-address-colon-port " "
		  ip-address-colon-port " "))
  "Matched substrings are (protocol source-ip source-port dest-ip dest-port).")

(defun rgr-unauth-extract-previous-attempts (&optional last-reported-entry-date
					     report-date)
  ;; This assumes we are narrowed down to just the log entries.  Identify the
  ;; ones we want to trim, and return a digested form for later summarization.
  (let* ((start (point-min))
	 (prev-end (set-marker (make-marker) (point)))
	 (result nil))
    (goto-char start)
    (let ((date (or last-reported-entry-date report-date)))
      ;; (message "[using regexp date %S]" date)
      (cond ((null date)
	      ;; no report date, so use it all.
	      )
	    ((not (re-search-forward (concat "^" (regexp-quote date)) nil t))
	      ;; hmm, this shouldn't happen, but just in case, leave it all in
	      ;; so that the user can figure it out.
	      (message "No date %S in log?" date)
	      (sit-for 1))
	    (last-reported-entry-date
	      ;; we want to exclude anything we've already reported.
	      (forward-line 1))
	    (t
	      ;; this should be the "report-date", so include this line.
	      (beginning-of-line)))
      (setq prev-end (set-marker (make-marker) (point))))
    ;; Now operate on lines between start & prev-end.
    (goto-char start)
    (while (< (point) prev-end)
      (let ((detail-start (point))
	    (detail-end (save-excursion
			  (forward-line 1)
			  (point)))
	    (date (buffer-substring (point) (+ (point) 15))))
	(cond ((re-search-forward rgr-unauth-protocol-and-ports-regexp
				  detail-end t)
		(let ((proto (string-to-int (match-string 1)))
		      (dest-port (string-to-int (match-string 5))))
		  (setq result (cons (list date proto dest-port) result)))
	        ;; make the next line come to us.
	        (delete-region detail-start detail-end))
	      (t
		(goto-char detail-end)))))
    ;; now get rid of the earlier detail lines.  [no; it's better to delete only
    ;; the detail lines we can parse.  -- rgr, 13-May-02.]
    ;; (delete-region start (point))
    (nreverse result)))

(defun rgr-unauth-insert-local-hostname (&optional ip-address-p)
  (let ((start (point)))
    (apply (function call-process) "hostname" nil t nil
	   (if ip-address-p '("-i") nil))
    ;; handle hostname wierdness.
    (while (member (char-after (1- (point))) '(?\ ?\n ?\t))
      (delete-char -1))
    (if (save-excursion
	  (goto-char start)
	  (looking-at "hostname:"))
	;; must be an error message.
	(let ((string (buffer-substring start (point))))
	  (delete-region start (point))
	  (message "%s" string)
	  (sit-for 2)))))

(defun rgr-unauth-previous-month-log-file-name ()
  ;; Log files are of the format "/var/log/messages.200207.log".  We compute the
  ;; previous month by finding out what day month and year it is, then
  ;; subtracting one.
  (let* (;; this-month is (month year ...)
	 (this-month (nthcdr 4 (decode-time)))
	 (last-month (1- (car this-month)))
	 (last-year (car (cdr this-month))))
    (if (zerop last-month)
	;; arrange to borrow 12 months from the year.
	(setq last-month 12
	      last-year (1- last-year)))
    (format "/var/log/messages.%04d%02d.log" last-year last-month)))

(defun rgr-unauth-relative-date-phrase (date)
  (if (stringp date)
      (let* ((day (car (discus-parse-date date)))
	     (today (car (discus-parse-date (current-time-string))))
	     (delta (- today day)))
	(cond ((= delta -1) "tomorrow")
	      ((< delta 0) "some time in the near future")
	      ((= delta 0) "today")
	      ((= delta 1) "yesterday")
	      ((= delta 2) "the day before yesterday")
	      (t "several days ago")))
      ;; say something reasonable if the caller couldn't find a date in the log.
      "recently"))

(defun rgr-unauth-insert-stuff (log-report-date abuse-address host-name host-ip
						attempt-alist plural-p
						&optional previous-reports)
  ;; Insert the body of an "unauthorized connection attempt" message.
  (let* ((protocols (mapcar (function car) attempt-alist))
	 (proto-insert (if (cdr protocols)
			   ""
			   (concat " for " (car protocols) " service")))
	 (last-report-date (car (cdr previous-reports)))
	 (previous-attempts nil))
    (save-excursion
      (goto-char (point-max))
      (insert "  I got the following unauthorized connection attempt"
	      (if plural-p "s" "")
	      proto-insert "\nfrom ")
      (if host-name
	  (insert host-name " (" host-ip ")")
	  (insert host-ip))
      (insert " " (rgr-unauth-relative-date-phrase log-report-date) ":")
      (rgr-mail-fill-paragraph nil)
      (insert "\n\n")
      ;; Grab complete log entries for this IP.
      (let ((start (point))
	    (last-log-file (rgr-unauth-previous-month-log-file-name)))
	;; look for hits in the previous month first.  if this file doesn't
	;; exist, then we probably just rolled the logs last night, so we should
	;; try /var/log/messages.1 instead.  [also look in the old system.  --
	;; rgr, 3-May-03.]
	(apply (function call-process) "fgrep" nil t nil
	       "-he" host-ip
	       (append (cond ((file-readable-p "/mnt/rh60/var/log/messages")
			       '("/mnt/rh60/var/log/messages")))
		       (cond ((file-readable-p last-log-file)
			       (list last-log-file))
			     ((file-readable-p "/var/log/messages.1")
			       '("/var/log/messages.1")))
		       '("/var/log/messages")))
	(save-excursion
	  (save-restriction
	    (narrow-to-region start (point))
	    ;; Prune log entries to eliminate anything we've already reported.
	    (setq previous-attempts
		  (rgr-unauth-extract-previous-attempts
		    last-report-date
		    (and rgr-unauth-use-log-report-date-p
			 log-report-date)))))
	;; Indent for prettiness (inasmuch as one can ever make a log report
	;; pretty).
	(indent-rigidly start (point) 4))
      ;; Insert boilerplate characterizing previous attempts.
      (if previous-reports
	  (let ((n-previous (length previous-attempts))
		(tail previous-attempts))
	    (insert "\nThis is in addition to "
		    (cond ((= n-previous 0)
			   ;; this happens when the last reported attempt
			   ;; doesn't show up in the logs.
			   "previous attempt(s)")
			  ((> n-previous 1) "previous attempts")
			  (t "a previous attempt"))
		    " from this IP ")
	    (while tail
	      (let* ((attempt (car tail))
		     (date (car attempt)) (protocol (car (cdr attempt)))
		     (dest-port (car (cdr (cdr attempt))))
		     (proto-name (cond ((= protocol 6) "tcp")
				       ((= protocol 17) "udp")
				       (t (format "proto%d" protocol)))))
		(insert "on " (substring date 0 6)
			" at " (substring date 7)
			" to " (format "%d" dest-port) "/" proto-name
			", ")
		(setq tail (cdr tail))))
	    (if previous-reports
		(let* ((tail (cdr (cdr previous-reports)))
		       (last (car tail))
		       (more-p (cdr tail)))
		  (if more-p
		      (insert "the last of which was "))
		  (insert "reported in an email with \"[" last
			  "]\" in the subject."))
		(insert "[not?] previously reported."))))
      ;; finish boilerplate.
      (insert "\nThe destination host is ")
      (if (or (equal abuse-address "abuse@mediaone.net")
	      (equal abuse-address "abuse@attbi.com"))
	  ;; cheating for our ISP.
	  (insert "h009027bdf26f.ne.client2.attbi.com")
	  (rgr-unauth-insert-local-hostname))
      (insert " (")
      (rgr-unauth-insert-local-hostname t)
      (insert ").  ")
      (let* ((zone (current-time-zone))
	     (hrs (/ (car zone) 3600))
	     (mins (mod (car zone) 3600)))
	(insert (if plural-p "Times are " "Time is ")
		(car (cdr zone))
		(format " (%.2d%.2d)" hrs mins)
		", and this machine is an NTP server (see 
http://www.eecis.udel.edu/~ntp/) that operates at stratum 3, 
so it generally keeps time within a fraction of a second."))
      (fill-paragraph nil)
      (insert "\n\n   Thanks in advance for taking care of this,"
	      "\n\n\t\t\t\t\t-- Bob Rogers\n"))))

(defun rgr-unauth-make-abuse-address (host-name)
  ;; Construct a plausible guess at an abuse address from the host domain name
  ;; string.
  (cond ((null host-name) "abuse@")
	((and (string-match "[^.]+\\.\\([^.]+\\)\\.\\([^.]+\\)$" host-name)
	      (= (- (match-end 2) (match-beginning 2)) 2)
	      (member (match-string 1 host-name)
		      '("com" "edu" "org" "net"
			;; .uk-style versions.
			"co" "ac" "or" "ne"
			;; also ".ad.jp" domains.
			"ad")))
	  ;; e.g. foo.co.uk; take three components instead of two.
	  (concat "abuse@" (match-string 0 host-name)))
	((string-match "[^.]+\\.[^.]+$" host-name)
	  (concat "abuse@" (match-string 0 host-name)))
	(t (concat "abuse@" host-name))))

(defun rgr-unauth-insert-poc-headers (source &optional netrange
					     addresses abuse-poc)
  ;; Generate email headers with POC information.  Some of this is useful for
  ;; constructing a "To:" address manually, and some of it will be logged by
  ;; rgr-unauth-mail-send-hook when the message is sent.
  (if (null abuse-poc)
      ;; Try to "promote" a POC address to an abuse address.
      (let ((tail addresses))
	(while tail
	  (let ((addr (car tail)))
	    (if (string-match "abuse.*@" addr)
		(setq abuse-poc addr tail nil)
		(setq tail (cdr tail)))))))
  (save-excursion
    (cond ((equal source "local")
	    (message "Found in local whois database."))
	  (abuse-poc
	    (message "Found %S from %s." abuse-poc source))
	  (t
	    (message "Found at %s." source)))
    (goto-char (point-min))
    (and (re-search-forward "^To:" nil t)
	 (goto-char (match-beginning 0)))
    (cond (abuse-poc
	    (if (looking-at "^To:")
		(delete-region (point) (progn (forward-line) (point))))
	    (insert "To: " abuse-poc "\n")
	    (forward-line -1)))
    ;; we are now before the "To:" header, if there is one.
    (insert "X-Abuse-POC-source: " source "\n")
    (if netrange
	(insert "X-Abuse-netrange: " netrange "\n"))
    (if addresses
	(let ((tail (cdr addresses)))
	  (insert "X-Abuse-tech-POC: " (car addresses))
	  (while tail
	    (insert ", " (car tail))
	    (setq tail (cdr tail)))
	  (insert "\n")))))

(defun rgr-unauth-scarf-ripe-data (end handle-spec para-prefix email-regexp)
  ;; the field is just called "e-mail", but we have to restrict this to just
  ;; entries with a "nic-hdl" listed as being a technical contact.  really, this
  ;; is just a relational join on the "nic-hdl" field.  fortunately, each entry
  ;; is a paragraph, and NIC handles are unique, so the search itself is
  ;; straightforward, we just have to construct a
  ;; (paragraph "^nic-hdl: *FOO" "^e-mail: *\\([^ \t\n]+\\)" expression on the
  ;; fly from what the first spec returned.
  (let ((handle (rgr-unauth-scarf-whois-data handle-spec end)))
    (and handle
	 (save-excursion
	   ;; this save-excursion is what makes it possible to wrap an "every"
	   ;; around a "ripe" spec.
	   (rgr-unauth-scarf-whois-data
	     (list 'paragraph
		   (concat para-prefix (regexp-quote handle))
		   email-regexp)
	     end)))))

(defun rgr-unauth-scarf-whois-data (spec &optional end)
  ;; Many of these clauses are not well tested.  -- rgr, 29-Dec-02.
  (cond ((null spec) nil)
	((stringp spec)
	  (and (re-search-forward spec end t)
	       (match-string 1)))
	((not (consp spec)) nil)
	((eq (car spec) 'match-string)
	  (and (re-search-forward (car (cdr spec)) end t)
	       (mapconcat (function match-string) (cdr (cdr spec)) "")))
	((eq (car spec) 'paragraph)
	  (and (re-search-forward (car (cdr spec)) end t)
	       (let ((para-spec (nth 2 spec))
		     (end (progn (forward-paragraph 1) (point))))
		 (backward-paragraph 1)
		 (rgr-unauth-scarf-whois-data para-spec end))))
	((eq (car spec) 'every)
	  ;; return a list of every hit, moving point each time.
	  (let ((hit (rgr-unauth-scarf-whois-data (car (cdr spec)) end)))
	    (and hit
		 (let ((rest (rgr-unauth-scarf-whois-data spec end)))
		   (if (member hit rest)
		       rest
		       (cons hit rest))))))
	((eq (car spec) 'ripe)
	  (apply (function rgr-unauth-scarf-ripe-data) end (cdr spec)))
	((eq (car spec) 'each)
	  ;; return a list with each hit or miss, resetting point each time.
	  (let ((start (point)))
	    (mapcar (function (lambda (subspec)
		      (prog1 (rgr-unauth-scarf-whois-data subspec end)
			(goto-char start))))
		    (cdr spec))))
	((eq (car spec) 'first)
	  ;; return the first hit, resetting point after unsuccessful specs.
	  (let ((start (point))
		(tail (cdr spec))
		(hit nil))
	    (while tail
	      (goto-char start)
	      (setq hit (rgr-unauth-scarf-whois-data (car tail) end))
	      (setq tail (if hit nil (cdr tail))))
	    hit))
	(t (error "Unrecognized 'Whois' data spec %S." spec))))

(defun rgr-unauth-query-whois-server-internal (host-ip whois-server)
  (let ((buffer (rgr-unauth-query-arin host-ip whois-server)))
    (save-excursion
      (set-buffer buffer)
      ;; need to evaluate all this in the query buffer.
      (let ((whois-regexps (assoc rgr-unauth-last-query-host
				  rgr-unauth-whois-server-regexps)))
	(cons rgr-unauth-last-query-host
	      (mapcar (function (lambda (spec)
			(goto-char (point-min))
			(rgr-unauth-scarf-whois-data spec)))
		      (cdr whois-regexps)))))))

(defun rgr-unauth-present-query-results (host-ip source &optional netrange
						 addresses abuse-poc)
  (with-output-to-temp-buffer "*Query summary*"
    (princ (format "Query IP:  %s\n" host-ip))
    (princ (format "Whois server:  %s\n" source))
    (and netrange
	 (princ (format "Net range:  %s\n" netrange)))
    (and addresses
	 (let ((tail addresses))
	   (princ "Technical contact addresses:\n")
	   (while tail
	     (princ "  ")
	     (princ (car tail))
	     (princ "\n")
	     (setq tail (cdr tail)))))
    (and abuse-poc
	 (princ (format "Abuse contact:  %s\n" abuse-poc)))))

(defun rgr-unauth-query-whois-server (host-ip &optional whois-server)
  ;; query whois server(s), and present the result summary to the user.
  (interactive
    (let* ((default-ip
	    (and (re-search-forward rgr-unauth-dotted-quad-regexp)
		 (match-string 0)))
	   (host-ip 
	     (if default-ip
		 (read-string (format "IP address (default %s): "
				      default-ip)
			      nil nil default-ip)
		 (read-string "IP address: ")))
	   (default-whois-server
	     (or (rgr-find-if (function rgr-subnet-whois-server)
			      (rgr-unauth-find-all-subnets host-ip))
		 "whois.arin.net"))
	   (whois-server
	     (read-string (format "WHOIS server (default %s): "
				  default-whois-server)
			  nil nil default-whois-server)))
      (list host-ip whois-server)))
  (apply (function rgr-unauth-present-query-results) host-ip
	 (rgr-unauth-query-whois-server-internal host-ip whois-server)))

(defun rgr-unauth-query-arin-inserting-headers (host-ip &optional whois-server
							official-abuse-address)
  ;; Helper fn for rgr-unauth-send-complaint (below) -- though perhaps it should
  ;; be called "*-maybe-query-arin-*" instead.  Looks up the IP address at
  ;; whois-server (defaults to "whois.arin.net") in order to discover the ISP
  ;; that owns it.  We always want to look this up, because reverse DNS often
  ;; gives us the DNS name of a small company that doesn't have an abuse
  ;; address.  [created.  -- rgr, 5-Aug-00.]  [changed to skip lookup if there's
  ;; an "official" abuse address.  -- rgr, 16-Dec-00.]  [extended to insert POC
  ;; headers.  -- rgr, 16-Dec-02.]
  (let ((poc-source "local") (addresses nil) (net-range nil)
	(abuse-poc nil))
    (if official-abuse-address
	;; don't bother adding a detail line with the address in it if we have
	;; an "official" one; it's also the "to" address, most probably works,
	;; and we have it in our local IP->ISP database already.
	(rgr-unauth-insert-poc-headers poc-source)
	;; query and insert results.
	(apply (function rgr-unauth-insert-poc-headers)
	       (rgr-unauth-query-whois-server-internal host-ip whois-server)))))

(defun rgr-unauth-send-complaint (data &optional report-date)
  ;; Send an email message reporting the unauthorized connection attempt
  ;; contained in data, which is the result of the rgr-unauth-scarf-entry-data
  ;; function.  Assumes at least one unauthorized connection attempt in the
  ;; passed data.
  (let* ((host-ip (car data)) (host-name (car (cdr data)))
	 (attempt-alist (or (nth 2 data)
			    (error "Must have at least one illegal attempt.")))
	 (plural-p (or (cdr attempt-alist)
		       (> (cdr (car attempt-alist)) 1)))
	 (previous-reports (rgr-unauth-find-previous-reports-for-ip host-ip))
	 (last-report-address (car previous-reports))
	 (subnets (rgr-unauth-find-all-subnets host-ip))
	 (whois-server
	   (rgr-find-if (function rgr-subnet-whois-server) subnets))
	 (official-abuse-address
	   (rgr-find-if (function rgr-subnet-abuse-address) subnets))
	 (abuse-address (or official-abuse-address
			    last-report-address
			    (rgr-unauth-make-abuse-address host-name)))
	 (uid (rgr-unauth-make-uid))
	 ;; We put a unique identifier into the subject so that we can tell what
	 ;; is being acknowledged for acknowledgement messages that only quote
	 ;; the subject.  (Surprisingly, the sender's address is often useless
	 ;; for this.)  Unfortunately, some ISPs don't quote anything.
	 (subject (concat "Unauthorized"
			  (if (null (cdr attempt-alist))
			      (concat " " (car (car attempt-alist)))
			      "")
			  " probe"
			  (if plural-p "s" "")
			  " from " host-ip " [" uid "]")))
    (vm-mail-internal nil abuse-address subject)
    (v+q-set-return-address "rogers-abuse@rgrjr.dyndns.org")
    (rgr-unauth-insert-stuff report-date abuse-address host-name host-ip
			     attempt-alist plural-p previous-reports)
    (rgr-unauth-query-arin-inserting-headers host-ip whois-server
					     official-abuse-address)))

(defun rgr-unauth-scarf-attempt-alist ()
  ;; helper for rgr-unauth-scarf-entry-data, below.
  (let ((attempt-alist nil))
    (while (looking-at rgr-unauth-protocol-disposition-line-regexp)
      (let ((proto (match-string 1))
	    (disp (match-string 4)))
	(if (or (member disp '("REJECT" "DENY"))
		;; This will be accepted by the firewall, but may be an attack
		;; of some sort (if I don't recognize who it's coming from).
		(equal proto "ssh/tcp"))
	    (let ((attempts (string-to-int (match-string 2))))
	      (let ((entry (assoc proto attempt-alist)))
		(if entry
		    (setcdr entry (+ attempts (cdr entry)))
		    (setq attempt-alist
			  (cons (cons proto attempts) attempt-alist)))))))
      (forward-line))
    ;; Should now be on the next "From" line.
    attempt-alist))

(defun rgr-unauth-scarf-entry-data (&optional error-p)
  ;; Grab the host IP, the host name (if present), and the alist of (proto
  ;; . n-times) for rejected/denied connection attempts for the current entry.
  ;; Returns nil if there are no more matching lines and error-p is nil.
  (cond ((looking-at "^ *From \\([^ ]+\\) (\\([.0-9]+\\))")
	  (forward-line)
	  (list (match-string 2) (match-string 1)
		(rgr-unauth-scarf-attempt-alist)))
	((looking-at "^ *From \\[\\([.0-9]+\\)\\]")
	  ;; no reverse DNS.
	  (forward-line)
	  (list (match-string 1) nil
		(rgr-unauth-scarf-attempt-alist)))
	(error-p
	  (error "Not on a \"From host\" line."))
	(t
	  nil)))

(defun rgr-unauth-all-attempts-benign-p (data)
  ;; Returns T iff this guy connected only on benign protocols.
  (let ((result t) (tail (nth 2 data)))
    (while (and result tail)
      (setq result (string-match rgr-unauth-benign-protocol-regexp
				 (car (car tail))))
      (setq tail (cdr tail)))
    result))

(defun rgr-unauth-private-address-p (data)
  "Return non-nil if data is for a private address."
  ;; [added 169.254.0.0/16.  -- rgr, 2-Feb-01.]
  (string-match "^192\\.168\\.\\|^169\.254\.\\|^10\\.\\|^0\\.0\\.0\\.0$"
		(car data)))

(defun rgr-unauth-scarf-report-date ()
  (save-excursion
    (if (re-search-backward "^[A-Z][a-z][a-z] [ 0-9][0-9]:" nil t)
	(buffer-substring (match-beginning 0) (1- (match-end 0))))))

;;;###autoload
(defun rgr-unauthorized-connection ()
  (interactive)
  (beginning-of-line)
  (or (looking-at "^ +From ")
      (re-search-forward "^ +From " nil t)
      (error "No more rejected/denied connections in this buffer."))
  ;; We are now positioned at the start of a host summary.  Find the next host
  ;; summary with denied and/or rejected connections.
  (beginning-of-line)
  (let ((data nil))
    (while (and (setq data (rgr-unauth-scarf-entry-data))
		(cond ((null (nth 2 data))
			;; [debugging.]
			(cond (nil
				(message "Skipping %S" data)
				(sit-for 1)))
			t)
		      ((rgr-unauth-private-address-p data)
			(message "Skipping private address %S" data)
			(sit-for 1)
			t)
		      ((rgr-unauth-all-attempts-benign-p data)
			;; generate a message about attempts that were rejected
			;; but are considered "benign."
			(let ((strings nil) (tail (nth 2 data)))
			  (while tail
			    (let ((entry (car tail)))
			      (if strings
				  (setq strings (cons ", " strings)))
			      (setq strings
				    (cons (format "%s (%d)"
						  (car entry) (cdr entry))
					  strings)))
			    (setq tail (cdr tail)))
			  (message "Ignoring %s" (apply 'concat strings))
			  (sit-for 1)
			  t))))
      ;; loop for effect.
      )
    (if data
	(rgr-unauth-send-complaint data (rgr-unauth-scarf-report-date))
	(error "No more rejected/denied connections."))))

;;; debugging

(defun rgr-unauth-entry-n-address-blocks (entry)
  (let ((addresses (rgr-subnet-address entry)))
    (if (stringp addresses) 1 (length addresses))))

(defun rgr-unauth-count-address-blocks (&optional entry-list)
  (let ((tail (or entry-list rgr-unauth-abuse-addresses))
	(count 0))
    (while tail
      (setq count (+ count (rgr-unauth-entry-n-address-blocks (car tail))))
      (setq tail (cdr tail)))
    count))

;; [186 as of now.  -- rgr, 21-Jul-02.]
;; (rgr-unauth-count-address-blocks)

(defun rgr-unauth-summarize-buckets (buckets)
  (let ((i 0) (total 0))
    (while (< i 256)
      (let* ((entries (aref buckets i))
	     (n (length entries)))
	(if entries
	    (insert (format "%d -> %d entries\n" i n)))
	(setq total (+ total n)))
      (setq i (1+ i)))
    (insert (format "Total: %s\n" total))
    total))

;; (rgr-unauth-summarize-buckets (cdr rgr-unauth-bucketized-abuse-addresses))

