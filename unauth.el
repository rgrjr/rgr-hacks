;;;; Generating email reporting unauthorized network connection attempts.
;;;
;;;    Do M-x rgr-unauthorized-connection in a check-logs.pl report.  For hosts
;;; with rejected or denied connection attempts, a complaint message will be
;;; composed, and you will be left in mail mode to touch up and send the
;;; message.  Repeat for each offending host.
;;;
;;;    [old] Modification history:
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
;;; rgr-unauth-send-complaint: use rgr-unauth-make-uid.  -- rgr, 9-Mar-01.
;;; rgr-unauth-abuse-addresses: remove 62.224.0.0/14.  -- rgr, 22-Mar-01.
;;; rgr-unauth-insert-stuff: also check /var/log/messages.1.  -- rgr, 1-Apr-01.
;;; ...
;;; rgr-unauth-insert-local-hostname error check.  -- rgr, 21-Oct-01.
;;; rgr-unauth-insert-stuff: TIA before sig.  -- rgr, 22-Oct-01.
;;; fudge host name for reports to mediaone.  -- rgr, 28-Oct-01.
;;; rgr-unauth-insert-stuff: include numeric timezone.  -- rgr, 1-Nov-01.
;;; rgr-unauth-scarf-attempt-alist: also scarf ssh/tcp.  -- rgr, 13-Dec-01.
;;; attbi.net -> attbi.com, new netblock, fix my DNS name.  -- rgr, 14-Mar-02.
;;; rgr-unauth-make-abuse-address: support ".ad.jp".  -- rgr, 13-Apr-02.
;;; rgr-unauth-insert-stuff: mention NTP in time blurb.  -- rgr, 7-May-02.
;;; rgr-unauth-insert-stuff: extract previous attempts.  -- rgr, 13-May-02.
;;; bbnplanet.com -> genuity.net.  -- rgr, 4-Jul-02.
;;; rgr-unauth-benign-protocol-regexp, skip 1433 ("Spida" worm), correct gte.net
;;;	entry, more concentric.net, thrunet.com.  -- rgr, 5-Jul-02.
;;; rgr-unauth-class-a-entries interface to bucketized address DB, plus the
;;;	complete "CHINANET Sichuan province network".  -- rgr, 21-Jul-02.
;;; rgr-unauth-benign-protocol-regexp: added KaZaA.  -- rgr, 24-Jul-02.
;;; rgr-unauth-previous-month-log-file-name search.  -- rgr, 27-Aug-02.
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
;;; $Id$

(require 'rgr-unauth-db)
(require 'rgr-unauth-query)

(defvar rgr-unauth-use-log-report-date-p nil
  "*If nil, include all unreported log events.  If non-nil, use only
those log events generated since the start of the report period.")

(defvar rgr-unauth-email-return-address "rogers-abuse2@rgrjr.dyndns.org"
  "*Sender address to use for probe reports.")

(defvar rgr-unauth-enumerate-previous-attempts-p nil
  "*Whether to list previous attempts in gory detail.")

(defvar rgr-unauth-last-query-host nil
  "String naming the last server queried for POCs, e.g. \"whois.ripe.net\".
This is made buffer-local to the *whois.ripe.net* buffer.")

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
        (concat "^\\(ftp\\|21\\|ms-sql-[sm]\\|143[34]\\|epmap\\|135\\|8080"
		"\\|443\\|901\\|57"
		"\\|6346\\|1214\\|445\\|microsoft-ds\\|17300\\)/tcp$"
		"\\|^\\(epmap\\|135\\|ms-sql-[sm]\\|143[34]\\)/udp$")
  ;; 1433 is for the Microsoft SQL Server; see the "Spida" worm advisory on the
  ;; http://www.iss.net/security_center/alerts/advise118.php page.  I no longer
  ;; report these.  And 6346 is gnutella, and 1214 is KaZaA, both of which are
  ;; too ambiguous to report.  [445 aka "microsoft-ds" is for Windows file
  ;; service; there seems to be a new worm propagating.  -- rgr, 18-Dec-02.]
  ;; [don't really know what 17300 is, but it's probably another damn microsoft
  ;; thing.  -- rgr, 15-May-03.]  [add 8080/tcp, since crawlers seem to be
  ;; hitting it.  -- rgr, 13-Dec-03.]  [add 901/tcp because there are too many
  ;; of these, and 443/tcp (https) because I'm tired of it.  also flush 57,
  ;; because it only seems to be associated with 1433.  -- rgr, 18-Dec-03.]
  "Regexp that matches protocol names (e.g. 'ftp/tcp') for which we
should never complain even when somebody attempts to connect.  For
instance, FTP connections are benign, because I do run a Web server, so
somebody might legitimately suppose I was also running an FTP server.")

(defvar rgr-unauth-minimum-attempts 10
  "Minimum number of non-benign attempts on a given day before an IP is
worth noticing.  This should really be 1, but I don't have that much
time to kill.")

(defvar rgr-unauth-evil-protocol-regexp
	"^\\(ssh\\|22\\)/tcp$"
  "Regexp that matches 'evil' protocols, ones for which a connection
attempt is unambiguously malevolent.")

(defvar rgr-unauth-minimum-evil-attempts 1
  "Minimum number of 'evil' attempts (defined as matching
rgr-unauth-evil-protocol-regexp) on a given day before an IP is worth
noticing.")

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

;;; Generating the message.

(defvar rgr-unauth-ipchains-protocol-and-ports-regexp
        (let* ((digits "\\([0-9]+\\)")
	       (ip-address-colon-port (concat "\\([0-9.]+\\):" digits)))
	  (concat "PROTO=" digits " " ip-address-colon-port " "
		  ip-address-colon-port " "))
  "Matched substrings are (protocol source-ip source-port dest-ip dest-port).")

(defvar rgr-unauth-iptables-protocol-and-ports-regexp
        (let ((digits "\\([0-9]+\\)"))
	  (concat "PROTO=\\([0-9A-Z]+\\) SPT=" digits " DPT=" digits " "))
  "Matched substrings are (protocol source-ip source-port dest-ip dest-port).")

(defun rgr-unauth-extract-previous-attempts (&optional last-reported-entry-date
					     report-date)
  ;; This assumes we are narrowed down to just the log entries.  Identify the
  ;; ones we want to trim, and return a digested form for later summarization.
  (let* ((start (point-min))
	 (prev-end (set-marker (make-marker) (point)))
	 (result nil))
    (goto-char start)
    (let* ((date (or last-reported-entry-date report-date))
	   (regexp (and date (concat "^" (regexp-quote date)))))
      ;; (message "[using regexp date %S]" date)
      (cond ((null date)
	      ;; no report date, so use it all.
	      )
	    ((not (re-search-forward regexp nil t))
	      ;; this can happen when the last probe is several log files old.
	      ;; in any case, leave it all in so that the user can figure it
	      ;; out.
	      (message "No date %S in log?" date)
	      (sit-for 1))
	    (last-reported-entry-date
	      ;; we want to exclude anything we've already reported.  assume
	      ;; that if there are multiple lines matching this date, we've
	      ;; reported them all.
	      (beginning-of-line)
	      (while (looking-at regexp)
		(forward-line 1)))
	    (t
	      ;; this should be the "report-date", so include this line.
	      (beginning-of-line)))
      (set-marker prev-end (point)))
    ;; Now operate on lines between start & prev-end.
    (goto-char start)
    ;(message "Operating on lines between %S and prev-end %S" (point) prev-end)
    ;(sit-for 2)
    (while (< (point) prev-end)
      (let ((detail-start (point))
	    (detail-end (save-excursion
			  (forward-line 1)
			  (point)))
	    (date (buffer-substring (point) (+ (point) 15))))
	(cond ((re-search-forward rgr-unauth-ipchains-protocol-and-ports-regexp
				  detail-end t)
		(let ((proto (string-to-int (match-string 1)))
		      (dest-port (string-to-int (match-string 5))))
		  (setq result (cons (list date proto dest-port) result)))
	        ;; make the next line come to us.
	        (delete-region detail-start detail-end))
	      ((re-search-forward rgr-unauth-iptables-protocol-and-ports-regexp
				  detail-end t)
		(let ((proto (match-string 1))
		      (dest-port (string-to-int (match-string 3))))
		  (setq result (cons (list date proto dest-port) result)))
	        ;; make the next line come to us.
	        (delete-region detail-start detail-end))
	      (t
		(goto-char detail-end)))))
    ;; now get rid of the earlier detail lines.  [no; it's better to delete only
    ;; the detail lines we can parse.  -- rgr, 13-May-02.]
    ;; (delete-region start (point))
    (nreverse result)))

;; [doesn't work on suse 8.1, where hostname is "rgrjr" and the IP is for the
;; internal network.  -- rgr, 21-May-03.]
'(defun rgr-unauth-insert-local-hostname (&optional ip-address-p)
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

(defun rgr-unauth-insert-local-host-ip ()
  ;; [the name is a misnomer; this fn no longer inserts.  -- rgr, 5-Aug-04.]
  (save-excursion
    (let ((regexp (concat "inet addr:\\(" rgr-hacks-dotted-quad-regexp "\\)"))
	  (result nil))
      (set-buffer (get-buffer-create " *host-ip*"))
      (erase-buffer)
      (apply (function call-process) "/sbin/ifconfig" nil t nil)
      (goto-char (point-min))
      (while (and (not result)
		  (re-search-forward regexp nil t))
	(let ((match (match-string 1)))
	  (if (string-match "^192\\.168\\." match)
	      nil
	      (setq result match))))
      (or result "[ip unknown]"))))

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
      (let* ((day (car (discus-parse-date date 'past)))
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
    ;; (message "Got %S" previous-reports)
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
		(tail (and rgr-unauth-enumerate-previous-attempts-p
			   previous-attempts)))
	    (insert "\nThis is in addition to "
		    (cond ((= n-previous 0)
			   ;; this happens when the last reported attempt
			   ;; doesn't show up in the logs.
			   "previous attempt(s)")
			  ((> n-previous 1) "previous attempts")
			  (t "a previous attempt"))
		    " from this IP ")
	    (save-excursion
	      (and (search-backward "the following unauthorized")
		   (replace-match "the following further unauthorized" t t)))
	    (while tail
	      (let* ((attempt (car tail))
		     (date (car attempt)) (protocol (car (cdr attempt)))
		     (dest-port (car (cdr (cdr attempt))))
		     (proto-name (cond ((stringp protocol) protocol)
				       ((= protocol 6) "TCP")
				       ((= protocol 17) "UDP")
				       (t (format "PROTO%d" protocol)))))
		(insert " on " (substring date 0 6)
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
	      (equal abuse-address "abuse@attbi.com")
	      (equal abuse-address "abuse@comcast.net"))
	  ;; cheating for our ISP.
	  (insert "h009027bdf26f.ne.client2.attbi.com")
	  (insert (system-name)))
      (insert " (" (rgr-unauth-insert-local-host-ip) ").  ")
      (let* ((zone (current-time-zone))
	     (hrs (/ (car zone) 3600))
	     (mins (/ (mod (car zone) 3600) 60)))
	(insert (if plural-p "Times are " "Time is ")
		(car (cdr zone))
		(format " (%.2d%.2d)" hrs mins)
		", and this machine is an NTP server (see 
http://www.ntp.org/) that operates at stratum 3,
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
	       (rgr-unauth-query-whois-server-cached host-ip whois-server)))))

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
    (v+q-set-return-address rgr-unauth-email-return-address)
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
		(equal proto "22/tcp"))
	    (let* ((attempts (string-to-int (match-string 2)))
		   (entry (assoc proto attempt-alist)))
	      (if entry
		  (setcdr entry (+ attempts (cdr entry)))
		  (setq attempt-alist
			(cons (cons proto attempts) attempt-alist))))))
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

(defun rgr-unauth-too-few-attempts-p (data)
  ;; Returns T iff this guy made less than the minimum number of non-benign
  ;; connection attempts.
  (let ((total 0) (evil-total 0) (tail (nth 2 data)))
    (while tail
      (let* ((pair (car tail))
	     (proto (car pair)) (count (cdr pair)))
	(cond ((string-match rgr-unauth-evil-protocol-regexp proto)
		(setq total (+ count total))
	        (setq evil-total (+ count evil-total)))
	      ((string-match rgr-unauth-benign-protocol-regexp proto))
	      (t
	        (setq total (+ count total)))))
      (setq tail (cdr tail)))
    (and (< evil-total rgr-unauth-minimum-evil-attempts)
	 (< total rgr-unauth-minimum-attempts))))

(defun rgr-unauth-private-address-p (data)
  "Return non-nil if data is for a private address."
  ;; [added 169.254.0.0/16.  -- rgr, 2-Feb-01.]
  (string-match "^192\\.168\\.\\|^169\.254\.\\|^10\\.\\|^0\\.0\\.0\\.0$"
		(car data)))

(defun rgr-unauth-scarf-report-date ()
  (save-excursion
    (if (re-search-backward "^[A-Z][a-z][a-z] [ 0-9][0-9]:" nil t)
	(buffer-substring (match-beginning 0) (1- (match-end 0))))))

(defun rgr-unauth-summarize-attempt-alist (attempt-alist)
  ;; make a string that summarized the name and counts of each attempt alist
  ;; entry.
  (let ((strings nil) (tail attempt-alist))
    (while tail
      (let ((entry (car tail)))
	(if strings
	    (setq strings (cons ", " strings)))
	(setq strings
	      (cons (format "%s (%d)" (car entry) (cdr entry))
		    strings)))
      (setq tail (cdr tail)))
    (apply 'concat strings)))

;;;###autoload
(defun rgr-unauthorized-connection ()
  (interactive)
  ;; first, make sure we're in the right kind of buffer.
  (while (not (eq major-mode 'vm-mode))
    (cond ((string-match "^\\*mail-" (buffer-name))
	    ;; [bug: this might loop forever.  -- rgr, 5-Oct-03.]
	    (other-window 1))
	  ((string-match "^\\*whois" (buffer-name))
	    (bury-buffer))
	  (t
	    (switch-to-buffer "INBOX"))))
  ;; now look for the next rejection line.
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
			(message "Ignoring %s"
				 (rgr-unauth-summarize-attempt-alist
				   (nth 2 data)))
		        ;; (sit-for 1)
			t)
		      ((rgr-unauth-too-few-attempts-p data)
			;; generate a message about malicious attempts from a
			;; host that were not enough to be worth reporting.
			(message "Ignoring %s (too few)"
				 (rgr-unauth-summarize-attempt-alist
				   (nth 2 data)))
		        ;; (sit-for 1)
			t)
		      ((rgr-unauth-excepted-host-p (car data) (nth 2 data))
			;; tell about hosts that are considered "benign."
			(message "Ignoring host %s" (car data))
		        ;; (sit-for 1)
			t)))
      ;; loop for effect.
      )
    (if data
	(rgr-unauth-send-complaint data (rgr-unauth-scarf-report-date))
	(error "No more rejected/denied connections."))))

