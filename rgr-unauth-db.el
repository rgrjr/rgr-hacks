;;;; Database for email reports of unauthorized network connection attempts.
;;;
;;;    Code in this file supports the report logging interface.  See M-x
;;; rgr-unauthorized-connection for the user interface.
;;;
;;;    [old] Modification history:
;;;
;;; created.  -- rgr, 13-Dec-02.
;;; rgr-unauth-scarf-msg-entry: tweak addr, add source.  -- rgr, 14-Dec-02.
;;; moved subnet database here.  -- rgr, 23-Dec-02.
;;; more CHINANET Shanxi, RR-COMM-SOUTHEAST.  -- rgr, 27-Dec-02.
;;; more kornet.net, started "abuse@nic.bora.net".  -- rgr, 31-Dec-02.
;;; another jpnic entry.  -- rgr, 1-Jan-03.
;;; more chinanet, jpnic.  -- rgr, 5-Jan-03.
;;; more comcastpc.com.  -- rgr, 6-Jan-03.
;;; rgr-unauth-abuse-addresses: REMOVE entries for abuse@swbell.net and
;;;	abuse@corp.earthlink.net -- now picked up from ARIN.  -- rgr, 9-Jan-03.
;;; another whois.nic.ad.jp subnet.  -- rgr, 10-Jan-03.
;;; REMOVE attbi.com entry in favor of ARIN lookup.  -- rgr, 12-Jan-03.
;;; REMOVE att.net, uswest.net, & pbi.net in favor of ARIN.  -- rgr, 16-Jan-03.
;;; REMOVE abuse@t-ipnet.de in favor of RIPE lookup.  -- rgr, 18-Jan-03.
;;; REMOVE rr.com and kornet.net in favor of whois lookups.  -- rgr, 21-Jan-03.
;;; "CHINANET heilongjiang province network".  -- rgr, 22-Jan-03.
;;; REMOVE uu.net, xo.com in favor of whois lookups.  -- rgr, 23-Jan-03.
;;; flushed obsolete 211.200.0.0/16 entries.  -- rgr, 28-Jan-03.
;;; REMOVE abuse@genuity.net in favor of ARIN lookup.  -- rgr, 3-Feb-03.
;;; rgr-unauth-abuse-addresses: more krnic.  -- rgr, 6-Feb-03.
;;; rgr-unauth-abuse-addresses: more chinanet.  -- rgr, 8-Feb-03.
;;; REMOVE gte.net block now referred to verizon.  -- rgr, 9-Feb-03.
;;; more krnic.  -- rgr, 13-Feb-03.
;;; more krnic.  -- rgr, 16-Feb-03.
;;; REMOVE abuse@aol.com in favor of ARIN lookup.  -- rgr, 19-Feb-03.
;;; another chinanet block.  -- rgr, 23-Feb-03.
;;; REMOVE abuse@verizon.net in favor of ARIN lookup.  -- rgr, 25-Feb-03.
;;; REMOVE abuse@rogers.com in favor of ARIN lookup.  -- rgr, 27-Feb-03.
;;; add "abuse@verizon.net" for GTE netblock.  -- rgr, 1-Mar-03.
;;; rgr-unauth-abuse-addresses: LACNIC lacuna.  -- rgr, 3-Mar-03.
;;; REMOVE abuse@verio.net in favor of ARIN lookup.  -- rgr, 16-Mar-03.
;;; another nic.ad.jp netblock.  -- rgr, 20-Mar-03.
;;; another nic.ad.jp netblock.  -- rgr, 3-Apr-03.
;;; another nic.ad.jp netblock.  -- rgr, 10-Apr-03.
;;; rgr-unauth-scarf-msg-entry: include date, new log file.  -- rgr, 11-Apr-03.
;;; yet another nic.ad.jp netblock.  -- rgr, 12-Apr-03.
;;; rgr-unauth-find-previous-reports-for-ip: use  new fmt.  -- rgr, 15-Apr-03.
;;; REMOVE "abuse@bellsouth.net" in favor of ARIN lookup.  -- rgr, 24-Apr-03.
;;; another nic.ad.jp netblock.  -- rgr, 25-Apr-03.
;;;
;;; $Id$

(defvar rgr-unauth-report-log-file (expand-file-name "~/mail/abuse-log.tbl")
  "File for recording sent firewall log messages.")

(defvar rgr-unauth-report-entry-date-regexp
	(let* ((digit "[0-9]")
	       (2digit (concat digit digit)))
	  (concat "^ +\\(\\(Jan\\|Feb\\|Mar\\|Apr\\|May\\|Jun\\|"
		  "Jul\\|Aug\\|Sep\\|Oct\\|Nov\\|Dec\\) +" digit "+ "
		  2digit ":" 2digit ":" 2digit "\\)"))
  "Capture the date from a reported log entry.")

(defvar rgr-unauth-host-exceptions
	'(;; SSH connections into home from work.
	  ("66.9.243.106" nil "^22/TCP")
	  ;; SSH connections from BMERC at Boston University.
	  ("128.197.54.24" nil "^22/TCP")
	  ("128.197.54.63" nil "^22/TCP")
	  ;; See http://www.just4youx.net/scanning.html
	  ("80.253.125.28 - 80.253.125.32" nil
	   "^\\(3128\\|6588\\|8080\\|1[01]80\\)/TCP")
	  ;; According to DOD-CERT, all of these are spoofed.
	  ("21.0.0.0/8" nil))
  "Alist of (subnet parsed-subnet port-re) for host IP ranges for which
we want to some or all connection attempts, as controlled by port-re.
parsed-subnet should always be specified as nil.  If the port-re is nil
or missing, then all connection attempts are ignored.")

(defvar rgr-unauth-abuse-addresses
	'(;; From ARIN
	  (("24.159.0.0/16") "abuse@chartercom.com")
	  (("32.0.0.0/8") "abuse@att.com")
	  ;; [now mci.com.  -- rgr, 24-Nov-03.]
	  ;; ("63.0.0.0 - 63.61.255.255" "abuse@uunet.uu.net")
	  ("63.216.0.0 - 63.223.255.255" "abuse@cais.net")
	  (("64.152.0.0/13") "abuse@level3.com")
	  ("65.0.0.0 - 65.15.255.255" "noc-abuse@noc.home.net")
	  (("65.112.0.0 - 65.121.255.255"
	    "63.236.0.0 - 63.239.255.255")
	   "abuse@qwest.net")
	  ;; [sprint doesn't have their arin act together.  -- rgr, 20-Jan-03.]
	  (("65.160.0.0 - 65.174.255.255"
	    "204.212.0.0 - 204.215.255.255"
	    "206.228.0.0 - 206.231.255.255"
	    "207.12.0.0 - 207.15.255.255"
	    "208.0.0.0 - 208.35.255.255")
	   "abuse@sprint.net")
	  ("66.19.0.0/16" "security@starnetinc.com")
	  ("66.168.0.0 - 66.169.255.255" "abuse@chartercom.com")
	  (("68.32.0.0 - 68.63.255.255"
	    "68.80.0.0 - 68.87.255.255")
	   "abuse@comcastpc.com")
	  (("61.0.0.0 - 61.255.255.255"
	    "150.1.0.0 - 150.101.255.255"
	    "163.13.0.0 - 163.32.255.255"
	    "163.208.0.0 - 163.212.255.255"
	    "165.246.0.0/16"
	    "166.103.0.0 - 166.104.255.255"
	    "166.111.0.0/16"
	    "202.0.0.0 - 203.255.255.0"
	    "210.0.0.0 - 211.255.255.255"
	    "218.0.0.0/8"
	    "219.0.0.0/8"
	    "220.0.0.0/8"
	    "221.0.0.0/8") nil :whois "whois.apnic.net")
	  (("24.132.0.0/16"
	    "62.0.0.0/8"
	    "80.0.0.0/8"
	    "81.0.0.0/8"
	    "82.0.0.0/8"
	    "130.228.0.0/16"
	    "141.0.0.0 - 141.85.255.255"
	    "148.81.0.0/16"
	    "151.1.0.0 - 151.2.255.255"
	    "192.114.0.0 - 192.118.255.255"
	    "193.0.0.0/8"
	    "194.0.0.0/8"
	    "195.0.0.0/8"
	    "212.0.0.0/8"
	    "213.0.0.0/8"
	    "217.0.0.0/8")
	   nil :whois "whois.ripe.net")
	  (("61.192.0.0 - 61.199.255.255"
	    "61.200.0.0 - 61.215.255.255"
	    "133.0.0.0/8"
	    "202.32.0.0 - 202.35.255.255"
	    "202.216.0.0 - 202.219.255.255"
	    "203.136.0.0 - 203.141.255.255"
	    "203.178.0.0 - 203.183.255.255"
	    "210.160.0.0 - 210.175.255.255"
	    "210.232.0.0 - 210.235.255.255"
	    "210.224.0.0 - 210.225.255.255"
	    "210.248.0.0 - 210.255.255.255"
	    "211.0.0.0 - 211.7.255.255"
	    "211.120.0.0 - 211.135.255.255"
	    "218.40.0.0 - 218.47.255.255"
	    "219.96.0.0 - 219.127.255.255"
	    "219.160.0.0 - 219.165.255.255"
	    "219.166.0.0 - 219.167.255.255"
	    "220.96.0.0 - 220.99.255.255"
	    "220.208.0.0 - 220.223.255.255")
	   nil :whois "whois.nic.ad.jp")
	  ;; LACNIC is nominally "200.0.0.0/8" (of which whois.registro.br gets
	  ;; the top half), but there appears to be a gap at 200.62.0.0 -
	  ;; 200.62.63.255 for IFX Corporation, Florida, that is still handled
	  ;; by ARIN.  -- rgr, 2-Mar-03.
	  (("24.232.0.0/16"
	    "163.247.0.0/16"
	    "168.234.0.0/16"
	    "200.0.0.0 - 200.61.255.255"
	    "200.62.64.0 - 200.255.255.255") nil :whois "whois.lacnic.net")
	  (("200.96.0.0/13"
	    "200.128.0.0 - 200.255.255.255")
	   nil :whois "whois.registro.br")
	  (("66.12.0.0 - 66.14.255.255")
	   ;; ARIN reports this as belonging to GTE, but a comment says to use
	   ;; verizon.net for abuse reports.  -- rgr, 1-Mar-03.
	   "abuse@verizon.net")
	  ;; from whois.registro.br
	  (("200.158.0.0/17"
	    "200.171.0.0/16")
	    "security@telesp.net.br")
	  ;; From NIC.OR.KR (KRNIC)
	  (("61.254.88.0/24"
	    "211.52.246.0/24"
	    "211.109.72.0/24"
	    "211.110.63.0/24"
	    "211.110.168.0/24"
	    "211.243.230.0/24")
	   "abuse@thrunet.com")
	  ;; From APNIC
	  (("61.32.0.0 - 61.43.255.255") "abuse@bora.net")
	  (("61.72.0.0 - 61.77.255.255"
	    "61.78.0.0 - 61.85.255.255"		;; korea telecom
	    "61.96.0.0 - 61.111.255.255"
	    "61.248.0.0 - 61.255.255.255"
	    "202.30.0.0 - 202.31.255.255"
	    "203.224.0.0 - 203.224.255.255"
	    "203.226.0.0 - 203.239.255.255"
	    "203.240.0.0 - 203.243.255.255"
	    "203.244.0.0 - 203.247.255.255"
	    "203.248.0.0 - 203.255.255.255"
	    "210.90.0.0 - 210.91.255.255"
	    "210.92.0.0 - 210.95.255.255"
	    "210.96.0.0 - 210.97.191.255"
	    "210.99.0.0 - 210.99.255.255"
	    "210.100.0.0 - 210.103.223.255"
	    "210.104.0.0 - 210.123.255.255"
	    "210.124.0.0 - 210.127.255.255"
	    "210.178.0.0 - 210.183.255.255"
	    "210.204.0.0 - 210.207.255.255"
	    "210.216.0.0 - 210.219.255.255"
	    "210.220.0.0 - 210.223.255.255"
	    "211.32.0.0 - 211.39.255.255"
	    "211.40.33.0 - 211.40.33.255"
	    "211.41.0.0 - 211.63.255.255"
	    "211.104.0.0 - 211.119.255.255"
	    "211.168.0.0 - 211.171.255.255"
	    "211.172.0.0 - 211.199.255.255"
	    "211.200.0.0 - 211.205.255.255"
	    "211.206.0.0 - 211.211.255.255"
	    "211.212.0.0 - 211.215.255.255"
	    "211.216.0.0 - 211.225.255.255"
	    ;; "211.217.0.0 - 211.255.255.255"
	    "211.226.0.0 - 211.231.255.255"
	    "211.232.0.0 - 211.255.255.255"
	    "218.36.0.0 - 218.39.255.255"
	    "218.48.0.0 - 218.49.255.255"
	    "218.50.0.0 - 218.55.255.255"
	    "218.232.0.0 - 218.233.255.255"
	    "218.234.0.0 - 218.239.255.255"
	    "218.144.0.0 - 218.159.255.255"
	    "219.240.0.0 - 219.241.255.255"
	    "219.248.0.0 - 219.255.255.255"
	    "220.64.0.0 - 220.71.255.255"
	    "220.72.0.0 - 220.87.255.255"
	    "220.88.0.0 - 220.95.255.255"
	    "220.116.0.0 - 220.127.255.255"
	    "221.138.0.0 - 221.143.255.255"
	    "221.144.0.0 - 221.168.255.255")
	   nil :whois "whois.nic.or.kr")
	  (("61.216.0.0 - 61.227.255.255"
	    "202.39.128.0 - 202.39.255.255"
	    "203.66.0.0 - 203.66.255.255"
	    "203.69.0.0 - 203.69.255.255"
	    "203.74.0.0 - 203.74.255.255"
	    "203.75.0.0 - 203.75.255.255"
	    "210.241.224.0 - 210.241.255.255"
	    "210.242.0.0 - 210.242.255.255"
	    "210.59.128.0 - 210.59.255.255"
	    "210.61.0.0 - 210.61.255.255"
	    "210.62.248.0 - 210.62.255.255"
	    "210.65.0.0 - 210.65.255.255"
	    "210.71.128.0 - 210.71.255.255"
	    "211.20.0.0 - 211.23.255.255"
	    "211.72.0.0 - 211.72.255.255"
	    "211.75.0.0 - 211.75.255.255")
	   "abuse@hinet.net")
	  ;; from APNIC
	  (("61.187.0.0 - 61.187.255.255")
	   "abuse@ns.chinanet.cn.net")))
  "Alist of (ip-subnet abuse-address &key extras), culled from NIC sites.
This database is only necessary in those cases when the abuse address is
not obvious from the subscriber's DNS name.  For that reason, this list
tends to include a lot of foreign ISPs, since they seem not to implement
reverse DNS for their subscribers.  Do not put small ISPs on this list;
their abuse addresses are likely to change as they get gobbled up by the
big guys.

   After updating this, be sure to (setq rgr-unauth-whois-results-cache nil)
so that old queries are redone using the new database.")

(defvar rgr-unauth-bucketized-abuse-addresses nil
  "Cache used to hold all of the above in a more easily digestible
format.  Accessed via rgr-unauth-class-a-entries, which lazily calls
rgr-unauth-bucketize-address-blocks to rebuild it when needed.")

(defvar rgr-unauth-ip-address-regexp
	(let ((digits "\\([0-9]+\\)"))
	  (concat "^" digits "\\." digits "\\." digits "\\." digits "$")))
(defvar rgr-unauth-ip-subnet-regexp
	(concat (substring rgr-unauth-ip-address-regexp 0 -1)
		"/\\([0-9]+\\)$"))

;;; Subnet database.

(defun rgr-subnet-getf (subnet property)
  ;; helper for subnet fields.
  (let ((tail (cdr (cdr subnet)))
	(result nil))
    (while tail
      (if (eq (car tail) property)
	  (setq result (car (cdr tail))
		tail nil)
	  (setq tail (cdr (cdr tail)))))
    result))

;; Accessors for common subnet fields.  [should be used more consistently.  --
;; rgr, 31-Dec-00.]
(defsubst rgr-subnet-address (subnet) (car subnet))
(defsubst rgr-subnet-abuse-address (subnet) (car (cdr subnet)))
(defsubst rgr-subnet-whois-server (subnet) (rgr-subnet-getf subnet ':whois))

(defun rgr-unauth-parse-ip-address (address)
  (if (string-match rgr-unauth-ip-address-regexp address)
      (list (string-to-int (match-string 1 address))
	    (string-to-int (match-string 2 address))
	    (string-to-int (match-string 3 address))
	    (string-to-int (match-string 4 address)))
      (error "%S is not a valid IP address." address)))

(defun rgr-unauth-mask-ip (subnet-tail sig-bits orca-p)
  ;; Given a parsed IP address (or its tail) and the number of significant bits
  ;; remaining in the subnet part of the address, return an address with all the
  ;; local bits on (if orca-p is nil, the OR-with-complement) or off (otherwise,
  ;; the AND with the mask).  -- rgr, 24-Dec-00.
  (cond ((null subnet-tail) nil)
	((zerop sig-bits)
	  ;; out of the subnet part of the mask.
	  (cons (if orca-p 0 255)
		(rgr-unauth-mask-ip (cdr subnet-tail) 0 orca-p)))
	((< sig-bits 8)
	  ;; matching the last mask octet, which is partial, so we need to 
	  (let ((local-mask (lsh 255 (- sig-bits 8))))
	    (cons (if orca-p
		      (logand (lognot local-mask) (car subnet-tail))
		      (logior local-mask (car subnet-tail)))
		  (rgr-unauth-mask-ip (cdr subnet-tail) 0 orca-p))))
	(t
	  ;; still in the subnet octets.
	  (cons (car subnet-tail)
		(rgr-unauth-mask-ip (cdr subnet-tail) (- sig-bits 8)
				    orca-p)))))

(defun rgr-unauth-parse-ip-subnet (subnet)
  ;; Return a list of two parsed addresses:  the minimum and maximum values
  ;; within the range.  Accepts both an explicit range of "min-max", and the
  ;; "min/bits" formats.
  (cond ((string-match rgr-unauth-ip-subnet-regexp subnet)
	  (let ((ip (list (string-to-int (match-string 1 subnet))
			  (string-to-int (match-string 2 subnet))
			  (string-to-int (match-string 3 subnet))
			  (string-to-int (match-string 4 subnet))))
		(sig-bits (string-to-int (match-string 5 subnet))))
	    ;; Turn the IP address into the two bounding addresses.
	    (list (rgr-unauth-mask-ip ip sig-bits t)
		  (rgr-unauth-mask-ip ip sig-bits nil))))
	((string-match " *- *" subnet)
	  ;; Double bounds.  [Extract both addresses before parsing them, lest
	  ;; rgr-unauth-parse-ip-address reset match data.  -- rgr, 24-Dec-00.]
	  (let ((addr1 (substring subnet 0 (match-beginning 0)))
		(addr2 (substring subnet (match-end 0))))
	    (list (rgr-unauth-parse-ip-address addr1)
		  (rgr-unauth-parse-ip-address addr2))))
	((string-match rgr-unauth-ip-address-regexp subnet)
	  ;; we'll let an address stand for itself.
	  (let ((ip (list (string-to-int (match-string 1 subnet))
			  (string-to-int (match-string 2 subnet))
			  (string-to-int (match-string 3 subnet))
			  (string-to-int (match-string 4 subnet)))))
	    (list ip ip)))
	(t
	  (error "%S is not a valid IP subnet specification" subnet))))

(defun rgr-unauth-canonicalize-ip-address (address)
  ;; Really, we only check to make sure that there are the right number of
  (let ((rev-octets nil)
	(next 0))
    (while (string-match "[0-9]+\\(\.\\|$\\)" address next)
      (setq rev-octets (cons (match-string 0 address) rev-octets))
      (setq next (match-end 0)))
    (if (< next (length address))
	(error "%S is not a valid IP address." address))
    (let ((len (length rev-octets)))
      (while (< len 4)
	(setq rev-octets (cons ".0" rev-octets))
	(setq len (1+ len))))
    (apply (function concat) (nreverse rev-octets))))

(defun rgr-unauth-canonicalize-subnet (subnet)
  ;; Given a string, returns another string that fixes some common infelicities
  (cond ((string-match "/[0-9]+$" subnet)
	  (let ((ip (substring subnet 0 (match-beginning 0)))
		(bits (substring subnet (match-beginning 0))))
	    (concat (rgr-unauth-canonicalize-ip-address ip) bits)))
	((string-match " *- *" subnet)
	  ;; Double bounds.  [Extract both addresses before parsing them, lest
	  ;; rgr-unauth-parse-ip-address reset match data.  -- rgr, 24-Dec-00.]
	  (let ((addr1 (substring subnet 0 (match-beginning 0)))
		(addr2 (substring subnet (match-end 0))))
	    (concat (rgr-unauth-canonicalize-ip-address addr1)
		    " - "
		    (rgr-unauth-canonicalize-ip-address addr2))))
	(t
	  ;; we'll assume this is an address that stands for itself.
	  (rgr-unauth-canonicalize-ip-address subnet))))

(defun rgr-unauth-subnet-match-tail-p (mask-bits subnet-tail addr-tail)
  (cond ((zerop mask-bits)
	  ;; matched the entire mask.
	  t)
	((< mask-bits 8)
	  ;; match the last mask octet.
	  (let ((shift (- mask-bits 8)))
	    (= (lsh (car subnet-tail) shift)
	       (lsh (car addr-tail) shift))))
	;; still matching the mask octets.
	((= (car subnet-tail) (car addr-tail))
	  (rgr-unauth-subnet-match-tail-p (- mask-bits 8)
					  (cdr subnet-tail)
					  (cdr addr-tail)))))

(defun rgr-unauth-subnet-match-bounds-tail-p (min-tail addr-tail max-tail)
  ;; helper fn for rgr-unauth-subnet-match-p, below.
  ;; (message "Matching %S to %S:%S" addr-tail min-tail max-tail)
  (or (null addr-tail)
      (let ((addr (car addr-tail))
	    (min (if min-tail (car min-tail) 0))
	    (max (if max-tail (car max-tail) 255)))
	(cond ((= min max)
		(and (= min addr)
		     (rgr-unauth-subnet-match-bounds-tail-p (cdr min-tail)
							    (cdr addr-tail)
							    (cdr max-tail))))
	      ((or (< addr min) (> addr max))
		;; complete exclusion.
		nil)
	      ;; min/max specifies a range, and we fall somewhere within it.
	      ((= min addr)
		(rgr-unauth-subnet-match-bounds-tail-p (cdr min-tail)
						       (cdr addr-tail)
						       nil))
	      ((= addr max)
		(rgr-unauth-subnet-match-bounds-tail-p nil
						       (cdr addr-tail)
						       (cdr max-tail)))
	      (t
		;; must be completely contained.
		t)))))

(defun rgr-unauth-subnet-match-p (subnet ip)
  ;; Return non-nil iff the given IP address belongs to the given subnet.  Both
  ;; args may be IP address strings, or a parsed subnet/address.
  (let ((subnet-data
	  (cond ((consp subnet) subnet)
		((stringp subnet) (rgr-unauth-parse-ip-subnet subnet))))
	(address-data
	  (cond ((consp ip) ip)
		(t (rgr-unauth-parse-ip-address ip)))))
    (if (null subnet)
	(error "%S is not a valid subnet." subnet))
    (rgr-unauth-subnet-match-bounds-tail-p (car subnet-data)
					   address-data
					   (car (cdr subnet-data)))))

(defun rgr-unauth-subnet-size-tail (prefix-multiplier min max)
  ;; helper for rgr-unauth-subnet-size, below.
  (cond ((null (cdr min)) prefix-multiplier)
	((= (car min) (car max))
	  (rgr-unauth-subnet-size-tail prefix-multiplier (cdr min) (cdr max)))
	(t
	  (rgr-unauth-subnet-size-tail (* prefix-multiplier
					  (1+ (- (car max) (car min))))
				       (cdr min) (cdr max)))))

(defun rgr-unauth-subnet-size (subnet)
  ;; Return the number of addresses in the subnet as an integer divided by 256
  ;; (to avoid possible fixnum overflow problems).
  (let ((subnet-data
	 (cond ((consp subnet) subnet)
	       ((stringp subnet) (rgr-unauth-parse-ip-subnet subnet)))))
    (rgr-unauth-subnet-size-tail 1 (car subnet-data) (car (cdr subnet-data)))))

(defun rgr-unauth-subnet-set-match-p (addresses addr)
  ;; addresses can be either a single string, or a list of addresses (strings or
  ;; preparsed).  Returns the matching entry.
  (cond ((stringp addresses)
	  (if (rgr-unauth-subnet-match-p addresses addr)
	      addresses))
	((null addresses)
	  nil)
	((rgr-unauth-subnet-match-p (car addresses) addr)
	  (car addresses))
	(t
	  (rgr-unauth-subnet-set-match-p (cdr addresses) addr))))

(defun rgr-unauth-find-subnet (ip)
  ;; Crude, but effective.
  ;; [works only on the raw database; deprecated.  -- rgr, 21-Jul-02.]
  (let ((tail rgr-unauth-abuse-addresses)
	(addr (rgr-unauth-parse-ip-address ip))
	(result nil))
    (while tail
      (let ((subnet (car tail)))
	(if (rgr-unauth-subnet-set-match-p (rgr-subnet-address subnet) addr)
	    (setq result subnet
		  tail nil)
	    (setq tail (cdr tail)))))
    result))

;; Bucketizing.

(defun rgr-unauth-bucketize-address-blocks (entry-tail)
  ;; Given a list of ISP entries (see rgr-unauth-abuse-addresses), build a
  ;; dispatch array for the first octet, where each entry is an alist of
  ;; (subnet . entry).  Note that this speeds things up three ways:  (1) the
  ;; obvious improvement of an array over a linear list, (2) the subnet
  ;; addresses we need to search are preparsed, and (3) for an ISP or whois
  ;; server that straddles multiple class A networks, we need only search the
  ;; subnets that are in range.
  (let ((buckets (make-vector 256 nil))
	(count 0))
    (while entry-tail
      (let* ((entry (car entry-tail))
	     (addresses (rgr-subnet-address entry))
	     (addr-tail (if (stringp addresses) (list addresses) addresses)))
	(while addr-tail
	  (let* ((raw-subnet (car addr-tail))
		 (subnet (rgr-unauth-parse-ip-subnet raw-subnet))
		 (first-ip-first-octet (car (car subnet)))
		 (last-ip-first-octet (car (car (cdr subnet)))))
	    (while (<= first-ip-first-octet last-ip-first-octet)
	      (aset buckets first-ip-first-octet
		    (cons (cons subnet entry)
			  (aref buckets first-ip-first-octet)))
	      (setq first-ip-first-octet (1+ first-ip-first-octet))))
	  (setq addr-tail (cdr addr-tail))))
      (setq entry-tail (cdr entry-tail)))
    ;; Now that we've built the buckets, sort the entries in each alist by
    ;; subnet size, largest first.  That will make find-one matches succeed
    ;; sooner, and find-all-smallest-first generate them in the correct order.
    (let ((i 0))
      (while (< i 256)
	(let ((entries (aref buckets i)))
	  (if (cdr entries)
	      (aset buckets i
		    (sort entries (function (lambda (a b)
				    (> (rgr-unauth-subnet-size (car a))
				       (rgr-unauth-subnet-size (car b)))))))))
	(setq i (1+ i))))
    buckets))

(defun rgr-unauth-class-a-entries (ip-addr)
  ;; Given a parsed IP, return the alist of (subnet . entry) of all subnets with
  ;; the same class A address.  This is mostly syntactic sugar to hide the
  ;; existence of the bucket table, which it rebuilds if necessary.
  (or (and rgr-unauth-bucketized-abuse-addresses
	   (eq (car rgr-unauth-bucketized-abuse-addresses)
	       rgr-unauth-abuse-addresses))
      ;; cache empty or invalid.
      (setq rgr-unauth-bucketized-abuse-addresses
	    (cons rgr-unauth-abuse-addresses
		  (rgr-unauth-bucketize-address-blocks
		    rgr-unauth-abuse-addresses))))
  (aref (cdr rgr-unauth-bucketized-abuse-addresses) (car ip-addr)))

(defun rgr-unauth-find-all-subnets (ip)
  ;; Given an IP address, return a list of all containing subnets, sorted in
  ;; order of decreasing relevance (i.e. with the smallest netblock first).
  ;; Relies on the bucketized version of rgr-unauth-abuse-addresses, which was
  ;; designed to make things easy for us.
  (let* ((addr (rgr-unauth-parse-ip-address ip))
	 (tail (rgr-unauth-class-a-entries addr))
	 (result nil))
    (while tail
      (let ((pair (car tail)))
	(if (rgr-unauth-subnet-match-p (car pair) addr)
	    (setq result (cons (cdr pair) result))))
      (setq tail (cdr tail)))
    result))

;;; Exception database.

(defun rgr-unauth-excepted-host-p (ip attempt-alist)
  ;; if attempt-alist is empty, then this returns nil; if t, it returns the
  ;; regexp (if any).
  (let ((tail rgr-unauth-host-exceptions)
	(matched-entry nil))
    (while tail
      (let* ((entry (car tail))
	     (parsed-host (car (cdr entry))))
	(cond ((not parsed-host)
		(setq parsed-host (rgr-unauth-parse-ip-subnet (car entry)))
	        (setcar (cdr entry) parsed-host)))
	(if (rgr-unauth-subnet-match-p parsed-host ip)
	    (setq matched-entry entry tail nil)
	    (setq tail (cdr tail)))))
    (if matched-entry
	(let* ((port-re (nth 2 matched-entry))
	       (result (if (eq attempt-alist t) port-re t))
	       (tail (if (or (null port-re) (eq attempt-alist t))
			 nil
			 attempt-alist)))
	  ;; if we have a nontrivial regexp to check against a nonempty
	  ;; attempt-alist, and we haven't been told to just return the regexp,
	  ;; then return non-nil only if all attempts match.
	  (while tail
	    (if (not (string-match port-re (car (car tail))))
		(setq result nil tail nil)
		(setq tail (cdr tail))))
	  result))))

;; (rgr-unauth-excepted-host-p (rgr-unauth-parse-ip-address "80.253.125.31") '(("8081/TCP" 1)))
;; (string-match "^\\(3128\\|6588\\|8080\\)/TCP" "8080/TCP")

;;; Logging.

(defun rgr-unauth-find-previous-reports-for-ip (ip-string)
  "Returns a list of (last-address last-date last-tag &rest other-tags) if
it found at least one entry, else nil."
  (save-excursion
    (let ((expression (concat "^\\([^\t]*\\)\t" (regexp-quote ip-string) "\t"))
	  (result nil) (last-address nil) (last-date nil))
      (set-buffer (find-file-noselect rgr-unauth-report-log-file))
      (goto-char (point-max))
      ;; (message "using expression %S." expression)
      (while (re-search-backward expression nil t)
	(let ((tag (match-string 1)))
	  (if (null result)
	      ;; most recent report for this IP; snag the address field.
	      (save-excursion
		(let* ((fields (split-string
				 (buffer-substring-no-properties
				   (match-beginning 0)
				   (progn (end-of-line)
					  (point)))
				 "\t"))
		       (date (nth 2 fields))
		       (addr (nth 3 fields)))
		  (and date
		       (setq last-date date))
		  (and addr
		       (setq last-address addr)))))
	  (setq result (cons tag result))))
      (if result
	  (cons last-address (cons last-date (nreverse result)))))))

(defun rgr-unauth-scarf-msg-entry ()
  ;; try to extract a message log entry from the message in the current buffer.
  ;; currently, we have to punt on the POC source field.
  (save-excursion
    (goto-char (point-min))
    (let* ((header-end (save-excursion
			 (cond ((search-forward mail-header-separator nil t)
				 (match-beginning 0))
			       (t
				 (forward-paragraph)
				 (point)))))
	   (tag (save-excursion
		  (and (re-search-forward "^Subject:.*\\[\\([0-9]+\\)\\]"
					  nil header-end)
		       (match-string 1))))
	   (last-date
	     (save-excursion
	       (goto-char (point-max))
	       (and (re-search-backward rgr-unauth-report-entry-date-regexp
					nil t)
		    (match-string 1))))
	   (addr (save-excursion
		   ;; in case of multiple recipients, take only the first
		   ;; (e.g. "To: abuse@wanadoo.fr, postmaster@wanadoo.fr").
		   (and (re-search-forward "^To: *\\([^ \t\n,]+\\)"
					   nil header-end)
			(match-string 1))))
	   (source (save-excursion
		     (and (re-search-forward
			    "^x-abuse-poc-source: *\\([^ \t\n,]+\\)"
			    nil header-end)
			  (downcase (match-string 1)))))
	   (ip (save-excursion
		 (goto-char header-end)
		 (and (re-search-forward "[0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+"
					 nil t)
		      (match-string 0)))))
      (and tag addr last-date
	   (concat tag "\t" ip "\t" last-date
		   "\t" addr "\t" (or source "unknown") "\n")))))

(defun rgr-unauth-add-msg-entry ()
  ;; try to extract a message log entry from the current buffer, and add it to
  ;; the log file if successful.
  (let ((entry (rgr-unauth-scarf-msg-entry)))
    (cond (entry
	    (save-excursion
	      (set-buffer (find-file-noselect rgr-unauth-report-log-file))
	      (goto-char (point-max))
	      (insert entry)
	      (save-buffer)))
	  (t
	    (message "Not adding a log entry to %s."
		     rgr-unauth-report-log-file)))))

(defun rgr-unauth-mail-send-hook ()
  ;; The mail-send-actions list is local to each message, and runs after the
  ;; message has been passed to the MTA successfully, so this would be the ideal
  ;; place to update the database of sent abuse messages.  However, mail-send
  ;; always flushes this list, so we wouldn't get an update if the message is
  ;; re-sent, e.g. after fixing a broken contact address.  mail-send-hook is
  ;; global (i.e. shared by all messages), runs before the message is sent
  ;; (there is no mail-after-send-hook), so we can tweak mail-send-actions
  ;; there, but have to be careful to do that only for genuine abuse mail.  See
  ;; mail-send in /usr/share/emacs/20.7/lisp/mail/sendmail.el for gorier
  ;; details.
  (and rgr-unauth-report-log-file
       (rgr-unauth-scarf-msg-entry)
       (setq mail-send-actions
	     (cons '(rgr-unauth-add-msg-entry) mail-send-actions))))

(add-hook 'mail-send-hook 'rgr-unauth-mail-send-hook)

(provide 'rgr-unauth-db)

;;; debugging

;; (rgr-unauth-find-previous-reports-for-ip "66.176.192.178")

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
;; [down to 135, since we're relying more heavily on ARIN and friends.  -- rgr,
;; 23-Aug-03.]
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
