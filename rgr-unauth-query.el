;;;; Querying NIC whois servers.
;;;
;;; [split out of the unauth.el file.  -- rgr, 23-Aug-03.]
;;;
;;; $Id$

(require 'rgr-unauth-db)

(defvar rgr-unauth-whois-servers
	'(("whois.arin.net" "(\\(NET\\(BLK\\)?-[^ \t\n\)]+\\))")
	  ("whois.ripe.net" "(\\(NETBLK-[^ \t\n\)]+\\))") ;; probably wrong
	  ;; [apnic is more verbose, so re-querying is actually redundant for
	  ;; our purposes.  -- rgr, 12-Dec-02.]
	  ;; ("whois.apnic.net" "^netname *: +\\([^ \t\n]+\\)")
	  ))

(defvar rgr-unauth-whois-results-cache nil
  "Used by rgr-unauth-query-whois-server-cached to store returned
values.  Format is (parsed-netrange . results).")

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

;;; Querying servers.

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

;;; Parsing query results.

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
  "This is the principal entry point for uncached noninteractive
queries, despite the name.  Performs all server querying, including
followups, parses the results, and returns a list of (last-query-host
. whois-fields) in the format prescribed by the appropriate
rgr-unauth-whois-server-regexps entry."
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

(defun rgr-unauth-query-find-cached-entry (ip &optional cache)
  (let ((tail (or cache rgr-unauth-whois-results-cache))
	(result nil))
    (while tail
      (let ((entry (car tail)))
	(if (rgr-unauth-subnet-match-p (car entry) ip)
	    (setq result (cdr entry)
		  tail nil)
	    (setq tail (cdr tail)))))
    result))

(defun rgr-unauth-query-whois-server-cached (host-ip whois-server)
  "This is the principal entry point for noninteractive queries.  If
there are cached results for this netblock, then return those.
Otherwise, queries the appropriate server(s), and parses and caches the
results.  Returns a list of (last-query-host . whois-fields) in the
format prescribed by the appropriate rgr-unauth-whois-server-regexps
entry in either case."
  (let* ((ip (rgr-unauth-parse-ip-address host-ip))
	 (entry (rgr-unauth-query-find-cached-entry ip)))
    (cond ((null entry)
	    (setq entry
		  (rgr-unauth-query-whois-server-internal host-ip whois-server))
	    (setq rgr-unauth-whois-results-cache
		  (cons (cons (rgr-unauth-parse-ip-subnet (car (cdr entry)))
			      entry)
			rgr-unauth-whois-results-cache))))
    entry))

(defun rgr-unauth-present-query-results (host-ip source &optional netrange
						 addresses abuse-poc)
  "Interactive interface to (uncached) server querying.
Prints a summary of the parsed results."
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
	    (and (re-search-forward rgr-hacks-dotted-quad-regexp)
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

(provide 'rgr-unauth-query)

;;; Debugging.

;; (rgr-unauth-query-find-cached-entry (rgr-unauth-parse-ip-address "24.29.147.127"))
;; (rgr-unauth-query-whois-server-cached "24.29.147.127" "whois.arin.net")
