;;;*****************************************************************************
;;;
;;;    ange-ftp extensions.
;;;
;;;    This file defines (among others) the ange-ftp-set-host-canonical-name
;;; function, which allows host nicknames to be defined, and patches
;;; ange-ftp-ftp-name to return the canonical name.  One can use a variety of
;;; nicknames for a given host, and all will share the same FTP connection.
;;; However, the original name is not changed, so different file names could be
;;; used for the same file, which would confuse emacs.
;;;
;;;    Note that this file will not work under emacs 18 without modification;
;;; ange-ftp-ftp-name is called ange-ftp-ftp-path there.
;;;
;;;    Modification history:
;;;
;;; canonicalizing host nicknames.  -- rgr, 1-Feb-95.
;;; don't canonicalize local machine names.  -- rgr, 15-Mar-97.
;;; cleaned up a bit.  -- rgr, 17-Dec-97.
;;; (provide 'ange-ftp-hacks).  -- rgr, 15-Jan-98.
;;;

(require 'ange-ftp)

(defvar ange-ftp-host-hashtable (ange-ftp-make-hashtable)
  "Mapping from host nicknames to preferred names.")

(defun ange-ftp-set-host-canonical-name (nickname host)
  "Define NICKNAME as a short name for HOST."
  (ange-ftp-put-hash-entry nickname host ange-ftp-host-hashtable))

(defun ange-ftp-host-canonical-name (host)
  "Given a HOST name or nickname, return the preferred name."
  (or (ange-ftp-get-hash-entry host ange-ftp-host-hashtable)
      host))

(defun ange-ftp-ftp-name (name)
  ;; Version that supports host nicknames.
  (if (string-equal name ange-ftp-ftp-name-arg)
      ange-ftp-ftp-name-res
    (setq ange-ftp-ftp-name-arg name
	  ange-ftp-ftp-name-res
	  (save-match-data
	    (if (string-match (car ange-ftp-name-format) name)
		(let* ((ns (cdr ange-ftp-name-format))
		       (host (ange-ftp-host-canonical-name
			       (ange-ftp-ftp-name-component 0 ns name)))
		       (user (ange-ftp-ftp-name-component 1 ns name))
		       (name (ange-ftp-ftp-name-component 2 ns name)))
		  (if (zerop (length user))
		      (setq user (ange-ftp-get-user host)))
		  (list host user name))
	      nil)))))

(provide 'ange-ftp-hacks)

;;; bmerc applications.

(ange-ftp-set-host-canonical-name "prep" "prep.ai.mit.edu")
(ange-ftp-set-user "prep.ai.mit.edu" "anonymous")
(ange-ftp-set-host-canonical-name "rfc" "ds.internic.net")
(ange-ftp-set-user "ds.internic.net" "anonymous")
(ange-ftp-set-host-canonical-name "mw.ai.mit.edu" "mini-wheats.ai.mit.edu")
(ange-ftp-set-host-canonical-name "mini-wheats" "mini-wheats.ai.mit.edu")
(ange-ftp-set-host-canonical-name "mw" "mini-wheats.ai.mit.edu")

