;;;*****************************************************************************
;;;
;;;; rgr-html-tags local server definitions.
;;;
;;;    This was split out of rgr-html-tags.el in order to hide local
;;; configuration information.  To get it (or something like it) loaded, put a
;;; form like:
;;;
;;;	(add-hook 'rgr-html-tags-load-hook '(lambda () (load "rgr-html-servers")))
;;; in an appropriate init file (e.g. your .emacs or site-start.el).  -- rgr,
;;; 20-Nov-00.
;;;
;;;    Modification history:
;;;
;;; created (split out of rgr-html-tags.el).  -- rgr, 20-Nov-00.
;;; oops; for got to update for .../ns3.2/ dir.  -- rgr, 7-Aug-01.
;;; update for AOLserver 3.4.  -- rgr, 28-Oct-01.
;;;

(rgr-html-define-server "bmerc-www.bu.edu" "^bmerc-www\\(\\.bu\\.edu\\)?$"
  ;; Damn, I hate symbolic links.  -- rgr, 6-Apr-00.
  :root-directory '("/usr/local/etc/httpd/htdocs"
		    "/usr/local/apache/htdocs"))

;; My home server.  -- rgr, 27-May-00.
(rgr-html-define-server "rgrjr.dyndns.org"
			"\\(^rgrjr\\.dyndns\\.org\\|rgrjr\\.com\\)$"
  :root-directory '("/usr/local/aolserver/servers/rgrjr/pages"))
