;;;*****************************************************************************
;;;
;;;;    find-file-at-point hackery.
;;;
;;; [created.  -- rgr, 15-Nov-03.]
;;;
;;; $Id$

(defvar rgr-ffap-file-to-application-map
	'(("\\.gnumeric$" "gnumeric")
	  ("\\.xls$" "gnumeric")
	  ;; ("\\.csv$" "gnumeric")	;; [doesn't work.  -- rgr, 18-Feb-04.]
	  ("\\.doc$" "abiword")
	  ("\\.pdf$" "acroread")))

;;;###autoload
(defun rgr-ffap-file-finder (file-name &optional wildcards)
  ;; This is supposed to be pin-compatible with find-file, so that ffap can call
  ;; it interactively if told to suppress its magic.
  (interactive "FFind file: \np")
  (let ((app-name nil) (tail rgr-ffap-file-to-application-map))
    (if (not wildcards)
	(while tail
	  (if (string-match (car (car tail)) file-name)
	      (setq app-name (car (cdr (car tail)))
		    tail nil)
	      (setq tail (cdr tail)))))
    (cond ((not app-name)
	    (find-file file-name wildcards))
	  ((y-or-n-p (format "Launch %S on %S? " app-name file-name))
	    (start-process app-name
			   (get-buffer-create (concat "*" app-name "*"))
			   app-name file-name))
	  (t
	    (find-file file-name)))))

;;;###autoload
(defun rgr-install-ffap ()
  (require 'ffap)
  (require 'ffap-local-url-patch)
  ;; this enables launching other applications based on file name syntax.
  (setq ffap-file-finder 'rgr-ffap-file-finder)
  ;; this disables attempts to use "foo.pl" as a file server in poland.  -- rgr,
  ;; 21-Oct-03.
  (setq ffap-machine-p-known 'reject)
  (setq ffap-machine-p-unknown 'reject)
  ;; this disables use and generation of "news:" and "mailto:" urls.
  (setq ffap-foo-at-bar-prefix nil)
  (setq ffap-url-regexp
	(concat "\\`\\("
		"file:"			; no host ok
		"\\|"
		"\\(ftp\\|https?\\|telnet\\|gopher\\|www\\|wais\\)://"
					; needs host
		"\\)."			; require one more character
		))
  ;; [enable this if you want "C-u C-x C-f" to invoke the regular find-file
  ;; command and "C-u C-x C-f" to use the extra ffap features.  -- rgr,
  ;; 9-Aug-03.]
  ;; (setq ffap-require-prefix t)
  (global-set-key "\C-x\C-f" 'find-file-at-point)
  (global-set-key "\C-x4\C-f" 'ffap-other-window)
  ;; Put these somewhere generally available.  [kept for backward compatibility.
  ;; -- rgr, 9-Aug-03.]
  (global-set-key "\C-x\M-\C-f" 'find-file-at-point)
  (global-set-key "\C-x4\M-\C-f" 'ffap-other-window))

(provide 'rgr-ffap)
