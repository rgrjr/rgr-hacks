;;;*****************************************************************************
;;;
;;;;    find-file-at-point hackery.
;;;
;;; [created.  -- rgr, 15-Nov-03.]
;;;

(eval-when-compile
  (require 'ffap))

(defvar rgr-ffap-file-to-application-map
	'(("\\.gnumeric$" "gnumeric")
	  ("\\.sxc$" "oocalc")
	  ("\\.xl[st]x?$" "oocalc")
	  ("\\.ods$" "oocalc")
	  ("\\.xl[st]x?$" "gnumeric")
	  ("\\.ods$" "gnumeric")
	  ;; ("\\.csv$" "gnumeric")	;; [doesn't work.  -- rgr, 18-Feb-04.]
	  ("\\.pp[st]x?$" "ooimpress")
	  ("\\.docx?$" "oowriter")
	  ("\\.doc$" "abiword")
	  ("\\.pdf$" "okular")
	  ("\\.pdf$" "acroread")
	  ("\\.svg$" "inkscape")
	  ("\\.dia$" "dia")
	  ;; image extensions.
	  ("\\.\\(png\\|jpe?g\\|gif\\)$" "gimp")))

;;;###autoload
(defun rgr-ffap-file-finder (file-name &optional wildcards)
  ;; This is supposed to be pin-compatible with find-file, so that ffap can call
  ;; it interactively if told to suppress its magic.
  (interactive "FFind file: \np")
  ;(message "[got %S]" file-name)
  (unless wildcards
    (let ((chosen-app nil) (tail rgr-ffap-file-to-application-map))
      (while tail
	(let* ((app-data (car tail))
	       (app-name (car (cdr app-data))))
	  (if (and (string-match (car app-data) file-name)
		   (y-or-n-p (format "Launch %S on %S? " app-name file-name)))
	      (setq chosen-app app-data
		    tail nil)
	    (setq tail (cdr tail)))))
      (let ((app-name (car (cdr chosen-app)))
	    (app-opts (cdr (cdr chosen-app))))
	(cond ((not chosen-app)
		(find-file file-name wildcards))
	      ((and (symbolp app-name) (fboundp app-name))
		(apply app-name file-name app-opts))
	      (t
		(apply (function start-process) app-name
		       (get-buffer-create (concat "*" app-name "*"))
		       app-name (append app-opts (list file-name)))))))))

;;;###autoload
(defun rgr-install-ffap ()
  (require 'ffap)
  ;; (require 'ffap-local-url-patch)
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
  ;; [enable this if you want "C-x C-f" to invoke the regular find-file command
  ;; and "C-u C-x C-f" to use the extra ffap features.  -- rgr, 9-Aug-03.]
  ;; (setq ffap-require-prefix t)
  (global-set-key "\C-x\C-f" 'find-file-at-point)
  (global-set-key "\C-x4\C-f" 'ffap-other-window))

(provide 'rgr-ffap)
