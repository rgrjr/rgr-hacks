;;;; Hacking log-view-mode
;;;
;;; Put the following in your ~/.emacs file:
;;;
;;;	(add-hook 'log-view-mode-hook
;;;		  (function (lambda ()
;;;		    (require 'rgr-log-view)
;;;		    (define-key log-view-mode-map "=" 'rgr-log-view-diff))))
;;;
;;; [created.  -- rgr, 24-Jul-03.]
;;;
;;; $Id$

(require 'log-view)

(defun rgr-log-view-diff (file first-tag &optional other-tag)
  "Offer to compare the current revision to any other tag for this file.
The default is the next older revision, which shows the changes that were
committed to make the current revision, except that the default for the
initial revision is the next older revision.  By special dispensation, the
tag \"HEAD\" is accepted in any case combination.

Programmatically, rgr-log-view-diff accepts required FILE and FIRST-TAG
arguments, plus an optional OTHER-TAG; the other tag defaults to the
current revision in this case.  [It probably should be considered a bug
that there is no convenient way to specify the interactive default.  --
rgr, 4-Sep-03.]"
  (interactive
    (let* ((file (or (log-view-current-file)
		     (error "No current logged file.")))
	   (current-tag (or (log-view-current-tag)
		    (error "Can't find current tag for file %S." file)))
	   (next-tag
	     (condition-case error
		 (save-excursion
		   (log-view-msg-next)
		   (log-view-current-tag))
	       (error
		;; [this is because log-view-msg-next signals if we're
		;; physically past the initial revision.  -- rgr, 4-Sep-03.]
		;; (message "[got error %S]" error)
		nil)))
	   (default-tag
	     (if (and next-tag
		      (not (equal current-tag next-tag)))
		 next-tag
		 ;; must be looking at the initial revision, so offer the
		 ;; previous one as the default.  the quasi-redundant
		 ;; log-view-goto-rev makes it work when point is below the
		 ;; "revision" line of the current revision.
		 (save-excursion
		   (log-view-goto-rev current-tag)
		   (log-view-msg-prev)
		   (log-view-current-tag))))
	   (other-tag
	     (if current-prefix-arg
		 default-tag
		 (read-string (format "Compare %S tag %s to (default %S): "
				      file current-tag default-tag)
			      nil nil default-tag))))
      (list file current-tag other-tag)))
  (and (string-equal (upcase first-tag) "HEAD")
       (setq first-tag "HEAD"))
  (and (string-equal (upcase other-tag) "HEAD")
       (setq other-tag "HEAD"))
  '(message "log view: %S"
    (list 'file file 'first-tag first-tag 'other-tag other-tag))
  (vc-version-diff file first-tag other-tag))

(provide 'rgr-log-view)

;; (setq debug-on-error t)
