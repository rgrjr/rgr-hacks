;;;; Stuff for home use
;;;
;;;    Modification history:
;;;
;;; created (with rgr-mac-before-save-hook).  -- rgr, 17-Mar-00.
;;;

;;;###autoload
(defun rgr-mac-before-save-hook ()
  "If saving to ~rogers/Bob/ or a subdirectory, and the buffer file
end-of-line encoding is undefined, offer to change it to Mac style."
  (if (and (string-match "^/home/rogers/Bob/" buffer-file-name)
	   (or (null buffer-file-coding-system)
	       (null (coding-system-eol-type buffer-file-coding-system)))
	   (yes-or-no-p "Use Macintosh end-of-line encoding? "))
      ;; This code stolen from set-buffer-file-coding-system, which uses related
      ;; logic to default the new eol type from the old coding system.  We use
      ;; the reverse logic to reset the eol type and keep the rest of any coding
      ;; system.  -- rgr, 17-Mar-00.
      (let ((x (coding-system-eol-type 'undecided-mac))
	    (y (coding-system-eol-type
		 (coding-system-base (or buffer-file-coding-system
					 'undecided)))))
	(cond ((not (and (numberp x) (>= x 0) (<= x 2) (vectorp y)))
		(error "Internal error in %S" 'rgr-mac-before-save-hook)
	        (sit-for 2)))
	(set-buffer-file-coding-system (aref y x))))
  ;; Must return nil!
  nil)

(provide 'rgr-home-hacks)
