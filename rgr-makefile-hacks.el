;;;****************************************************************************
;;;
;;;    GNU Emacs makefile hackery.
;;;
;;;    To compile this without error, do the following:
;;;
;;;	  (mapcar 'require '(makefile shell))
;;;
;;;    Modification history:
;;;
;;; created.  -- rgr, 19-Nov-96.
;;; use shell M-*.  -- rgr, 2-Dec-96.
;;;

;;;###autoload
(defun rgr-makefile-mode-hook ()
  "makefile-mode is for editing 'make' files."
  ;; rgr-fill-script-comment is defined in ./rgr-shell-hacks.el (from which it
  ;; is autoloaded); makefiles use the same syntax.
  (define-key makefile-mode-map "\M-q" 'rgr-fill-script-comment)
  (define-key makefile-mode-map "\M-*" 'rgr-add-to-shell-modification-history)
  )

