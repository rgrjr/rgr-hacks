;;;****************************************************************************
;;;
;;;    GNU Emacs makefile hackery.
;;;
;;;    To compile this without error, do the following:
;;;
;;;	  (mapcar 'require '(makefile shell))
;;;
;;; [created.  -- rgr, 19-Nov-96.]
;;;
;;; $Id$

(defun rgr-fill-makefile-comment (arg)
  "Fill the makefile comment around point."
  (interactive "P")
  (or (rgr-fill-prefix-comment "#")
      (error "Not in a comment.")))

;;;###autoload
(defun rgr-makefile-mode-hook ()
  "makefile-mode is for editing 'make' files."
  (define-key makefile-mode-map "\M-q" 'rgr-fill-makefile-comment)
  (define-key makefile-mode-map "\M-*" 'rgr-add-to-shell-modification-history))

