;;;; Customizing the Emacs/W3 package.
;;;
;;;    Modification history:
;;;
;;; created.  -- rgr, 13-Jan-00.
;;; set url-privacy-level to high.  -- rgr, 28-Jan-00.
;;;

;;;###autoload
(defun rgr-w3-load-hook ()
  ;; (url-setup-privacy-info)
  (setq url-privacy-level 'high)
  ;; Disabling \240 as the hard space: Do the following after w3-vars.el is
  ;; loaded but before w3-parse.el . . .
  ;; (setcdr (assoc 'nbsp w3-html-entities) ? )
  ;; . . . or this afterwards.
  (put 'nbsp 'html-entity-expansion '(CDATA . " ")))

(provide 'rgr-w3-hacks)

