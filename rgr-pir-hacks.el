;;;*****************************************************************************
;;;
;;;; Hacking PIR mode.
;;;
;;;    This is for the *.imc and *.pasm files accepted by Parrot, the runtime
;;; system for Perl6.
;;;
;;; [created.  -- rgr, 12-Feb-05.]
;;;
;;; $Id$

;;;###autoload
(defun rgr-pir-mode-hook ()
  (modify-syntax-entry ?_ "_" pir-mode-syntax-table))

(provide 'rgr-pir-hacks)
