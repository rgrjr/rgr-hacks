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
  (setq pir-blink-matching-block nil)	;; this is buggy.
  (setq pir-electric-colon-p 'newline)	;; enable newline insertion
  (define-key pir-mode-map "\M-q" 'rgr-fill-comment)
  (define-key pir-mode-map "-" 'rgr-c-electric-dash)
  (modify-syntax-entry ?_ "_" pir-mode-syntax-table))

(provide 'rgr-pir-hacks)
