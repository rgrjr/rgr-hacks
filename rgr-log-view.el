;;;; Hacking log-view-mode
;;;
;;; Put the following in your ~/.emacs file:
;;;
;;;	(autoload 'rgr-log-view-mode-hook "rgr-log-view" nil t)
;;;	(add-hook 'log-view-mode-hook 'rgr-log-view-mode-hook)
;;;
;;; [created.  -- rgr, 24-Jul-03.]
;;;
;;; $Id$

(require 'log-view)

;;;###autoload
(defun rgr-log-view-mode-hook ()
  ;; [don't understand why i seem to need this only at home . . .  -- rgr,
  ;; 16-Feb-05.]
  (setq buffer-read-only nil))

(provide 'rgr-log-view)

;; (setq debug-on-error t)
