;;;; Playing with frames under X11.
;;;
;;;    Modification history:
;;;
;;; created.  -- rgr, 20-Aug-01.
;;;

(defvar rgr-interesting-frame-heights '(58 43 28))

(defun rgr-window-frame (&optional window)
  ;; If given no args, this effectively returns the "current" frame, i.e. the
  ;; frame of the current window.
  (window-frame (or window (next-window))))

;;;###autoload
(defun rgr-toggle-frame-height ()
  (interactive)
  (let ((new-height
	 (or (car (cdr (member (frame-height) rgr-interesting-frame-heights)))
	     (car rgr-interesting-frame-heights))))
    (message "Changing frame height to %d." new-height)
    (set-frame-height (rgr-window-frame) new-height)))

;; (global-set-key [f5] 'rgr-toggle-frame-height)
