;;;*****************************************************************************
;;;
;;;    Hacking Zmacs-like possibilities buffers.
;;;
;;;    The essential added-value of possibilities buffers is that they stack,
;;; i.e. you can interrupt stepping through one set of things to look at
;;; something else for a while, then go back to what you were doing.  This is
;;; immensely helpful when (for instance) doing a series of grep's to remove
;;; uncalled functions; flushing one function may suggest other candidates that
;;; are no longer called, which can then be dealt with immediately before going
;;; back to the previous operation.
;;;
;;;    We customize a number of emacs features (most notably compilation mode)
;;; in order to push them onto the stack, and add object-oriented hooks so we
;;; can deal with them generically.
;;;

(eval-when-compile
  (require 'compile))

(defvar ilisp-possibilities-buffers nil
  "List of buffer names, some of which may have been deleted in the
interval.")

(defvar ilisp-next-possibility nil
  "Buffer-dependent command to go to the next possibility.")

(defvar ilisp-possibility-buffer-gensym-index 0
  "For generating unique buffer names.")

(defun ilisp-possibility-buffer-p ()
  "Returns T iff the current buffer is a possibility buffer."
  (not (null ilisp-next-possibility)))

(defun ilisp-current-possibility-buffer ()
  ;; Returns the top of the stack, ensuring that it exists.  May side-effect
  ;; ilisp-possibilities-buffers otherwise.
  (and ilisp-possibilities-buffers
       ;; speed hack.
       (or (get-buffer (car ilisp-possibilities-buffers))
	   (let ((buf nil) (tail ilisp-possibilities-buffers))
	     ;; buf is something we've tried to get (implicitly the first time
	     ;; through), and (car tail) is its name.
	     (while (and (null buf) (cdr tail))
	       (setq tail (cdr tail))
	       (setq buf (get-buffer (car tail))))
	     ;; either we found nothing, or we've found (car tail); update the
	     ;; stack to reflect any dead buffers popped.
	     (setq ilisp-possibilities-buffers (and buf tail))
	     buf))))

(defun ilisp-push-possibility-buffer (&optional reset-from-point-p)
  ;; Add the current buffer to the stack.  If already there, move it to the
  ;; front.  If reset-from-point-p supplied as non-nil, then do whatever the
  ;; mode requires to make the next possibility be the one at point.
  (setq ilisp-possibilities-buffers
	(cons (buffer-name)
	      (delete (buffer-name) ilisp-possibilities-buffers)))
  (if reset-from-point-p
      (let ((resetter (get major-mode 'reset-possibilities-from-point)))
	(if resetter
	    (funcall resetter)))))

(defun ilisp-gensym-possibility-buffer-name (mode)
  ;; Suitable as a compilation-buffer-name-function value.  [hack -- do this
  ;; only to grep buffers.  compilation buffers usually expire too quickly to be
  ;; of lasting interest.  -- rgr, 27-May-96.]
  (cond ((eq mode 'compilation) ;; (equal mode "compilation")
	  (concat "*" mode "*"))
	(t
	  (setq ilisp-possibility-buffer-gensym-index
		(1+ ilisp-possibility-buffer-gensym-index))
	  (format "*%s-%d*"
		  (downcase mode) ilisp-possibility-buffer-gensym-index))))

(defun ilisp-pop-possibilities (&optional message)
  "Take the current possibility buffer off the stack.  The buffer is not
killed, so it may be revisited by doing \\[ilisp-goto-possibility] in that
buffer."
  ;; should maybe restore the window configuration?  optionally?
  (interactive)
  (let ((old-buffer (or (ilisp-current-possibility-buffer)
			(error "No more possibilities."))))
    (setq ilisp-possibilities-buffers (cdr ilisp-possibilities-buffers))
    (or message
	(setq message (format "Popping %s" old-buffer)))
    (let ((new-buffer (ilisp-current-possibility-buffer)))
      (if new-buffer
	  (message "%s; C-. is now next %s possibility." message new-buffer)
	  (message "%s; no more possibilities." message)))))

(defun ilisp-goto-next-possibility (&optional n)
  "Go to the next possibility on the stack."
  (interactive "P")
  (cond ((ilisp-possibility-buffer-p)
	  (ilisp-push-possibility-buffer t)
	  (or n (setq n 0))))
  (let* ((buffer (or (ilisp-current-possibility-buffer)
		     (error "No more possibilities.")))
	 (command (save-excursion
		    (set-buffer buffer)
		    ilisp-next-possibility))
	 (old-point (point)) (old-buffer (current-buffer))
	 (result (funcall command buffer n)))
    ;; (message "%s in %s returned %s." command buffer result)
    (cond ((null result)
	    (ilisp-pop-possibilities (format "No more %s possibilities" buffer)))
	  ((eq old-buffer (current-buffer))
	    ;; Moved in the same buffer; remember where we were.  Don't generate
	    ;; a message, in case the command said something useful.
	    (push-mark old-point t)))))

(defun ilisp-next-possibility (&optional raw-prefix)
  "Go to the next possibility on the stack."
  (interactive "P")
  (let ((prefix (prefix-numeric-value raw-prefix)))
    (cond ((not raw-prefix)
	    (ilisp-goto-next-possibility))
	  ((< prefix 0) (ilisp-pop-possibilities))
	  ((= prefix 0)
	    (switch-to-buffer (or (ilisp-current-possibility-buffer)
				  (error "No current possibilities buffer."))))
	  ((> prefix 0) (ilisp-goto-next-possibility prefix)))))

;;;; compilation-mode interface

(defun ilisp-compile-next-possibility (buffer &optional n)
  ;; Returns a boolean.
  (setq compilation-last-buffer buffer)
  (condition-case error
      (progn (next-error n)
	     t)
    (error nil)))

(defun ilisp-push-compile-possibility-buffer ()
  ;; Use as a compilation-mode-hook value -- sets the current buffer up as a
  ;; compilation possibility buffer.
  (set (make-local-variable 'ilisp-next-possibility)
       'ilisp-compile-next-possibility)
  (ilisp-push-possibility-buffer))

(defun ilisp-compile-mode-reset-possibilities-from-point ()
  ;; Hookoid called from ilisp-push-possibility-buffer, above.  This is based on
  ;; the compile-goto-error function (but fixes a bug therein; see below).  --
  ;; rgr, 20-Oct-95.  [must set compilation-last-buffer so we don't confuse
  ;; compile, which has a different non-stack notion of which compilation buffer
  ;; is current.  but don't force reparsing, since the appropriate variables are
  ;; buffer local anyway.  -- rgr, 23-Dec-98.]
  (setq compilation-last-buffer (current-buffer))
  (beginning-of-line)
  ;; [this is unfortunately dependent on compilation-mode internals.  -- rgr,
  ;; 27-Oct-08.]
  (if (not (get-text-property (point) 'message))
      (let ((pos (next-single-property-change (point) 'message)))
	(and pos
	     (goto-char pos))))
  (compile-goto-error))

(put 'compilation-mode 'reset-possibilities-from-point
     'ilisp-compile-mode-reset-possibilities-from-point)
(put 'grep-mode 'reset-possibilities-from-point
     'ilisp-compile-mode-reset-possibilities-from-point)
(add-hook 'compilation-mode-hook 'ilisp-push-compile-possibility-buffer)
(setq compilation-buffer-name-function 'ilisp-gensym-possibility-buffer-name)

;;;; diff-mode interface

(defun ilisp-diff-next-possibility (buffer &optional n)
  ;; Returns a boolean.
  (condition-case error
      (progn (diff-next-error n nil)
	     t)
    (error
      (unless (equal error '(error "No next hunk"))
	(message "[error %S]" error)
	(sit-for 2))
      nil)))

(defun ilisp-push-diff-possibility-buffer ()
  ;; Use as a diff-mode-hook value -- sets the current buffer up as a
  ;; diff possibility buffer.
  (set (make-local-variable 'ilisp-next-possibility)
       'ilisp-diff-next-possibility)
  (ilisp-push-possibility-buffer))

(add-hook 'diff-mode-hook 'ilisp-push-diff-possibility-buffer)

;;;; wrapup.

(provide 'ilisp-possibilities)

;;;; testing

;; (ilisp-current-possibility-buffer)
;; (global-set-key [(?\C-\.)] 'ilisp-next-possibility)

