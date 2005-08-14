;;;*****************************************************************************
;;;
;;;    Random rmail hackery.
;;;
;;;    To compile this (almost) without error, do the following:
;;;
;;;	(mapcar 'require '(rmail))
;;;
;;; Actually, this still leaves an undefined function warning for the
;;; rgr-install-rmail-commands function, defined by rgr-rmail-18.el, which can't
;;; be loaded in emacs 19.  -- rgr, 25-Jan-95.
;;;
;;;    Modification history:
;;;
;;; rgr-rmail-mode-hook: no rgr-setup-rmail-aliases in e29.  -- rgr, 17-Jan-95.
;;; supply better rmail-default-rmail-file value.  -- rgr, 24-Jan-95.
;;; jan-mail-message-rgr: new.  -- rgr, 14-Sep-95.
;;; rgr-rmail-mode-hook & vt102 window hacking.  -- rgr, 14-Feb-96.
;;; rgr-rmail-mode-hook: must load rmailsum for this.  -- rgr, 30-Mar-96.
;;; rgr-rmail-mode-hook: must load rmailsum for this.  -- rgr, 30-Mar-96.
;;; psa-send-label-bug-reply: new (alas).  -- rgr, 9-Sep-96.
;;; ignore "^sender:\\|^organization:\\|^references:\\|".  -- rgr, 11-Feb-98.
;;; split rmail stuff out of the rgr-mail-hacks.el file.  -- rgr, 16-Dec-98.
;;; rgr-rmail-summary-output-to-rmail-file: kludge fix.  -- rgr, 16-Dec-98.
;;; rgr-rmail-mode-hook: move mail-yank-ignored-headers stuff to site-start.el
;;;	file.  -- rgr, 16-Feb-99.
;;;
;;; $Id$

;; Load rmailsum, because I'm going to use it anyway, and because I will want to
;; define commands in rmail-summary-mode-map.  (can't require this, because the
;; file doesn't provide anything, so we need to load at top level so we don't do
;; this multiple times.)  -- rgr, 30-Mar-96.
(load "rmailsum")

;; like rmail-summary-output-to-rmail-file, but handles a numeric arg correctly
;; when deleting after output.
(defun rgr-rmail-summary-output-to-rmail-file (&optional count)
  "Append the current message (or next N messages) to an Rmail file.
If the file does not exist, ask if it should be created.
If file is being visited, the message is appended to the Emacs
buffer visiting that file."
  (interactive "p")
  (save-excursion
    (set-buffer rmail-buffer)
    (let ((rmail-delete-after-output nil))
      ;; [this should use the same numeric arg we got.  -- rgr, 16-Dec-98.]
      (call-interactively 'rmail-output-to-rmail-file)))
  (if rmail-delete-after-output
      (let ((n (or count 1)))
	(while (> n 0)
	  (rmail-summary-delete-forward)
	  (setq n (1- n))))))

;;;; Hook function.

;;;###autoload
(defun rgr-rmail-mode-hook ()
  (define-key rmail-mode-map "\C-\M-l" 'rgr-switch-to-other-buffer)
  (setq rmail-delete-after-output t)
  (setq rmail-default-rmail-file (expand-file-name "~/mail/thread.rmail"))
  ;; These two seem to be new in e19.  -- rgr, 17-Jan-95.
  (setq rmail-reply-prefix "")
  (setq rmail-display-summary t)
  ;; Attempt to hack the windows.
  (setq rmail-summary-window-size (max (/ (frame-height) 4) 6))
  ;; fix bug in output with a numeric arg.
  (define-key rmail-summary-mode-map "o"
    'rgr-rmail-summary-output-to-rmail-file)
  (rgr-emacs-major-version-case
    (18
      ;; The emacs 19.27 version of rmail supports these directly.  -- rgr,
      ;; 17-Jan-95.
      (load "rgr-rmail-18")
      ;; forward ref into previously loaded file.
      (rgr-install-rmail-commands))
    ((19 lucid19 20)
      (define-key rmail-summary-mode-map [f1] 'rgr-reinvoke-rmail-from-summary)
      (define-key rmail-mode-map [f1] 'rgr-reinvoke-rmail))))

;; [hushes compiler warnings.  -- rgr, 16-Dec-98.]
;; (setq rmail-buffer nil)
