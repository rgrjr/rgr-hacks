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
;;; be loaded in emacs 19.  -- rgr, 25-Jan-95.  And for jan-mail-message-rgr,
;;; which refers to rmail-buffer freely (which is not declared by rmail).  --
;;; rgr, 28-Mar-96.  [got rid of jan-mail-message-rgr.  -- rgr, 18-Nov-98.]
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

;;;; Old stuff.

;; [no longer needed.  -- rgr, 18-Nov-98.]
'(defun jan-mail-message-rgr ()
  "Append the current RMAIL message to the ~/mail/jan.text file.
Named so I can type 'M-x jan' to invoke this."
  (interactive)
  (let ((local-rmail-buffer (if (eq major-mode 'rmail-summary-mode)
				rmail-buffer
				(current-buffer))))
    (save-excursion
      (set-buffer (find-file-noselect "~/mail/jan.text"))
      (goto-char (point-max))
      (insert "------------------------------------"
	      "------------------------------------\n")
      (insert-buffer local-rmail-buffer)
      ;; insert-buffer leaves point at the beginning, and pushes the mark at the
      ;; end, so we need to do C-u Space to get to the end.
      (goto-char (mark))
      (pop-mark)
      (insert "\n"))))

(defun rgr-snarf-line (tag)
  (save-excursion
    (if (re-search-forward tag nil t)
	(progn
	  (beginning-of-line)
	  (buffer-substring (point)
			    (progn
			      (forward-line)
			      (point)))))))

(defun psa-send-label-bug-reply ()
  "In rmail, generates an explanatory reply to an 'Expected a label' response.
Heuristicates the message body, and leaves you in mail mode, as via
rmail-reply."
  (interactive)
  (cond ((eq major-mode 'rmail-mode)
	 (rmail-reply t))
	((eq major-mode 'rmail-summary-mode)
	 (rmail-summary-reply t))
	(t
	 (error "Must be in rmail for this to work.")))
  (let (;; (*psa-bindings* nil) (request-id nil)
	(message nil) (to-address nil))
    ;; Get the headers and "interesting" part of the body.
    (save-excursion 
      (set-buffer mail-reply-buffer)
      (goto-char (point-min))
      (save-excursion
	(re-search-forward "^To: *\\(.*\\)$")
	(setq to-address (buffer-substring (match-beginning 1) (match-end 1))))
      (setq message (concat (rgr-snarf-line "^Date: ")
			    (rgr-snarf-line "^From: ")
			    (rgr-snarf-line "^To: ")
			    "\n"))
      (search-forward "We regret")
      (beginning-of-line)
      (let ((start (point)))
	(search-forward "\"" nil nil 2)
	(setq message (concat message
			      (buffer-substring start (point))
			      " . . .\n")))
      (search-forward "Syntax errors")
      (beginning-of-line)
      (let ((start (point)))
	(forward-paragraph)
	(setq message (concat message "\n"
			      (buffer-substring start (point))
			      "\n"))))
    (progn
      ;; Fixup reply headers.
      (goto-char (point-min))
      (replace-regexp "^To: .*$" (concat "To: " to-address))
      (replace-regexp "^Subject: .*$"
		      "Subject: Bug in psa-request WWW interface")
      (goto-char (point-max))
      (let ((start (point)))
	(insert message)
	(indent-rigidly start (point) 4))
      (insert "Your request was rejected by the mail server due to a bug in the
psa-request WWW interface, which failed to require you to fill in the
\"Subject\" field.  Please resubmit your request, adding a short \"Subject\"
string, which will be used to label plots and graphs.  We apologize for
the inconvenience, and hope you find our server useful.

					-- Bob Rogers
					   rogers@darwin.bu.edu
					   BMERC, 617-353-7123
"))))

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
