;;;****************************************************************************
;;;
;;;    mail mode hackery.
;;;
;;;    To compile this without error, do the following:
;;;
;;; (mapcar 'require '(rmail sendmail mailabbrev vm))
;;;
;;; [But it still fails on rmail-buffer (which is not declared by rmail), and
;;; mail-aliases-setup (which is not defined until 19.30).  -- rgr, 12-Feb-99.]
;;; [mail-aliases-setup appears to be obsolete by 19.31,
;;; rmail-summary-get-new-mail is not loadable via require, and xemacs also
;;; needs (require 'mail-abbrevs).  -- rgr, 11-Jul-02.]
;;;
;;;    Modification history:
;;;
;;; rgr-mail-mode-hook (& rmail), . . .  -- rgr, 7-Apr-94.
;;; *** emacs 19 update ***
;;;	Split out ./rgr-rmail-18.el .  -- rgr, 17-Jan-95.
;;; rgr-mail-mode-hook: (require 'mail-abbrevs) now.  -- rgr, 17-Jan-95.
;;; rgr-mail-mode-hook rename-buffer hack.  -- rgr, 23-Jun-95.
;;; rgr-sign-email: new.  -- rgr, 8-Sep-95.
;;; rgr-mail-mode-hook: emacs 18 robustification.  -- rgr, 6-Oct-95.
;;; rgr-sign-email: add email address.  -- rgr, 1-Dec-95.
;;; rgr-sign-email: insert divider if not at eob.  -- rgr, 12-Jan-96.
;;; rgr-sign-email: two-phase implementation.  -- rgr, 16-Feb-96.
;;; rgr-sign-email: don't kill indentation of appendix.  -- rgr, 19-Feb-96.
;;; rgr-mail-mode-hook: make "\C-c\C-s" do rgr-sign-email.  -- rgr, 7-Mar-96.
;;; move out of the ./rgr-hacks.el file.  -- rgr, 29-Mar-96.
;;; rgr-reinvoke-rmail & summary version.  -- rgr, 4-Apr-96.
;;; rgr-import-claris-works-document: new.  -- rgr, 13-Apr-96.
;;; rgr-reinvoke-rmail: kludge patch.  -- rgr, 17-Apr-96.
;;; rgr-mail-mode-hook: use 19.31 fill.  -- rgr, 4-Oct-96.
;;; rgr-mail-mode-hook: wrong.  -- rgr, 9-Oct-96.
;;; rgr-mail-mode-hook: alpha 19.28 has no mail abbrevs.  -- rgr, 19-Jul-97.
;;; rgr-sign-email: kill forwarding dreck.  -- rgr, 6-Feb-98.
;;; rgr-invoke-rmail & siblings now run vm on .vm files, rgr-reinvoke-rmail
;;;	ensures header in other window.  -- rgr, 13-Nov-98.
;;; rgr-invoke-rmail: make smarter about subordinate emacs.  -- rgr, 18-Nov-98.
;;; split out rmail stuff (except rgr-invoke-rmail) into the rgr-rmail-hacks.el
;;;	file.  -- rgr, 16-Dec-98.
;;; rgr-mail-abbrevs-setup: split out of rgr-mail-mode-hook.  -- rgr, 12-Feb-99.
;;; rgr-sign-email: no dividers in vm mime mail buffer.  -- rgr, 22-Sep-99.
;;; rgr-invoke-rmail: use vm as the standard at home.  -- rgr, 11-Dec-99.
;;; rgr-invoke-rmail: use vm everywhere.  -- rgr, 27-Dec-99.
;;; rgr-invoke-rmail: oops -- but not while "su".  -- rgr, 28-Dec-99.
;;; move rgr-vm-mail-yank-hook to the rgr-vm-hacks.el file.  -- rgr, 12-Jan-00.
;;; rgr-mail-mode-hook: clean up.  -- rgr, 19-Jan-00.
;;; flush unused rgr-rmail-summary-quit command.  -- rgr, 6-Mar-00.
;;; rgr-sign-email: generalized to N strings.  -- rgr, 3-May-00.
;;; rgr-invoke-rmail: rmail -> vm in prompt.  -- rgr, 17-Dec-00.
;;; new std strings, rgr-sign-email now takes numeric arg.  -- rgr, 25-May-01.
;;; rgr-email-signature-strings: add phone numbers.  -- rgr, 18-Jul-01.
;;; rgr-sign-email: no plusp/minusp in elisp.  -- rgr, 5-Aug-01.
;;; rgr-mail-abbrevs-setup: made work in xemacs 21.  -- rgr, 9-Aug-01.
;;; rgr-email-signature-strings: change port in URL.  -- rgr, 27-Aug-01.
;;; rgr-email-signature-strings: change port back.  -- rgr, 8-Sep-01.
;;; rgr-email-signature-strings: remove ESS.  -- rgr, 11-Jul-02.
;;; rgr-invoke-rmail: don't try to run vm if not installed.  -- rgr, 14-Nov-02.
;;;

;;;; mail mode hacks.

(defvar rgr-mail-buffer-index 0
  "Counter for generating mail buffer names.")
(defvar rgr-email-signature-strings '("\t\t\t\t\t-- Bob" " Rogers" "
					   http://rgrjr.dyndns.org/" "
					   781-646-9215")
  "List of strings to insert on successive rgr-sign-email calls.")

(defun rgr-mail-maybe-insert-divider ()
  ;; Expects the caller to do save-excursion if desired.
  (let ((start (point)))
    (skip-chars-forward " \t\n")
    (cond ((eobp))
	  ((and (featurep 'vm)
		vm-send-using-mime
		(get-text-property (point) 'vm-mime-object))
	    ;; vm forward or reply:  vm is going to mime-encode this message
	    ;; along with its attachments, so a divider would be redundant.
	    )
	  (t
	    ;; text after signature in a regular mail buffer (e.g. reply or
	    ;; forward): standardize whitespace (leaving the "\n" we just did,
	    ;; and any indentation on the current line), and insert a divider.
	    (skip-chars-backward " \t")
	    (delete-region start (point))
	    (insert "
------------------------------------------------------------------------
")
	    ;; Get rid of silly rmail "forwarded message" boilerplate.
	    (if (looking-at "^-+ Start of forwarded message -+\n")
		(replace-match ""))
	    (if (re-search-forward
		 "^-+ End of forwarded message -+\n" nil t)
		(replace-match ""))))))

;;;###autoload
(defun rgr-sign-email (&optional n)
  "Insert a 'formal' signature, as in an email message.  Invoke this
repeatedly (or use a prefix argument) to get successively more 'detailed' 
signatures, based on the rgr-email-signature-strings variable."
  (interactive "p")
  (let ((tail rgr-email-signature-strings)
	(insert-tail nil))
    (cond ((null n) (setq n 1))
	  ((< n 0) (error "Prefix arg must be non-negative.")))
    (while tail
      (let ((string (car tail)))
	(setq tail (cdr tail))
	(if (equal string
		   (buffer-substring (- (point) (length string))
				     (point)))
	    (setq insert-tail (or tail
				  (error "No more signature strings."))
		  tail nil))))
    (cond ((null insert-tail)
	    ;; if we couldn't find any of the strings, we must be starting a
	    ;; signature; fix up whitespace in this vicinity.  Note that this
	    ;; happens even if n is zero.
	    (insert (if (bolp) "\n" "\n\n")
		    (car rgr-email-signature-strings))
	    (save-excursion
	      (insert "\n")
	      (or (zerop n)
		  (rgr-mail-maybe-insert-divider)))
	    (setq insert-tail (cdr rgr-email-signature-strings))
	    (setq n (1- n))))
    ;; handle insertions for second or subsequent strings.
    (while (> n 0)
      (insert (or (car insert-tail)
		  (error "No more signature strings.")))
      (setq insert-tail (cdr insert-tail))
      (setq n (1- n)))))

;;;###autoload
(defun rgr-mail-fill-paragraph (arg)
  "Fill paragraph at or after point in a mail buffer.
Prefix arg means justify as well.  This version knows not to fill headers."
  (interactive "P")
  (save-excursion
    (forward-paragraph)
    (or (bolp) (newline 1))
    (let ((end (point)))
      (backward-paragraph)
      (cond ((= (point) (point-min))
	      (search-forward "--text follows this line--\n")
	      (if (>= (point) end)
		  (error "Can't fill mail headers."))))
      (fill-region-as-paragraph (point) end arg))))

;;;; Entering and exiting

;;; This says it is particular to rmail, but I've now taught it to invoke vm
;;; when appropriate.  Eventually it will favor vm (at which time I'll move it
;;; to the rgr-vm-hacks.el file).  -- rgr, 16-Dec-98.

(defun rgr-count-windows (&optional frame)
  ;; [doesn't this already exist?  -- rgr, 16-Dec-98.]
  (let* ((selected (frame-selected-window frame))
	 (next (next-window selected))
	 (n 1))
    (while (not (eq selected next))
      (setq n (1+ n)
	    next (next-window next)))
    n))

;;;###autoload
(defun rgr-invoke-rmail (&optional file-name-arg)
  "Invoke rmail (or vm), arranging it so that the headers are always in the
top window.  A numeric argument prompts for an RMAIL or vm file to read."
  (interactive
    (if (or current-prefix-arg
	    ;; Try not to have two copies of RMAIL in different emacs
	    ;; instantiations at the same time.  -- rgr, 18-Nov-98.
	    (and rgr-subordinate-emacs-p
		 (not (yes-or-no-p (concat "Running in a subordinate emacs; "
					   "really invoke vm on INBOX? ")))))
	(list (read-file-name "Run rmail or vm on file: "
			      nil nil t))))
  (let ((selected (selected-window))
	(next (next-window))
	(vm-p (fboundp 'vm)))
    (cond ((and file-name-arg
		vm-p
		(not (string-match "\\.rmail$" file-name-arg)))
	    ;; not rmail.
	    (vm-visit-folder file-name-arg))
	  ((and (null file-name-arg)
		vm-p
		;; (eq rgr-site 'home)
		(equal (user-real-login-name) "rogers"))
	    ;; use vm as the standard mail reader at home.  -- rgr, 11-Dec-99.
	    ;; [now do it at BMERC as well.  -- rgr, 27-Dec-99.]  [but not while
	    ;; logged in as psa or thread.  -- rgr, 28-Dec-99.]
	    (vm))
	  ;; rmail possibilities.
	  ((eq selected next)
	    ;; One window.
	    (rmail file-name-arg))
	  ((eq selected (next-window next))
	    ;; Two windows.  Invoke rmail in the bottom one.  [in the
	    ;; documentation, it's called frame-top-window, not
	    ;; frame-first-window . . .  -- rgr, 29-Mar-96.]
	    (if (eq (frame-first-window) selected)
		(select-window next))
	    (rmail file-name-arg))
	  (t
	    ;; Three or more windows.  Let rmail reset things.
	    (delete-other-windows)
	    (rmail file-name-arg)))))

(defun rgr-reinvoke-rmail (&optional file-name-arg)
  "Invoke rmail (or vm) from an rmail buffer.  As with the
rgr-invoke-rmail command, a numeric argument prompts for an RMAIL or vm
file to read."
  (interactive (if current-prefix-arg
		   (list (read-file-name "Run rmail or vm on file: "
					 nil nil t))))
  (if file-name-arg
      (rgr-invoke-rmail file-name-arg)
      (let ((summary-buffer (rmail-summary-exists)))
	(rmail-get-new-mail)
	(cond ((or (not (= (rgr-count-windows) 2))
		   (null summary-buffer))
		;; let rmail-summary make the buffer and/or change the windows.
		(rmail-summary))
	      ((eq (window-buffer (next-window)) summary-buffer)
		;; the other window is now looking at the headers; just go
		;; there.  [probably a bad assumption.  -- rgr, 4-Apr-96.]
		;; [definitely.  -- rgr, 17-Apr-96.]  [ok, ok; I'm working on
		;; it.  -- rgr, 6-Feb-98.]  [finally fixed?  -- rgr, 13-Nov-98.]
		(other-window 1))
	      (t
		;; two windows and a summary buffer; just put it there.
		(other-window 1)
		(switch-to-buffer summary-buffer))))))

(defun rgr-reinvoke-rmail-from-summary (&optional file-name-arg)
  "Invoke rmail (or vm) from an rmail summary buffer.  As with the
rgr-invoke-rmail command, a numeric argument prompts for an RMAIL or vm
file to read."
  (interactive (if current-prefix-arg
		   (list (read-file-name "Run rmail or vm on file: "
					 nil nil t))))
  (if file-name-arg
      (rgr-invoke-rmail file-name-arg)
      (rmail-summary-get-new-mail)))

;;;; Related hacks.

(defvar rgr-claris-works-text-replacments '(("\325" . "'")
					    ("\322" . "\"")
					    ("\323" . "\"")))

(defun rgr-import-claris-works-document ()
  (interactive)
  ;; Convert funny characters to regulation ASCII.
  (mapcar (function (lambda (pair)
	    (let ((target (car pair)) (replacement (cdr pair)))
	      (goto-char (point-min))
	      (while (search-forward target nil t)
		(replace-match replacement nil t)))))
	  rgr-claris-works-text-replacments)
  ;; Fill paragraphs.
  (goto-char (point-min))
  (while (not (eobp))
    (skip-chars-forward " \t\n")
    (end-of-line)
    (if (> (current-column) 80)
	(fill-paragraph nil))
    (forward-paragraph)))

;;;; Hook functions.

(defun rgr-mail-abbrevs-setup ()
  ;; don't need to autoload this, because it's only used from
  ;; rgr-mail-mode-hook, below.  [the define-key's below have to be done after
  ;; autoloading mail-abbrevs-setup defines the commands.  -- rgr, 12-Feb-99.]
  (cond ((eq rgr-emacs-flavor 'xemacs)
	  (require 'mail-abbrevs))
	(t
	  (mail-abbrevs-setup)
	  (define-key mail-mode-map "\C-n" 'mail-abbrev-next-line)
	  (define-key mail-mode-map "\M->" 'mail-abbrev-end-of-buffer))))

;;;###autoload
(defun rgr-setup-mail-commands (map)
  ;; [emacs 19.31, and possibly earlier versions, seems to handle headers
  ;; correctly.  -- rgr, 4-Oct-96.]  [wrong.  -- rgr, 9-Oct-96.]
  (define-key map "\M-q" 'rgr-mail-fill-paragraph)
  (define-key map "\C-cs" 'rgr-sign-email)
  ;; This is normally bound to mail-send (or vm-mail-send), but I keep mistyping
  ;; it for the rgr-sign-email command.  [should be able to bind this to nil,
  ;; but for some reason that doesn't work.  -- rgr, 19-Jan-00.]
  (define-key map "\C-c\C-s" nil))

;;;###autoload
(defun rgr-mail-mode-hook ()
  (rgr-text-mode-hook)
  (rgr-setup-mail-commands (current-local-map))
  (cond ((rgr-emacs-version-p 19 30)
	  ;; The 19.30 way of doing things.  -- rgr, 25-Mar-96.  [and presumably
	  ;; also the 20.x way as well.  -- rgr, 12-Feb-99.]
	  (add-hook 'mail-setup-hook 'rgr-mail-abbrevs-setup))
	((rgr-emacs-version-p 19 29)
	  (require 'mail-abbrevs)
	  (mail-aliases-setup)))
  ;; Generate a new buffer name each time.  -- rgr, 23-Jun-95.
  (let ((suffix (1+ rgr-mail-buffer-index)) (name nil))
    (while (get-buffer (setq name (format "*mail-%d*" suffix)))
      (setq suffix (1+ suffix)))
    (setq rgr-mail-buffer-index suffix)
    (rename-buffer name)))

;; (setq debug-on-error nil)
;; (frame-root-window) (frame-first-window) (frame-selected-window)

(provide 'rgr-mail-hacks)

