;;;****************************************************************************
;;;
;;;    mail mode hackery.
;;;
;;; [created at the dawn of time.  -- rgr, 23-Sep-07.]
;;;

(eval-when-compile
  ;; [This still warns on vm-user-composition-folder-buffer and
  ;; rmail-summary-get-new-mail.  -- rgr, 23-Sep-07.]
  (mapc 'require '(sendmail mailabbrev vm)))

;;;; mail mode hacks.

(defvar rgr-mail-buffer-index 0
  "Counter for generating mail buffer names.")
(defvar rgr-email-signature-strings '("\t\t\t\t\t-- Bob" " Rogers" "
					   http://www.rgrjr.com/" "
					   home: 781-646-9215" "
					   work: 617-441-6015")
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
------------------------------------------------------------------------\n")))))

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
    (if (save-excursion
	  (search-forward mail-header-separator nil t))
	(error "Can't sign the headers."))
    (while tail
      (let ((string (car tail)))
	(setq tail (cdr tail))
	(if (let ((prefix-start (- (point) (length string))))
	      (and (>= prefix-start (point-min))
		   (equal string (buffer-substring prefix-start (point)))))
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
	      (search-forward (concat mail-header-separator "\n"))
	      (if (>= (point) end)
		  (error "Can't fill mail headers."))))
      (fill-region-as-paragraph (point) end arg))))

;;;; Entering and exiting

;;; This says it is particular to rmail, but I've now taught it to invoke vm
;;; when appropriate.  Eventually it will favor vm (at which time I'll move it
;;; to the rgr-vm-hacks.el file).  -- rgr, 16-Dec-98.

;;;###autoload
(defun rgr-invoke-rmail (&optional file-name-arg)
  "Invoke vm (or rmail), arranging it so that the headers are always in the
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
	(next (next-window)))
    (cond ((null file-name-arg)
	    ;; Use vm as the standard mail reader.  If we are in a composition
	    ;; buffer, go to the related mail folder.
	    (require 'vm)
	    (let ((related-folder-buffer
		   ;; This is defined in vm 7.19, but not in vm 8.x.  -- rgr,
		   ;; 20-Apr-09.
		   (and (fboundp 'vm-user-composition-folder-buffer)
			(vm-user-composition-folder-buffer))))
	      (if (bufferp related-folder-buffer)
		  (vm (buffer-file-name related-folder-buffer))
		  (vm))))
	  ((and (fboundp 'vm) (not (string-match "\\.rmail$" file-name-arg)))
	    ;; Use vm on an explicit folder.
	    (require 'vm)
	    (vm-visit-folder file-name-arg))
	  ;; Rmail possibilities.
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

;;;; Hook functions.

;;;###autoload
(defun rgr-vm-mail-citation-hook (&optional message)
  "Invoked after a message has been yanked into a reply buffer."
  ;; Based on vm-mail-yank-default.
  (save-excursion
    (vm-reorder-message-headers nil vm-included-text-headers
				vm-included-text-discard-header-regexp)
    ;; if all the headers are gone, delete the trailing blank line, too.
    (if (eq (following-char) ?\n)
	(delete-char 1))
    (if (and message vm-included-text-attribution-format)
	(let ((vm-summary-uninteresting-senders nil))
	  (insert (vm-summary-sprintf vm-included-text-attribution-format
				      message))))
    (indent-rigidly (point) (point-max) 3)))

(defun rgr-mail-abbrevs-setup ()
  ;; don't need to autoload this, because it's only used from
  ;; rgr-mail-mode-hook, below.  [the define-key's below have to be done after
  ;; autoloading mail-abbrevs-setup defines the commands.  -- rgr, 12-Feb-99.]
  (mail-abbrevs-setup)
  (define-key mail-mode-map "\C-n" 'mail-abbrev-next-line)
  (define-key mail-mode-map "\M->" 'mail-abbrev-end-of-buffer))

(defvar rgr-mail-tweaked-bcc "rogers@rgrjr.com"
  "Replacement BCC address for rgr-mail-tweak-bcc.")
(defvar rgr-mail-address-change-note "
P.S.  I'm now using rogers@rgrjr.com as my preferred email address;
please add it to your contacts, lest I get relegated to spam.\n"
  "String to insert after the sig if rgr-mail-tweak-bcc gets a prefix arg.")

(defun rgr-mail-tweak-bcc (&optional insert-address-change-note-p)
  "Turn any 'BCC:' line into a BCC to home."
  (interactive "P")
  (save-excursion
    (goto-char (point-min))
    ;; Fix the BCC if there is one.
    (when (and rgr-mail-tweaked-bcc
	       (re-search-forward "^Bcc: " nil t))
      (let ((start (point)))
	(end-of-line)
	(delete-region start (point)))
      (insert rgr-mail-tweaked-bcc))
    ;; Fix the organization.
    (when (re-search-forward "^Organization: " nil t)
      (beginning-of-line)
      (when (looking-at "Organization: Modular")
	(let ((start (point)))
	  (forward-line 1)
	  (delete-region start (point)))))
    ;; Also insert an address change note if requested.
    (when insert-address-change-note-p
      (or (search-forward (car rgr-email-signature-strings) nil t)
	  (goto-char (point-max)))
      (forward-paragraph 1)
      (insert rgr-mail-address-change-note))))

;;;###autoload
(defun rgr-setup-mail-commands (map)
  ;; [emacs 19.31, and possibly earlier versions, seems to handle headers
  ;; correctly.  -- rgr, 4-Oct-96.]  [wrong.  -- rgr, 9-Oct-96.]
  (define-key map "\M-q" 'rgr-mail-fill-paragraph)
  (define-key map "\C-cs" 'rgr-sign-email)
  (define-key map "\C-ct" 'rgr-mail-tweak-bcc)
  ;; This is normally bound to mail-send (or vm-mail-send), but I keep mistyping
  ;; it for the rgr-sign-email command.  [should be able to bind this to nil,
  ;; but for some reason that doesn't work.  -- rgr, 19-Jan-00.]
  (define-key map "\C-c\C-s" nil))

;;;###autoload
(defun rgr-mail-setup-hook ()
  ;; This is designed for munging headers.
  (save-excursion
    (goto-char (point-min))
    (if (and mail-envelope-from
	     (not (equal user-mail-address mail-envelope-from))
	     (re-search-forward (concat "^BCC: "
					(regexp-quote user-mail-address)
					"$")
				nil t))
	(replace-match (concat "BCC: " mail-envelope-from) t t))))

;;;###autoload
(defun rgr-mail-mode-hook ()
  (rgr-text-mode-hook)
  (rgr-setup-mail-commands (current-local-map))
  ;; Generate a new buffer name each time.  -- rgr, 23-Jun-95.
  (let ((suffix (1+ rgr-mail-buffer-index)) (name nil))
    (while (get-buffer (setq name (format "*mail-%d*" suffix)))
      (setq suffix (1+ suffix)))
    (setq rgr-mail-buffer-index suffix)
    (rename-buffer name)))

(provide 'rgr-mail-hacks)
