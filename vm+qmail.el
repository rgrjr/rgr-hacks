;;; Hacks for using qmail (as MTA) with VM (as MUA).
;;;
;;;    Modification history:
;;;
;;; created.  -- rgr, 21-Feb-00.
;;; v+q-base-return-address override, related changes.  -- rgr, 5-Mar-00.
;;; v+q-set-return-address: now a command, actually sets envelope 'From:'
;;;	address properly (at least for sendmail-send-it).  -- rgr, 2-Apr-00.
;;; v+q-set-return-address: user-mail-address hacking.  -- rgr, 4-Apr-00.
;;; v+q-user-full-name feature.  -- rgr, 17-Apr-00.
;;; v+q-mbox-status: use dir instead of vm-spool-files.  -- rgr, 18-Apr-00.
;;; "From:" header now obeys mail-from-style, change v+q-base-return-address
;;;	and v+q-user-full-name defaults to nil.  -- rgr, 20-Apr-00.
;;; took a stab at vm Maildir support, v+q-mbox-status now handles Maildir.
;;;	-- rgr, 23-Apr-00.
;;; vm-maildir-move-mail-internal: fix date format.  -- rgr, 9-May-00.
;;; v+q-display-message, related v+q-mbox-status cleanup.  -- rgr, 15-May-00.
;;; v+q-generate-password: use prefix for length, case-p.  -- rgr, 31-May-00.
;;; v+q-set-return-address: new terms, addr in inbox list.  -- rgr, 17-Dec-00.
;;; extend v+q-mbox-directory syntax in v+q-mbox-status, loosen
;;;	v+q-folder-special-return-address maildir "syntax."  -- rgr, 2-Jan-01.
;;; v+q-mbox-status: directory name ellipsis.  -- rgr, 6-Feb-01.
;;; v+q-mbox-status: vm-session-initialization call.  -- rgr, 11-Feb-01.
;;; v+q-mbox-status: mboxes in v+q-mbox-directory list.  -- rgr, 22-Apr-02.
;;; allow maildirs to end in "/" in v+q-folder-special-return-address, fix
;;;	vm-maildir-move-mail-internal verbosity and no-mail-but-modified bug.
;;;	-- rgr, 24-Jul-02.
;;;

(eval-when-compile
  (require 'sendmail)
  (require 'message)
  (require 'vm))

(require 'message)

(defvar v+q-base-return-address nil
  "This is the base 'user@host' string that should be augmented to
'user-list@host' for replies to mailing list 'list'.  If nil (the
default), the value of user-mail-address is used instead.")
(defvar v+q-user-full-name nil
  "*Full name (e.g. \"Bob Rogers\") for augmenting return lists.  Must
be RFC822-friendly -- \"Joe O'Rourke\" is OK, but \"Joe...Shmoe\" isn't.
\(Actually, I think this is now quoted correctly.  -- rgr, 2-Jan-01.)")
(defvar v+q-mbox-directory "~/"
  "*For the v+q-mbox-status command.  Can be either a string naming an
mbox file or a Maildir directory, or a list of such strings and/or
\(directory-name regxep) pairs.  If specified, the regexp will override
v+q-mbox-regexp, which supplies the default.  All files/subdirs in each
directory that match the corresponding regexp are considered
inboxes/maildirs (so be careful of . and ..!).")
(defvar v+q-mbox-regexp "^Mailbox\\|^Maildir"
  "*Default inbox regexp for the v+q-mbox-status command.")
(defvar v+q-verbose-p nil
  "*Enable this to get extra spew.")

(defvar folder-buffer)	;; to make the byte-compiler happy.
(defvar buffer-user-mail-address)	;; ditto.

(defun v+q-incoming-message-count (mbox-or-maildir)
  ;; Return the number of messages in mbox-or-maildir.  If an mbox, use grep to
  ;; count them; if a maildir use ls/wc.  [the maildir case is not actually used
  ;; any more.  -- rgr, 15-May-00.]
  (let ((maildir-p (file-directory-p mbox-or-maildir)))
    (with-current-buffer (get-buffer-create " *v+q temp*")
      (buffer-disable-undo)
      (erase-buffer)
      (if maildir-p
	  (call-process "csh" nil t nil "-c"
			(format "ls '%s/new' | wc -l" mbox-or-maildir))
	  ;; This isn't quite right, since there must be a blank line before it.
	  (call-process "grep" nil t nil "-c" "^From " mbox-or-maildir))
      (goto-char (point-min))
      (read (current-buffer)))))

(defun v+q-maildir-incoming-message-count (maildir)
  ;; Return the number of messages in maildir by counting the directory entries
  ;; in the maildir/new/ subdirectory.
  (let ((files (directory-files (expand-file-name "new/" maildir)))
	(count 0))
    (while files
      (or (member (car files) '("." ".."))
	  (setq count (1+ count)))
      (setq files (cdr files)))
    count))

(defun v+q-mbox-status-internal (inbox-list)
  (let ((result nil)
	(directory-tail (if (listp inbox-list)
			    inbox-list
			    (list inbox-list))))
    ;; [this forestalls a "Loading..." message if vm is autoloaded (7.15).  --
    ;; rgr, 24-May-04.]
    (require 'vm-undo)
    (while directory-tail
      (let* ((entry (car directory-tail))
	     (directory (if (consp entry) (car entry) entry))
	     (tail (cond ((not (file-readable-p directory))
			   nil)
			 ((not (file-directory-p directory))
			   (list directory))
			 ((and (file-directory-p
				 (expand-file-name "new" directory))
			       ;; [kludge to rule out maildirs named "new".  --
			       ;; rgr, 6-Jun-05.]
			       (not (file-directory-p
				      (expand-file-name "new/new" directory))))
			   ;; maildir
			   (list directory))
			 (t
			   ;; normal directory of inboxes.
			   (directory-files directory t
					    ;; extract regexp
					    (if (consp entry)
						(car (cdr entry))
						v+q-mbox-regexp)))))
	     (dir-printed-p nil))
	(while tail
	  ;; See if there's any mail in inbox.
	  (let* ((inbox (car tail))
		 (n-messages
		   (cond ((file-directory-p inbox)
			   (v+q-maildir-incoming-message-count inbox))
			 (t (v+q-incoming-message-count inbox)))))
	    (if (> n-messages 0)
		(let ((inbox-short-name
			(if dir-printed-p
			    (concat ".../" (file-name-nondirectory inbox))
			    (abbreviate-file-name inbox))))
		  (setq result
			(if result
			    (format "%s, %d in %s" 
				    result n-messages inbox-short-name)
			    (format "%d message%s in %s"
				    n-messages (if (= n-messages 1) "" "s")
				    inbox-short-name)))
		  (setq dir-printed-p t))))
	  (setq tail (cdr tail))))
      (setq directory-tail (cdr directory-tail)))
    (if result
	(message (concat result "."))
	(message "No messages."))))

;;;###autoload
(defun v+q-mbox-status ()
  "Summarize how many messages are waiting in all known mbox files and Maildir
directories, controlled by the v+q-mbox-directory and v+q-mbox-regexp variables.
Messages are reported regardless of whether the box or directory appears as an
inbox in the vm-spool-files list.  Doesn't handle POP or IMAP drops."
  (interactive)
  (require 'vm)
  ;; vm-session-initialization is necessary to ensure that ~/.vm is loaded
  ;; properly.  -- rgr, 11-Feb-01.
  (vm-session-initialization)
  (v+q-mbox-status-internal v+q-mbox-directory))

;;; Munging return addresses.

(defun v+q-folder-spool-file-entry (folder)
  ;; Given a folder, return its entry in vm-spool-files, if any.
  (let ((result nil)
	(tail (vm-spool-files)))
    (while tail
      (let* ((entry (car tail))
	     (folder-name (and (consp entry)
			       (car entry)))
	     (list nil))
	(if (and folder-name
		 (eq (get-file-buffer (expand-file-name folder-name)) folder))
	    (setq result entry
		  tail nil)
	    ;; no luck; keep searching.
	    (setq tail (cdr tail)))))
    result))

(defun v+q-default-mailing-list-return-address (mailing-list)
  ;; Return the right "From:" email address to use for replies to messages from
  ;; the named mailing list.
  (let ((new-return-address (or v+q-base-return-address
				user-mail-address)))
    (if (string-match "@" new-return-address)
	(replace-match (concat "-" mailing-list "@")
		       t t new-return-address)
	user-mail-address)))

(defun v+q-folder-special-return-address (folder)
  ;; Return the right "From:" address for replies to messages from the given
  ;; folder (a buffer), or nil if the default "From:" address will do.  The
  ;; vm-spool-files might specify a particular address (in the undocumented
  ;; fourth entry), or the inbox/maildir name might imply a mailing list with a
  ;; separate subscriber address.  For a list "foo" in the latter case, the
  ;; inbox/maildir name must end with ".foo", and there must be a ~/.qmail-foo
  ;; file (which presumably directs qmail-local to put it in the given
  ;; inbox/maildir).
  (let ((entry (v+q-folder-spool-file-entry folder)))
    (and entry
	 ;; overriding name
	 (or (nth 3 entry)
	     ;; check for the inbox/maildir & .qmail file naming convention.
	     (let* ((inbox-name (and (consp entry)
				     (consp (cdr entry))
				     (car (cdr entry))))
		    (list-name
		      (cond ((null inbox-name) nil)
			    ((string-match "\\.[^./]+$" inbox-name)
			      (substring inbox-name (1+ (match-beginning 0))))
			    ((file-directory-p inbox-name)
			      ;; really a maildir; we'll use the whole name in
			      ;; that case.
			      (file-name-nondirectory
			        (let ((len (length inbox-name)))
				  (if (= (aref inbox-name (1- len)) ?/)
				      (substring inbox-name 0 (1- len))
				      inbox-name)))))))
	       (if (and list-name
			(file-readable-p (concat "~/.qmail-" list-name)))
		   (v+q-default-mailing-list-return-address list-name)))))))

(defun v+q-mail-buffer-special-return-address ()
  ;; Return the mailing list name (not the address) for the list associated with
  ;; the (current) reply buffer.
  (let ((folder
	  ;; using just vm-mail-buffer would be cleaner, but vm-mail-internal
	  ;; doesn't set it until after mail-mode is invoked.  so we must look
	  ;; at folder-buffer as well, which vm-mail-internal binds.
	  (or (and (boundp 'vm-mail-buffer) vm-mail-buffer)
	      (and (boundp 'folder-buffer) folder-buffer))))
    (and folder
	 (v+q-folder-special-return-address folder))))

(defun v+q-sendmail-send-it ()
  ;; user-mail-address is a buffer-local variable, but sendmail-send-it won't
  ;; see the new value because it copies the buffer contents into a scratch
  ;; buffer before mailing.  [haven't tried smtpmail-send-it yet, but it appears
  ;; to do the same.  it'd be nice to have lexical closures here . . .  -- rgr,
  ;; 21-Feb-00.]
  (kill-local-variable 'user-mail-address)	;; be sure it's gone.
  (let ((user-mail-address (or buffer-user-mail-address
			       user-mail-address)))
    ;; [this doesn't work; sendmail-send-it sees the old value after doing
    ;; (set-buffer tembuf).  -- rgr, 21-Feb-00.]  [works now; the trick is not
    ;; to try to make user-mail-address buffer-local.  -- rgr, 2-Apr-00.]
    (and v+q-verbose-p
	 (message "Binding user-mail-address to %S" user-mail-address))
    (sendmail-send-it)))

(defun v+q-insert-from-header (login)
  ;; Stolen from the sendmail-send-it function; inserts a "From:" header at
  ;; (point-min) according to message-from-style, with proper RFC822 quoting and
  ;; encoding.  -- rgr, 20-Apr-00.
  (let ((fullname (or v+q-user-full-name (user-full-name)))
	(quote-fullname nil))
    (message-remove-header "from")
    (if (string-match "[\200-\377]" fullname)
	(setq fullname (mail-quote-printable fullname t)
	      quote-fullname t))
    (goto-char (point-min))
    (cond ((member mail-from-style '(default angles))
	   (insert "From: " fullname)
	   (let ((fullname-start (+ (point-min) 6))
		 (fullname-end (point-marker)))
	     (goto-char fullname-start)
	     ;; Look for a character that cannot appear unquoted
	     ;; according to RFC 822.
	     (if (or (re-search-forward "[^- !#-'*+/-9=?A-Z^-~]"
					fullname-end 1)
		     quote-fullname)
		 (progn
		   ;; Quote fullname, escaping specials.
		   (goto-char fullname-start)
		   (insert "\"")
		   (while (re-search-forward "[\"\\]"
					     fullname-end 1)
		     (replace-match "\\\\\\&" t))
		   (insert "\""))))
	   (insert " <" login ">\n"))
	  ((eq mail-from-style 'parens)
	   (insert "From: " login " (")
	   (let ((fullname-start (point)))
	     (if quote-fullname
		 (insert "\""))
	     (insert fullname)
	     (if quote-fullname
		 (insert "\""))
	     (let ((fullname-end (point-marker)))
	       (goto-char fullname-start)
	       ;; RFC 822 says \ and nonmatching parentheses
	       ;; must be escaped in comments.
	       ;; Escape every instance of ()\ ...
	       (while (re-search-forward "[()\\]" fullname-end 1)
		 (replace-match "\\\\\\&" t))
	       ;; ... then undo escaping of matching parentheses,
	       ;; including matching nested parentheses.
	       (goto-char fullname-start)
	       (while (re-search-forward 
		       "\\(\\=\\|[^\\]\\(\\\\\\\\\\)*\\)\\\\(\\(\\([^\\]\\|\\\\\\\\\\)*\\)\\\\)"
		       fullname-end 1)
		 (replace-match "\\1(\\3)" t)
		 (goto-char fullname-start))))
	   (insert ")\n"))
	  (t
	   (insert "From: " login "\n")))))

;;;###autoload
(defun v+q-set-return-address (new-return-address)
  "Set the envelope and reply addresses to the given return address.
Interactively prompts for a new return address."
  (interactive
    (list (let ((default (or (v+q-mail-buffer-special-return-address)
			     user-mail-address)))
	    (read-string "'From' user: " (cons default 0) nil default))))
  (let ((old-user-mail-address user-mail-address))
    ;; Add default Reply-To: field.  (actually, vm-mail-internal does this for
    ;; us once we set mail-default-reply-to appropriately.  -- rgr, 21-Feb-00.]
    ;; [no; better to set the 'From:' header.  -- rgr, 2-Apr-00.]
    ;; (make-local-variable 'mail-default-reply-to)
    ;; (setq mail-default-reply-to new-return-address)
    ;; Add a "From:" header based on new-return-address, so that replies go
    ;; to the same inbox as list mail.  If we don't do this, sendmail-send-it
    ;; will build one based on user-mail-address.
    ;; [***bug***: this save-excursion works if i call this interactively, but
    ;; it seems to have the opposite effect from the hook!  -- rgr, 17-Dec-00.]
    (progn 'save-excursion
      (v+q-insert-from-header new-return-address))
    ;; Customize send-mail-function for sendmail, since sendmail-send-it copies
    ;; the buffer contents into a scratch buffer.
    (cond ((not (member send-mail-function
			'(v+q-sendmail-send-it
			  sendmail-send-it smtpmail-send-it)))
	    ;; The send-mail-function will already be v+q-sendmail-send-it if
	    ;; the user does it again (i.e. change of mind).
	    (error "Oops; can't set return address for the '%S' sender."
		   send-mail-function))
	  ((rgr-emacs-version-p 21 3)
	    ;; emacs 21 supports this directly (but 21.2 and 21.3 require a
	    ;; patch to make it work when these are buffer-local).  -- rgr,
	    ;; 3-Jul-03.
	    (set (make-local-variable 'mail-specify-envelope-from) t)
	    (set (make-local-variable 'mail-envelope-from) new-return-address))
	  (t
	    (error "Emacs version not supported.")))
    (if (and v+q-verbose-p
	     (not (equal new-return-address old-user-mail-address)))
	(message "Changed %S to %S."
		 old-user-mail-address new-return-address))
    t))

;;;###autoload
(defun v+q-maybe-change-return-address ()
  "Possibly customize the 'From:' address based on a folder mailing list.
Put this on mail-mode-hook to use it."
  (let ((address (v+q-mail-buffer-special-return-address)))
    (if address
	(v+q-set-return-address address))))

;;;###autoload
(defun v+q-mail-hook ()
  "Customize vm-mail mode for what might be a reply to a mailing list
delivered locally by qmail.  Put this on mail-mode-hook to use it."
  ;; So far, this is just equivalent to v+q-maybe-change-return-address, but I'm
  ;; keeping it around, as it is a logical place to put key bindings (if I ever
  ;; need any).  -- rgr, 2-Apr-00.
  (v+q-maybe-change-return-address))

;;;###autoload
(defun v+q-generate-password (&optional password-length)
  "Generate a password using alphanumerics plus unshifted special characters.
It will be echoed in the message area; see the *Messages* buffer if you
want to cut-and-paste it somewhere.  The password will be completely
obscure, since it is generated randomly.  The default length is 8
characters long, unless you give the command a numeric argument.  If the
argument is negative, it will produce something that doesn't use shift
keys at all."
  ;; part of vm+qmail for no good reason.  -- rgr, 21-Feb-00.
  (interactive "P")
  (let* ((alphabet "abcdefghijklmnopqrstuvwxyz")
	 (chars (concat alphabet
			(if (and (numberp password-length)
				 (< password-length 0))
			    ""
			    (upcase alphabet))
			"0123456789`-=[];',./"))
	 (max (length chars))
	 (password-length (if password-length
			      (abs (prefix-numeric-value password-length))
			      8))
	 (i 0) (result nil))
    (while (< i password-length)
      (setq result (cons (aref chars (random max)) result))
      (setq i (1+ i)))
    (setq result (concat result))
    (message result)
    result))

(provide 'vm+qmail)

