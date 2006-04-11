;;;****************************************************************************
;;;
;;;    GNU Emacs hackery.
;;;
;;;    To compile this (almost) without error, do the following:
;;;
;;;	(mapcar 'require '(rgr-mouse t-mouse))
;;;
;;; (But colon-double-space is a free reference in 19.27 and prior.)  [and must
;;; do (require 't-mouse) under linux.  -- rgr, 9-Dec-99.]
;;;
;;;    [old] Modification history:
;;;
;;; started history.  -- rgr, 2-Mar-94.
;;; . . .
;;; rgr-subordinate-emacs-p: use EMACS instead of TERM.  -- rgr, 1-May-00.
;;; rgr-subordinate-emacs-p: oops; be cleverer about ssh.  -- rgr, 4-Jan-01.
;;; rgr-parent-dir-and-file-name: remove unix dependence.  -- rgr, 11-Jan-01.
;;; rgr-subordinate-emacs-p: make user-settable.  -- rgr, 6-Apr-01.
;;; rgr-x11-display-host: new var, make rgr-install-window-system-hacks shorten
;;;	sigler window, related hackery.  -- rgr, 10-Apr-01.
;;; rgr-canonicalize-x11-display-and-find-display-host: oops; forgot to add the
;;;	":" back on.  -- rgr, 18-Apr-01.
;;; oops; was OK as is, but needed save-match-data.  -- rgr, 20-Apr-01.
;;; update for XEmacs 21.1 (on Tru64).  -- rgr, 25-Jul-01.
;;; change default site, move rgr-install-frame-properties here, add new
;;;	rgr-emacs-flavor var, other support for xemacs.  -- rgr, 26-Jul-01.
;;; rgr-install-frame-properties: use "emacs", not "fsf".  -- rgr, 30-Jul-01.
;;; rgr-install-function-keys: rgr-toggle-frame-height cmd.  -- rgr, 20-Aug-01.
;;; rgr-install-function-keys: insert-symbol-abbr on f7.  -- rgr, 28-Mar-02.
;;; rgr-canonicalize-x11-display-and-find-display-host: don't canonicalize
;;;	"localhost".  -- rgr, 19-Oct-02.
;;; rgr-reset-buffer-backed-up: new.  -- rgr, 20-Dec-02.
;;; rgr-install-function-keys: lose rgr-toggle-frame-height.  -- rgr, 7-Apr-03.
;;; rgr-next-possibility -> ilisp-next-possibility rename.  -- rgr, 19-Apr-03.
;;;
;;; $Id$

;;;; Variables.

;; First, functions used to initialize variables.

(defun rgr-host-domain-suffix (host)
  (if (string-match "\\." host)
      (substring host (match-beginning 0))
      ""))

(defun rgr-canonicalize-host-name (host)
  ;; Return the fully-qualified DNS name for the given host (if at all
  ;; possible).  host may be missing (null) or the empty string, in which case
  ;; we use the local host name.  We assume that unqualified names are in the
  ;; local domain.
  (cond ((or (null host) (equal host "") (equal host "localhost"))
	  (system-name))
	((not (string-match "\\." host))
	  ;; make fully qualified.
	  (concat host (rgr-host-domain-suffix (system-name))))
	(t host)))

(defvar rgr-hacks-dotted-quad-regexp
	(let (;; If I don't do something this explicit, I tend to get hits to
	      ;; message ID's generated by the vm mailer.
	      (octet "[0-2]?[0-9][0-9]?") (dot "\\."))
	  (concat octet dot octet dot octet dot octet)))

(defun rgr-canonicalize-x11-display-and-find-display-host ()
  ;; Fix broken values of DISPLAY, and attempt to find the name of the host on
  ;; which the emacs window actually appears.  This doesn't work for cascaded
  ;; tunneled ssh connections.
  (let ((display (getenv "DISPLAY")))
    (and display
	 ;; First, canonicalize the DISPLAY environment variable.
	 (let* ((original-host (and (string-match ":" display)
				    (substring display 0 (match-beginning 0))))
		(canonical-host
		  (save-match-data
		    (rgr-canonicalize-host-name original-host)))
		(display-host (if (equal original-host "localhost")
				  ;; don't use the official host name, because
				  ;; that won't work for tunneled OpenSSH 3.4
				  ;; connections, which are alway bound to the
				  ;; loopback address.  -- rgr, 19-Oct-02.
				  original-host
				  ;; otherwise, use the canonical name.
				  canonical-host))
		(etc (substring display (match-beginning 0)))
		(client (getenv "SSH_CLIENT")))
	   ;; Re-export $DISPLAY, but not if "su", since that may interfere with
	   ;; xauth permissions.  -- rgr, 25-Sep-03.
	   (if (equal (user-real-login-name) (user-login-name))
	       (setenv "DISPLAY" (concat display-host etc)))
	   (or (and client
		    ;; [this seems to have gone away with OpenSSH 3.4, if not
		    ;; earlier.  -- rgr, 12-May-03.]
		    ;; (getenv "REMOTEHOST")
		    (rgr-canonicalize-host-name
		      (or (getenv "REMOTEHOST")
			  (and (string-match rgr-hacks-dotted-quad-regexp
					     client)
			       (match-string 0 client)))))
	       canonical-host)))))

(defvar rgr-site 'home
  "Site location, for dealing with configuration differences.")

(defvar rgr-x11-display-host
	(and (eq window-system 'x)
	     (rgr-canonicalize-x11-display-and-find-display-host))
  "Fully-qualified domain name of the host on which the emacs display
window appears, or nil if we're not running X.  Note that the part of
the DISPLAY variable before the colon may not be the same, and may not
even be a prefix.  [It may not even be correct if multiple layers of rsh
or ssh X11 forwarding is in use; we can only peer through the first
layer.  That actually works best for rgr-subordinate-emacs-p purposes.
-- rgr, 18-Apr-01.]")

(defvar rgr-subordinate-emacs-p
	(and (equal (getenv "TERM") "emacs")
	     ;; Require that the connection be local, i.e. rule out ssh
	     ;; connections across domain boundaries (we don't know how to
	     ;; recognize telnet connections, but we shouldn't be doing these
	     ;; offsite anyway).  [We assume these define the scope of a "local"
	     ;; connection, but this is not generally true at BU.  -- rgr,
	     ;; 4-Jan-01.]
	     (let ((local-host (system-name))
		   (remote-host
		     ;; REMOTEHOST is defined only for ssh, so
		     ;; rgr-canonicalize-host-name will return the local host
		     ;; name if not connected via ssh, so this is not strictly
		     ;; the "remote host."  But it works.  [Except not for the
		     ;; OpenSSH 3.4p1 version shipped with SuSE 8.1, it appears.
		     ;; -- rgr, 14-Jul-03.]
		     (rgr-canonicalize-host-name (getenv "REMOTEHOST"))))
	       (or (equal local-host remote-host)
		   (equal (rgr-host-domain-suffix local-host)
			  (rgr-host-domain-suffix remote-host))))
	     ;; Require that we not be "su" as somebody else.
	     (equal (user-real-login-name) (user-login-name)))
  "*Non-nil if this emacs should be considered 'subordinate,' which
disables some things such as reading mail into INBOX.  By default, it is
set only if this emacs was provably invoked from within another for the
same user at the same site (i.e. we are not su or logged in remotely via
ssh).  This typically happens when using M-x rsh to fire up an emacs on
another machine.")

;;; emacs versions.

(defvar rgr-emacs-version-info
  	(let ((version (emacs-version)))
	  (cond ((string-match "^\\(.*\\)Emacs \\([0-9]+\\)\\.\\([0-9]+\\)"
			       version)
		  (list (let ((flavor (substring version (match-beginning 1)
						 (match-end 1))))
			  (cond ((string-equal flavor "X") 'xemacs)
				((string-equal flavor "GNU ") 'fsf)
				(t 'unknown)))
			(string-to-int (substring version (match-beginning 2)
						  (match-end 2)))
			(string-to-int (substring version (match-beginning 3)
						  (match-end 3)))))
		((string-match "^Lucid" version)
		  ;; this is historic, probably broken.
		  '(lucid 19 0))
		(t '(unknown 0 0))))
  "A list of (flavor major minor).")

(defvar rgr-emacs-flavor (car rgr-emacs-version-info)
  "Type of emacs (fsf, xemacs, or lucid).")
(defvar rgr-emacs-major-version (car (cdr rgr-emacs-version-info))
  "Emacs version (18, 19, 20, or 21), for dealing with configuration
differences.  Much code will fail to work on 18, though, so it's not
really supported.")
(defvar rgr-emacs-minor-version (car (cdr (cdr rgr-emacs-version-info)))
  "Emacs minor version (the second number), for dealing with configuration
differences.")

(defvar rgr-space-means-execute-and-exit nil
  "*Set this to get Symbolics-compatible behavior in buffer menu.")

;;;; Support hacks.

;;;###autoload
(defmacro rgr-emacs-major-version-case (&rest case-clauses)
  "Evaluate different case-like clauses depending on the emacs version.
For a list of legal major version values defined at present, see the
rgr-emacs-major-version variable; use a list of one or more of these to
run a clause in more than one version.  Use the symbols t or otherwise
to get a default clause."
  (cons 'cond
	(mapcar (function (lambda (clause)
		  (cons (let ((version (car clause)))
			  (if (memq version '(t otherwise))
			      t
			      (list (if (listp version) 'memq 'eq)
				    'rgr-emacs-major-version
				    (list 'quote version))))
			(cdr clause))))
		case-clauses)))

;;;###autoload
(defun rgr-emacs-version-p (major &optional minor)
  "Return t iff we are running in a version at least as advanced as the
one specified by MAJOR and MINOR.  MINOR defaults to 0 (i.e. any
incarnation of the major version)."
  (or (> rgr-emacs-major-version major)
      (and (= rgr-emacs-major-version major)
	   (>= rgr-emacs-minor-version (or minor 0)))))

;;;; Loading required code.

;; find-tag-tag is used by the rgr-show-elisp-arglist function.  [I'd like to
;; condition (require 'tags) on whether we actually need it, but it uses
;; find-tag-tag in an interactive form.  -- rgr, 12-Apr-94.]
(cond ((eq rgr-emacs-flavor 'lucid)
        ;; In lemacs, the "tags" feature is provided by the "etags"
        ;; file.  Stupid.
        (or (memq 'tags features)
	    (load "etags")))
      ;; End [arguable] of braindeath.  -- rgr, 26-Oct-94.
      ((eq rgr-emacs-major-version 18) (require 'tags))
      (t (require 'etags)))

;;;; General.

(defun rgr-hacks-getf (plist property-name &optional default)
  ;; [cheap getf, so we don't need the cl package.  -- rgr, 23-Apr-03.]
  (let ((tail plist) (result default))
    (while tail
      (if (eq (car tail) property-name)
	  (setq result (car (cdr tail))
		tail nil)
	  (setq tail (cdr (cdr tail)))))
    result))

(defun rgr-fix-manpath (new)
  "Fix MANPATH to include new, which may have been missed by a
$PATH-oriented \"man\" implementation."
  (let ((path (getenv "MANPATH")))
    (if (not path)
	(save-excursion
	  (condition-case err
	      (progn
		(set-buffer (get-buffer-create " *manpath-temp*"))
		(call-process "manpath" nil t nil)
		(goto-char (point-min))
		(end-of-line)
		(setq path (buffer-substring (point-min) (point))))
	    (file-error nil))))
    (if (and path
	     (file-directory-p new)
	     (file-readable-p new)
	     (not (string-match new path)))
	(setenv "MANPATH" (concat new ":" path)))))

;; Compatibility with earlier emacs versions.  -- rgr, 22-Aug-99.
(or (fboundp 'buffer-substring-no-properties)
    (fset 'buffer-substring-no-properties 'buffer-substring))

;;;; Renaming buffers.

(defun rgr-parent-dir-and-file-name (file-name)
  (let* ((dir-name (file-name-directory file-name))
	 (parent-dir (file-name-nondirectory (directory-file-name dir-name)))
	 (file-name (file-name-nondirectory file-name)))
    (concat parent-dir "/" file-name)))

;;;###autoload
(defun rgr-rename-buffer (&optional new-name unique-p)
  "Change the current buffer's name to NEW-NAME (a string).
This is just like \\[rename-buffer] except that the default comes from the
file name and the name of its immediately containing directory."
  (interactive
    (list (read-string "New buffer name: "
		       (if buffer-file-name
			   (rgr-parent-dir-and-file-name buffer-file-name)
			   ;; can't do any better.
			   (buffer-name)))))
  (rename-buffer (or new-name
		     (if buffer-file-name
			 (rgr-parent-dir-and-file-name buffer-file-name)
			 (error "Can't default buffer name without file name")))
		 unique-p))

;;;###autoload
(defun rgr-maybe-rename-buffer ()
  ;; This is useful as a hook function, e.g.
  ;;	(add-hook 'find-file-hooks 'rgr-maybe-rename-buffer)
  (if (and (string-match "<[0-9]+>$" (buffer-name))
	   buffer-file-name)
      (rename-buffer (rgr-parent-dir-and-file-name buffer-file-name) t)))

;;;###autoload
(defun rgr-reset-buffer-backed-up (&optional query-p)
  "Unconditionally resets the buffer-backed-up flag in the current buffer.
If this flag is not set, then a message to that effect is printed.
A numeric argument queries (but only if already backed up)."
  (interactive "P")
  (cond ((not buffer-backed-up)
	  (or (eq query-p 'all)
	      (message "The %S flag in %s is already cleared."
		       'buffer-backed-up (buffer-name)))
	  nil)
	((or (not query-p)
	     (y-or-n-p (format "Clear %S flag in buffer %s? "
			       'buffer-backed-up (buffer-name))))
	  (setq buffer-backed-up nil)
	  (message "Cleared %S flag in %s." 'buffer-backed-up (buffer-name))
	  t)
	(query-p
	  (message "Not cleared.")
	  nil)))

;;;###autoload
(defun rgr-maybe-reset-buffer-backed-up-everywhere ()
  "Offers to reset the buffer-backed-up flag in all buffers (for which a
backup file has already been made)."
  (interactive)
  (save-excursion
    (let ((buffer-tail (buffer-list)))
      (while buffer-tail
	(set-buffer (car buffer-tail))
	(rgr-reset-buffer-backed-up 'all)
	(setq buffer-tail (cdr buffer-tail))))
    (message "Done.")))

;;; auto-fill

(defun rgr-change-log-indent-relative-maybe ()
  "Keep indenting past a '+' bullet on the previous line."
  (indent-relative-maybe)
  (let ((start-column (current-column)))
    (if (save-excursion
	  (forward-line -1)
	  (move-to-column start-column)
	  ;; It doesn't count if start-column is inside a tab on this line.
	  ;; (message "[point is %s]" (point))
	  (and (= (current-column) start-column)
	       (eq (char-after) ?+)))
	(indent-relative))))

(defun rgr-do-auto-fill ()
  ;; Just do auto fill, without adaptive-fill-mode stuff.  [it seems that, in
  ;; emacs 20.x, I should just set normal-auto-fill-function to this, and flush
  ;; the rgr-auto-fill-mode interface.  -- rgr, 25-Nov-99.]
  ;; [rgr-auto-fill-mode is now history.  -- rgr, 1-Nov-04.]
  (let ((adaptive-fill-mode nil)
	;; According to the indent-to-left-margin source doc (the elisp comment
	;; before the definition), it should be the default for text mode, but
	;; somebody seems to have bashed it to indent-relative-maybe instead.
	;; This has the effect of completely sabotaging the rebinding of
	;; adaptive-fill-mode to nil above.  -- rgr, 25-Nov-99.  [but this is
	;; almost what we want in change-log-mode.  -- rgr, 20-Sep-04.]
	(indent-line-function
	  (if (eq major-mode 'change-log-mode)
	      'rgr-change-log-indent-relative-maybe
	      'indent-to-left-margin)))
    (do-auto-fill)))

;;;###autoload
(defun rgr-text-mode-hook ()
  ;; turning auto-fill-mode on unconditionally may not be the right thing in
  ;; certain text modes.  try it and see.  [Seems to work pretty well, but the
  ;; emacs 19.30 auto fill (& subsequent versions) are too smart for their own
  ;; good.  -- rgr, 6-Jun-96.]
  (setq normal-auto-fill-function 'rgr-do-auto-fill)
  (auto-fill-mode 1)
  (setq fill-column 72)
  ;; the standard text paragraph definition is a pain.  set it to be the same as
  ;; in lisp mode: an empty line or newpage.  -- rgr, 14-Feb-94.
  (setq paragraph-start "^$\\|^"))

(defun rgr-date-string ()
  ;; Return a concise date string, of the form "9-Mar-96".
  (let ((time-string (current-time-string)))
    (concat (if (eq (aref time-string 8) ?\ )
		(substring time-string 9 10)
	        (substring time-string 8 10))
	    "-"
	    (substring time-string 4 7)
	    "-"
	    (substring time-string 22 24))))

(defvar signature-login-name nil
  "*Name to use in rgr-insert-signature, if other than user-login-name.")

;;;###autoload
(defun rgr-insert-signature ()
  "Insert '  -- username, dd-mmm-yy.' at point."
  ;; It looks sorta like this.  -- rgr, 2-Feb-94.  Or like this, if you don't
  ;; use the signature-login-name feature to override the default name.  --
  ;; rogers, 14-Feb-94.
  (interactive)
  (insert (format "  -- %s, %s."
		  (or signature-login-name (user-login-name))
		  (rgr-date-string))))

(defvar rgr-modification-history-herald "Modification history:"
  "*Variable used by \\[rgr-add-to-lisp-modification-history] and \\[rgr-add-to-c-modification-history] to find the history.  This is used
both as a quoted search string and inserted as a herald.  See the
rgr-add-to-lisp-modification-history command for details.")

;;;###autoload
(defun rgr-add-to-modification-history-internal
       (header-start header-start-re header-comment header-comment-re
		     &optional fspec)
  ;; Guts of the rgr-add-to-foo-modification-history commands, for language
  ;; customization.  -- rgr, 13-Aug-96.
  (let ((header-regexp (concat header-start-re
			       (regexp-quote rgr-modification-history-herald))))
    (push-mark)
    (goto-char (point-min))
    (forward-paragraph)
    (cond ((re-search-backward header-regexp nil t)
	    (forward-line 2))
	  ((re-search-forward header-regexp
			      (save-excursion (forward-sexp 1) (point))
			      t)
	    (forward-line 2))
	  ((not (yes-or-no-p "Buffer has no modification history; make one? "))
	    (error "Aborted."))
	  (t
	    (insert header-start rgr-modification-history-herald "\n"
		    header-comment "\n")
	    (if (eq major-mode 'c-mode)
		 ;; [***kludge for C.  -- rgr, 12-Dec-96.]
		 (insert header-comment " */\n")
		 ;; normal case.
		 (insert header-comment "\n"))
	    (forward-line -1)))
    (while (not (or (eobp) ;; just in case
		    (looking-at header-comment-re)))
      (forward-line))
    (insert header-comment " ")
    (if fspec
	(insert fspec ": "))
    (insert "\n")
    (forward-char -1)))

;;; A backward-up-list that does strings.

(defun rgr-in-string-p ()
  "Return the string delimiter character if (point) is in a string, else nil."
  ;; Arcane usage of the parse-partial-sexp function.
  (save-excursion
    (let ((state (let ((end (point)))
		   (beginning-of-defun)
		   (parse-partial-sexp (point) end))))
      (nth 3 state))))

;;;###autoload
(defun rgr-backward-up-list (&optional arg)
  "Move backward out of one level of parentheses, or out of a string.
With argument, do this that many times.
A negative argument means move forward but still to a less deep spot."
  (interactive "p")
  (or arg (setq arg 1))
  (let ((quote (rgr-in-string-p)))
    (if quote
	(let ((chars (concat (list ?^ quote))))
	  ;; could call this backward-up-string . . . the heuristic is defeated
	  ;; by '". . .\\"', though.  (but that may be because emacs is, too.)
	  (skip-chars-backward chars)
	  (backward-char)
	  (while (and (not (bobp))
		      (= (char-after (1- (point))) ?\\))
	    (skip-chars-backward chars)
	    (backward-char))
	  (cond ((< arg 0)
		  (forward-sexp)
		  (setq arg (1+ arg)))
		((> arg 0)
		  (setq arg (1- arg)))))))
  (if (not (zerop arg))
      (backward-up-list arg)))

;;;; Edit-buffers hacks.

(defun rgr-buffer-menu-exit ()
  "Execute lines marked for save & deletion and exit from the buffer menu.
This does what Zmacs List Buffers mode SPC does, except that it doesn't
ask for confirmation before killing buffers."
  (interactive)
  (let ((buffer (current-buffer)))
    (Buffer-menu-execute)
    (Buffer-menu-this-window)
    (bury-buffer buffer)))

(defun rgr-buffer-menu-execute-and-select-current ()
  "Execute marked lines and exit, selecting the old current buffer."
  (interactive)
  (let ((old-point (point)))
    (goto-char (point-min))
    (or (re-search-forward "^\\." nil t)
	(goto-char old-point))
    (rgr-buffer-menu-exit)))

(defun rgr-buffer-menu-view-other ()
  "View the buffer on the current line in the other window.
Stays in buffer-menu mode."
  (interactive)
  (let ((original-window (selected-window)))
    (Buffer-menu-other-window)
    (select-window original-window)))

;;;###autoload
(defun rgr-list-buffers (arg)
  "Display a list of names of existing buffers.
Inserts it in buffer *Buffer List* and displays that.
Note that buffers with names starting with spaces are omitted.
Non-null optional arg FILES-ONLY means mention only file buffers.

The M column contains a * for buffers that are modified.
The R column contains a % for buffers that are read-only."
  ;; this is the same as the "stock" list-buffers, except that it puts the
  ;; buffer-list buffer in the current window, leaving point on the line with
  ;; the current buffer, and doesn't frob the window state otherwise.  for
  ;; reasons obscure to me, list-buffers is a subr.
  (interactive "P")
  (switch-to-buffer (list-buffers-noselect arg))
  ;; Current buffer may not be in the list if you gave a numeric arg.
  (or (re-search-forward "^\\." nil t)
      (forward-line 2))
  (beginning-of-line))

;;;; Mark ring (aka "point pdl") hackery.

;;;###autoload
(defun rgr-set-mark-command (arg)
  "Set mark at where point is, or jump to mark.
With no prefix argument, set mark, and push previous mark on mark ring.
With argument, jump to mark, and pop into mark off the mark ring.
With c-u c-u, pops mark off the mark ring without moving point (Lispm style)."
  (interactive "P")
  (cond ((null arg)
	  (push-mark))
	((null (mark))
	  (error "No mark set in this buffer"))
	(t
	  ;; Conditionalizing the goto-char is the only difference
	  ;; between this and the set-mark-command definition.
	  (if (not (equal arg '(16)))
	      (goto-char (mark)))
	  (pop-mark))))

;;;###autoload
(defun rgr-exchange-point-and-mark (n)
  "Put the mark where point is now, and point where the mark is now.
With a numeric argument, treats the mark ring as a stack (with point and
mark implicitly consed onto it) and rotates the top N 1-based elements.
The default argument is equivalent to 2 (just exchange point and mark)."
  (interactive "P")
  (cond ((null n) (setq n 2))	;; alternative default
	(t (setq n (prefix-numeric-value n))))
  (cond ((< n 0)
	  (error "Negative arg %s to %s doesn't work."
		 n 'rgr-exchange-point-and-mark))
	((< n 2))
	((= n 2)
	  ;; Same as default.
	  (exchange-point-and-mark))
	(t
	  ;; General case: destructively remove elt (- n 3) out of mark-ring
	  ;; (remember that n is 1-based), push the current mark and set it at
	  ;; point, & go to the extracted mark.
	  (let* ((elt (- n 3))
		 (new-point
		   (if (= elt 0)
		       (and mark-ring
			    (prog1 (car mark-ring)
			      (setq mark-ring (cdr mark-ring))))
		       (let ((tail-1 (nthcdr (1- elt) mark-ring)))
			 (and (cdr tail-1)
			      (prog1 (car (cdr tail-1))
				(setcdr tail-1 (cdr (cdr tail-1)))))))))
	    (if (null new-point)
		(error "Stack is only %d deep." (+ 2 (length mark-ring))))
	    ;; This moves the old mark onto the ring & sets it at point.
	    (push-mark nil t)
	    (goto-char new-point)
	    nil))))

;;;; Installing these goodies.

;;;###autoload
(defun rgr-install-buffer-hacks ()
  "Install a Lisp-Machine-like edit-buffers mode on C-x C-b (see also
M-x buffer-menu)."
  ;; Intended for .emacs calling.  [but let's also make this a command, so they
  ;; can be installed as an afterthought.  -- rgr, 3-Oct-99.]
  (interactive)
  (if (not (eq rgr-emacs-major-version 18))
      (global-set-key "\C-\M-l" 'rgr-switch-to-other-buffer))
  (global-set-key "\C-x\C-b" 'rgr-list-buffers)
  (if rgr-space-means-execute-and-exit
      ;; was next-line.  -- rgr, 21-Mar-94.
      (define-key Buffer-menu-mode-map " " 'rgr-buffer-menu-exit))
  ;; view in other window, staying in buffer menu.
  (define-key Buffer-menu-mode-map "O" 'rgr-buffer-menu-view-other)
  ;; execute and exit, going back to the former current buffer.
  (define-key Buffer-menu-mode-map "."
    'rgr-buffer-menu-execute-and-select-current))

;; (local-key-binding "\C-\M-u")
;; (global-key-binding [S-delete])

;;;###autoload
(defun rgr-install-global-editing-hacks ()
  ;; Intended for .emacs calling.
  (global-set-key "\C-cgl" 'goto-line)
  (global-set-key "\C-cgc" 'goto-char)
  (global-set-key "\C-cgs" 'rgr-view-sequence-at-point)
  ;; I keep typing "insert" by accident, and then overwrite stuff by accident.
  ;; Unbinding this key means that I have to type "M-x overwrite-mode RET" if I
  ;; really want to clobber myself.  -- rgr, 6-Feb-06.
  (global-set-key [insert] nil)
  ;; Patch fill to be smarter about sentences.  -- rgr, 11-Feb-96.  [This has
  ;; been implemented as part of version 19.30, though I'm still gonna need
  ;; fill-patch functionality for HTML fill.  -- rgr, 21-Mar-96.]
  (cond ((rgr-emacs-version-p 19 30)
	  (setq colon-double-space t))
	((rgr-emacs-version-p 19)
	  (require 'fill-patch)
	  ;; Then use said feature to make "The items:  Item number 1.  Item
	  ;; number 2."  appear as three sentences.  (Note the added ":" in the
	  ;; first char set.)
	  (setq sentence-end "[:.?!][]\"')}]*\\($\\| $\\|\t\\|  \\)[ \t\n]*")))
  ;; For when C-n moves off the screen.  In my usual configuration, the
  ;; split-screen windows are 30 lines tall, and the default amount to move
  ;; seems to be more than half a screen.  -- rgr, 1-Mar-94.
  (setq scroll-step 8)
  ;; Use ".~#~" versions on edited files.  -- rgr, 8-Apr-94.
  (setq version-control t) ;; was nil
  (setq dired-kept-versions 3) ;; was 2
  (setq delete-old-versions t) ;; was nil
  (setq kept-old-versions 0) ;; was 2
  (setq kept-new-versions 3) ;; was 2
  ;; Emacs 18 compatibility.  [But this is probably not possible anyway if
  ;; running emacs 20.  -- rgr, 3-Oct-99.]
  (cond ((not (rgr-emacs-version-p 20))
	  (setq byte-compile-compatibility t)
	  ;; More emacs-19.30 compatibility.  -- rgr, 25-Mar-96.
	  (setq byte-compile-dynamic-docstrings nil)))
  ;; New rgr-fasta-goto-base command.
  (global-set-key "\C-cgb" 'rgr-fasta-goto-base)
  ;; Re-enable random commands.
  (put 'narrow-to-region 'disabled nil)
  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil))

;;;###autoload
(defun rgr-install-global-hacks ()
  ;; Called by rgr-install-hacks to install commands that are truly global, for
  ;; both window systems and terminal emulators.
  (rgr-install-ffap)
  (rgr-install-miscellaneous-global-hacks))

(defun rgr-install-miscellaneous-global-hacks ()
  (global-set-key "\C-cs" 'rgr-insert-signature)
  (global-set-key "\C-x\C-x" 'rgr-exchange-point-and-mark)
  (cond ((not (eq rgr-emacs-flavor 'lucid))
	  ;; The Lucid emacs version of this has hairy zmacs-activate-region
	  ;; behavior.  -- rgr, 26-Oct-94.
	  (global-set-key "\C-@" 'rgr-set-mark-command)))
  ;; Make these the default everywhere, in case I get used to typing them.
  (global-set-key "\C-c." 'ilisp-next-possibility)
  (global-set-key "\C-c;" 'rgr-comment-region-lisp)
  ;; [actually, I'm now used to C-x C-x . . .  -- rgr, 15-Mar-97.]  [and we
  ;; need to put rgr-set-mark-command there.  -- rgr, 8-Dec-99.]
  ;; (global-set-key "\C-c " 'rgr-exchange-point-and-mark)
  (global-set-key "\C-c\e " 'rgr-exchange-point-and-mark)
  ;; Work around braindeath in QVT/Term and ssh -- you can't tell them to send
  ;; C-@ for C-SPC.  Rebind these everywhere, in case I get used to them.  [this
  ;; makes rgr-vt220-term-setup redundant now.  -- rgr, 8-Dec-99.]
  (global-set-key "\C-c " 'rgr-set-mark-command)
  ;; [also use C-x SPC, since C-c SPC is shadowed in html-helper-mode.  -- rgr,
  ;; 26-Nov-99.]  [and in emacs 20 comint mode.  -- rgr, 8-Dec-99.]
  (global-set-key "\C-x " 'rgr-set-mark-command)
  ;; The standard version doesn't deal with quoted strings . . .
  (global-set-key "\C-\M-u" 'rgr-backward-up-list)
  ;; Give *helpful* unique buffer names to scores of makefile files.  -- rgr,
  ;; 17-Dec-97.
  (add-hook 'find-file-hooks 'rgr-maybe-rename-buffer)
  ;; And make buffers go away on command.  -- rgr, 6-Feb-98.
  (global-set-key "\C-cb" 'bury-buffer))

(defun rgr-no-root-email ()
  (interactive)
  (error "No email for root."))

;;;###autoload
(defun rgr-install-function-keys ()
  ;; These function keys should also be supported by the NCSA/BYU Telnet vt200
  ;; hack, but for some reason it sends something different (see the
  ;; rgr-term-setup function).  -- rgr, 4-Apr-96.  [now fixed.  this used to be
  ;; part of rgr-install-x11-hacks, but now installed generally.  -- rgr,
  ;; 6-Feb-98.]
  (cond ((zerop (user-uid))
	  ;; root doesn't get mail anyway, no sense in loading all of vm just to
	  ;; find that out.
	  (global-set-key [f1] 'rgr-no-root-email)
	  (global-set-key [kp-f1] 'rgr-no-root-email)
	  (global-set-key [f2] 'rgr-no-root-email))
	(t
	  ;; normal user.
	  (global-set-key [f1] 'rgr-invoke-rmail)
	  (global-set-key [kp-f1] 'rgr-invoke-rmail)
	  (global-set-key [f2] 'v+q-mbox-status)))
  (global-set-key [f4] 'rgr-insert-symbol-abbreviation)
  ;; [ssh on Windows sends kp-f1 through kp-f4 for f1 through f4.  f5 sends
  ;; nothing, f10 is intercepted, and the others seem to be normal.  -- rgr,
  ;; 8-Dec-99.]
  (global-set-key [kp-f4] 'rgr-insert-symbol-abbreviation)
  ;; [this is easier to type on my new kinesis keyboard.  -- rgr, 28-Mar-02.]
  (global-set-key [f7] 'rgr-insert-symbol-abbreviation)
  ;; [this isn't really equivalent to the LispM version, and gets a funny error
  ;; message if M-p isn't bound.  -- rgr, 19-Aug-01.]
  '(global-set-key "\C-\M-y" "\M-p")
  ;; Put repeat-complex-command (C-x ESC ESC) on f6.
  (global-set-key [f6] 'repeat-complex-command)
  (global-set-key [kp-f6] 'repeat-complex-command)
  ;; [used to use F5 to select the default other-buffer in the other window,
  ;; creating one if in one-window configuration, and leave the cursor in the
  ;; current window.  now an alternate rubout that doesn't tweak my pinkie.  --
  ;; rgr, 15-Mar-98.]
  ;; (global-set-key [f5] "\C-x4b\n\C-xo")
  ;; [gee, I had forgotten all about these.  don't use them any more, and they
  ;; mess up xemacs.  -- rgr, 25-Jul-01.]
  '(cond ((not (eq rgr-emacs-flavor 'xemacs))
	  (global-set-key [f5] [delete])
	  (global-set-key [C-f5] [C-delete])
	  (global-set-key [M-f5] [M-delete])
	  (global-set-key [C-M-f5] [C-M-delete])))
  ;; [now a frame size toggler.  -- rgr, 20-Aug-01.]
  ;; [no longer needed, and a pain in the butt to mistype.  -- rgr, 7-Apr-03.]
  ;; (global-set-key [f5] 'rgr-toggle-frame-height)
  ;; rgr-find-shell is a new hack.  -- rgr, 17-Apr-97.
  (global-set-key [f8] 'rgr-find-shell)
  (global-set-key [f9] 'rgr-recompile))

(defun rgr-dns-lookup-ip (ip-address)
  ;; this is here for no good reason.
  (with-temp-buffer
    (call-process "nslookup" nil t nil "-q=ptr" ip-address)
    (goto-char (point-min))
    (and (re-search-forward "[ \t]name = \\([^ \t\n]+\\)$" nil t)
	 (downcase (match-string 1)))))

;; (setq debug-on-error t)

;;;###autoload
(defun rgr-install-window-system-hacks ()
  ;; Window system hacks (only defined for X11 at present).  -- rgr, 29-Mar-96.
  ;; Shorten the frame on sigler, which has a small screen.  -- rgr, 7-Mar-01.
  (if (equal rgr-x11-display-host "sigler.bu.edu")
      (modify-frame-parameters (selected-frame) '((height . 52))))
  ;; Commands.
  (cond ((not (eq rgr-emacs-flavor 'fsf))
	  (global-set-key [(control space)] 'rgr-set-mark-command)
	  (global-set-key [(control meta space)]
			  'rgr-exchange-point-and-mark)
	  ;; [xemacs 21 binds M-% to query-replace already.  -- rgr, 26-Jul-01.]
	  (global-set-key [(control ?%)] 'replace-string)
	  (global-set-key [(control meta ?%)] 'query-replace)
	  ;; [already bound somewhere else?  -- rgr, 26-Jul-01.]
	  '(global-set-key [(control space)] 'rgr-set-mark-command))
	((rgr-emacs-version-p 19 30)
	  ;; [emacs 20 binds these already?  -- rgr, 3-Oct-99.]
	  (global-set-key [?\C-%] 'replace-string)
	  (global-set-key [?\C-\M-%] 'query-replace)
	  (global-set-key [?\C- ] 'rgr-set-mark-command)
	  ;; Define C-M-Space.  See rgr-define-lisp-mode-commands, which has to
	  ;; un-shadow this key.
	  (global-set-key [?\M-\C- ] 'rgr-exchange-point-and-mark)))
  (cond ((eq rgr-emacs-flavor 'xemacs)
	  ;; [kludge: doesn't work yet.  -- rgr, 26-Jul-01.]
	  )
	((eq window-system 'x)
	  ;; [idiosyncratic naming.  -- rgr, 4-Apr-96.]
	  (rgr-install-x11-hacks))
	(t
	  (let ((setup-function (intern (concat "rgr-install-"
						(symbol-name window-system)
						"-hacks"))))
	    (cond ((fboundp setup-function)
		    (funcall setup-function))
		  (t
		    (message "No %s function defined for window system %s"
			     setup-function window-system)
		    (sit-for 2)))))))

;;;###autoload
(defun rgr-install-hacks ()
  "Install global hacks and do window or terminal inits.  This is only
useful if you want everything."
  (rgr-install-global-hacks)
  (rgr-install-global-editing-hacks)
  (rgr-install-buffer-hacks)
  (rgr-install-function-keys)
  (cond (window-system
	  (rgr-install-window-system-hacks))
	((and (string-match "linux" system-configuration)
	      (condition-case error
		  (require 't-mouse)
		(error nil))
	      (t-mouse-tty))
	  ;; Running on a Linux virtual terminal without X11, but the t-mouse
	  ;; package is available; t-mouse will give us a passable imitation of
	  ;; mouse behavior.  -- rgr, 27-Nov-99.
	  (t-mouse-run)
	  ;; But it's still a terminal.
	  (rgr-term-setup))
	(t
	  ;; Must be a terminal.
	  (rgr-term-setup))))

;; Just to be anal.

(provide 'rgr-hacks)
