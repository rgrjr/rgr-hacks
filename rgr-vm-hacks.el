;;;; vm customizations.
;;;
;;;    These work with vm-6.75 and later.
;;;
;;; $Id$

(eval-when-compile
  (require 'vm)
  (require 'vm-mime)
  (require 'vm-menu)
  (require 'vm-misc)
  (require 'dired)
  (require 'mail-parse))

(require 'vm-mime)	;; [must load first so we can bash.  -- rgr, 24-Sep-99.]

(defun rgr-vm-preprocess-spool-files (entries)
  "Given a list of (list &optional folder inbox crash list-address)
entries, expand it into a list of vm-spool-files entries using standard
defaults."
  (let ((incoming-directory (expand-file-name "incoming/" vm-folder-directory)))
    (mapcar (function (lambda (entry)
	      (let* ((list-name (if (consp entry) (car entry) entry))
		     (tail (if (consp entry) (cdr entry) nil))
		     (folder-name
		       (expand-file-name (or (car tail)
					     (concat list-name ".vm"))
					 vm-folder-directory))
		     (inbox-name
		       (let ((inbox (or (nth 1 tail) list-name)))
			 (if (string-match ":" inbox)
			     ;; pop or imap; leave it alone.
			     inbox
			     (expand-file-name inbox incoming-directory))))
		     (crash-box-name
		       (expand-file-name (or (nth 2 tail)
					     (concat list-name ".crash"))
					 incoming-directory))
		     (list-address (nth 3 tail)))
		(list folder-name inbox-name crash-box-name list-address))))
	    entries)))

;;;###autoload
(defun rgr-vm-mail-yank-hook ()
  ;; Avoid stupid trailing whitespace in yanked messages; this is run by
  ;; vm-yank-message (C-c C-y in a mail or reply buffer).  Because
  ;; vm-included-text-prefix is inserted, instead of using indent-rigidly, blank
  ;; lines are left with trailing whitespace when vm-included-text-prefix
  ;; includes trailing whitespace.  So we provide this alternative to
  ;; vm-mail-yank-default, which ignores vm-included-text-attribution-format,
  ;; and uses indent-rigidly instead of multiple insertions.  -- rgr, 6-Jan-00.
  (save-excursion
    (save-excursion
      (vm-reorder-message-headers nil vm-included-text-headers
				  vm-included-text-discard-header-regexp)
      ;; if all the headers are gone, delete the trailing blank line, too.
      (if (eq (following-char) ?\n)
	  (delete-char 1)))
    (indent-rigidly (point) (point-max) 3)))

(defun rgr-vm-quit-no-save ()
  "Buries the mail and summary buffers, doesn't save."
  (interactive)
  (vm-select-folder-buffer)
  ;; probably not desirable.  -- rgr, 12-Jan-00.
  ;; (save-excursion (run-hooks 'vm-quit-hook))
  (let ((summary-buffer vm-summary-buffer)
	(pres-buffer vm-presentation-buffer-handle)
	(mail-buffer (current-buffer)))
    ;; Get rid of the presentation buffer, but simply bury the summary.
    (when (and summary-buffer
	       (buffer-name summary-buffer))
      (bury-buffer summary-buffer))
    (when pres-buffer
      (vm-display pres-buffer nil nil nil)
      (kill-buffer pres-buffer))
    ;; [oops; we need these to clean up the dangling pointers.  -- rgr,
    ;; 13-Jan-00.]
    (vm-check-for-killed-summary)
    (vm-check-for-killed-presentation)
    (set-buffer mail-buffer)
    (quit-window)
    (vm-display mail-buffer nil nil nil)
    (bury-buffer mail-buffer))
  ;; [not a noop if there are other vm buffers around?  -- rgr, 12-Jan-00.]
  (vm-update-summary-and-mode-line))

(defun rgr-vm-quit ()
  "Saves the buffer, like vm-quit (\\[vm-quit]), but buries instead of killing."
  (interactive)
  (vm-select-folder-buffer)
  (if (and (buffer-modified-p)
	   (or buffer-file-name buffer-offer-save))
      (vm-save-folder))
  (rgr-vm-quit-no-save))

(defun rgr-vm-quit-and-lower-frame ()
  "Saves the mail buffer, like rgr-vm-quit (\\[rgr-vm-quit]), but lowers
the frame after burying the VM buffer."
  (interactive)
  (rgr-vm-quit)
  (lower-frame))

;;; Comparing message bodies.

(defvar rgr-vm-subject-noise-strings
	'("^Re: +" "^\\[[^] \t]\\]:? +" "^ *(fwd) +" " +(fwd) *$")
  "Regular expressions to ignore when comparing subjects.")

(defun rgr-vm-strip-subject-noise-strings (subject)
  (let ((bored-yet nil))
    (while (not bored-yet)
      (let ((new-subject subject)
	    (tail rgr-vm-subject-noise-strings))
	(while tail
	  (cond ((string-match (car tail) new-subject)
		  (setq new-subject (replace-match "" t t new-subject)))
		(t
		  (setq tail (cdr tail)))))
	(setq bored-yet (eq subject new-subject))
	(setq subject new-subject)))
    subject))

;; (rgr-vm-strip-subject-noise-strings "re: foo (Fwd) (Fwd)")

(defun rgr-vm-find-previous-message-with-subject
       (subject &optional no-error-p deleted-ok)
  ;; Look backwards starting from the message before the current message for a
  ;; message with the indicated (stripped) subject, ignoring deleted messages by
  ;; default.
  (let ((mp (vm-reverse-link-of (car vm-message-pointer))))
    (while (and mp
		(not (let* ((m (car mp))
			    (other-subject (vm-subject-of m)))
		       (and (or deleted-ok
				(not (vm-deleted-flag m)))
			    (string-equal (rgr-vm-strip-subject-noise-strings
					    other-subject)
					  subject)))))
      (setq mp (vm-reverse-link-of (car mp))))
    (cond (mp (car mp))
	  (no-error-p nil)
	  (t
	    (error "No previous%s message with subject matching %S."
		   (if deleted-ok "" " undeleted") subject)))))

(defun rgr-vm-prompt-for-message (prompt &optional default)
  ;; Based on code in the vm-yank-message interactive form.
  (let* ((default (cond ((eq default 'none) nil)
			(default (vm-number-of default))
			;; the default default is the current message.
			(vm-message-pointer
			  (vm-number-of (car vm-message-pointer)))))
	 (prompt (if default
		     (format "%s(default %s) " prompt default)
		     prompt))
	 (result 0))
    (while (zerop result)
      (setq result (read-string prompt))
      (if (and (string= result "") default)
	  (setq result default))
      (setq result (string-to-number result)))
    (let ((mp (or (nthcdr (1- result) vm-message-list)
		  (error "No such message."))))
      (car mp))))

(defun rgr-vm-write-message-body-internal (m file)
  (with-current-buffer (vm-buffer-of (setq m (vm-real-message-of m)))
    (vm-save-restriction
      (widen)
      (write-region (vm-text-of m)
		    (vm-text-end-of m)
		    file t 'quiet)
      ;; [don't bother setting this, since we're just going to delete the file
      ;; shortly anyway.  -- rgr, 14-Jan-02.]
      '(if (null (vm-written-flag m))
	  (vm-set-written-flag m t)))))

(defun rgr-vm-compare-message-bodies (previous m)
  "Use diff to compare the bodies of two messages in the current buffer,
prompting for both.  The default is to compare the body of the current
message to that of either the last message with the same subject, if it
exists, or the previous message."
  ;; based on vm-save-message-sans-headers and discus-diff
  (interactive
    (save-excursion
      (vm-select-folder-buffer)
      (let* ((m1 (rgr-vm-prompt-for-message "Compare message: "))
	     (subject1 (rgr-vm-strip-subject-noise-strings (vm-subject-of m1)))
	     (def2 (or (rgr-vm-find-previous-message-with-subject subject1 t)
		       (car (vm-reverse-link-of (car vm-message-pointer)))))
	     (m2 (rgr-vm-prompt-for-message
		  (format "Compare msg %s with message: " (vm-number-of m1))
		  def2)))
	(list m2 m1))))
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (let* ((n-previous (vm-number-of previous))
	 (n-this (vm-number-of m))
	 (tmpdir (or (getenv "TMPDIR") "/tmp/"))
	 (file1 (make-temp-name
		 (expand-file-name (format "msg-%s." n-previous) tmpdir)))
	 (file2 (make-temp-name
		 (expand-file-name (format "msg-%s." n-this) tmpdir))))
    (message "Comparing message %s with message %s" n-previous n-this)
    (unwind-protect
	 (progn
	   (rgr-vm-write-message-body-internal previous file1)
	   (rgr-vm-write-message-body-internal m file2)
	   ;; [it would be clever if we could change the file dates to match the
	   ;; message dates, since "diff -u" puts the dates in the header . . .
	   ;; -- rgr, 19-Jan-02.]
	   (with-output-to-temp-buffer "*diff*"
	     (call-process "diff" nil "*diff*" t "-u" file1 file2)))
      (condition-case () (delete-file file1)
	(file-error nil))
      (condition-case () (delete-file file2)
	(file-error nil)))))

;;; Defining new charset mappings.

(defun rgr-vm-mime-define-charset-coding (charset coding)
  ;; Defines charset (e.g. the "gb2312" in the MIME header "Content-Type:
  ;; text/plain; charset=gb2312") to map to the given emacs internal coding.
  ;; charset is a string, coding is a symbol that names a charset (e.g. on the
  ;; charset-list).
  (let ((entry (list charset coding)))
    (or (member entry vm-mime-mule-charset-to-coding-alist)
	(setq vm-mime-mule-charset-to-coding-alist
	      (cons entry vm-mime-mule-charset-to-coding-alist)))))

;;; Changing Content-Disposition to "inline" on request.

;; vm-mime-display-button-xxxx has an example of code that does this; it creates
;; the "Click mouse-2 to display message" button callback.  Unfortunately, I
;; haven't been able to improve upon that.

(defun rgr-vm-mime-set-content-disposition-inline-internal (button)
  ;; cobbled from vm-mime-display-body-as-text, though we are trying to be
  ;; slightly more permanent.  no luck so far, not even while the buffer remains
  ;; in memory.
  (let ((layout (copy-sequence (vm-extent-property button 'vm-mime-layout))))
    (vm-set-extent-property button 'vm-mime-disposable t)
    (vm-set-extent-property button 'vm-mime-layout layout)
    ;; not universally correct, but close enough.
    (vm-set-mm-layout-type layout '("text/plain" "charset=us-ascii"))
    (vm-set-mm-layout-disposition layout (list "inline"))
    (goto-char (vm-extent-start-position button))
    (vm-decode-mime-layout layout)))

(defun rgr-vm-mime-set-content-disposition-inline ()
  ;; use vm-mime-run-display-function-at-point (which I can't call
  ;; interactively).
  (interactive)
  (vm-mime-run-display-function-at-point
    'rgr-vm-mime-set-content-disposition-inline-internal))

;; [this doesn't work; need to do something to rebuild the menus, i think.  --
;; rgr, 12-Jan-00.]
(defun rgr-vm-install-menus ()
  (let ((tail (or (member ["Display as Text using Default Face"
			   (vm-mime-run-display-function-at-point
			    'vm-mime-display-body-as-text) t]
			  vm-menu-mime-dispose-menu)
		  (last vm-menu-mime-dispose-menu)))
	(new-entry ["Make inline"
		    (rgr-vm-mime-set-content-disposition-inline) t]))
    (or (equal (car (cdr tail)) new-entry)
	(setcdr tail (cons new-entry (cdr tail))))))

(defun vm-mime-reader-save-all-attachments (&optional directory)
  "Offer to save each attachment into the default file in a specified
directory."
  (interactive (list (read-file-name "Directory to save attachments: "
				     nil default-directory)))
  (let ((last-e nil)
	(n-queried 1)
	(n-saved 0)
	(default-directory (or directory default-directory)))
    (while (not (eobp))
      (let ((e (vm-find-layout-extent-at-point)))
	(cond ((and e (not (eq e last-e)))
	       (let* ((layout (vm-extent-property e 'vm-mime-layout))
		      (default-filename
		        (or (vm-mime-get-disposition-parameter layout "filename")
			    (format "attachment-%d" n-queried)))
		      (file (expand-file-name
			      (file-name-nondirectory default-filename))))
		 (cond ((y-or-n-p (format "Save to %s? " file))
			  (vm-mime-send-body-to-file layout nil file)
			  (setq n-saved (1+ n-saved))))
		 (setq last-e e)))))
      (setq n-queried (1+ n-queried))
      (forward-line))
    (message "Done; saved %d message%s."
	     n-saved (if (= n-saved 1) "" "s"))))

;;; Selective expunging.

(defun vm-region-expunge-folder (beg end &optional shaddap)
  "Expunge messages with the `deleted' attribute with headers in the region.
For normal folders this means that the deleted messages are
removed from the message list and the message contents are
removed from the folder buffer.

For virtual folders, messages are removed from the virtual
message list.  If virtual mirroring is in effect for the virtual
folder, the corresponding real messages are also removed from real
message lists and the message contents are removed from real folders."
  (interactive "r")
  (or (eq major-mode 'vm-summary-mode)
      (error "Not in summary."))
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-read-only)
  (let ((mp vm-message-list)
	(messages nil))

    ;; Skip messages before beg.
    (while (and mp
		(<= (vm-su-end-of (car mp)) beg))
      (setq mp (cdr mp)))
    ;; (message "got %d messages after beg" (length mp))

    ;; Collect deleted messages between beg and end.
    (while (and mp
		(< (vm-su-start-of (car mp)) end))
      (if (vm-deleted-flag (car mp))
	  (setq messages (cons (car mp) messages)))
      (setq mp (cdr mp)))

    ;; Operate on them.
    (cond (shaddap
	    (and messages
		 (vm-expunge-folder shaddap t messages))
	    messages)
	  ((null messages)
	    (error "No deleted messages in the region."))
	  ((yes-or-no-p (format "OK to expunge %d message%s? "
				(length messages)
				(if (cdr messages) "s" "")))
	    (vm-expunge-folder nil t messages)))))

;;; Attaching files from dired.

(defun vm-dired-attach-files-to-message (files message-buffer)
  ;; this duplicates much of the vm-mime-attach-file interactive dialog in order
  ;; to ask about MIME type, etc., for each file.
  (with-current-buffer message-buffer
    (let ((tail files))
      (while tail
	(let* ((file (car tail))
	       (abbrev (abbreviate-file-name file))
	       (default-type (or (vm-mime-default-type-from-filename file)
				 "application/octet-stream"))
	       (user-type (completing-read
			    (format "Content type for %s (default %s): "
				    abbrev default-type)
			    vm-mime-type-completion-alist))
	       (type (if (> (length user-type) 0)
			 user-type
			 default-type))
	       (charset nil) (description nil))
	  (if (vm-mime-types-match "text" type)
	      (let ((cs (completing-read
			  (format "Character set for %s (default US-ASCII): "
				  abbrev)
			  abbrev vm-mime-charset-completion-alist)))
		(if (> (length cs) 0)
		    (setq charset cs))))
	  (setq description
		(read-string (format "One line description for %s: " abbrev)))
	  (vm-mime-attach-file file type charset description))
	(setq tail (cdr tail))))))

;;;###autoload
(defun vm-dired-attach-file ()
  "Attach current or all marked files in dired to a vm-mail message."
  (interactive)
  (let* ((files (dired-get-marked-files))
	 (files-description (if (cdr files)
				(format "%d files" (length files))
				(abbreviate-file-name (car files))))
	 (vm-dired-buffers (buffer-list))
	 (vm-dired-mail-buffers nil)
	 (chosen-buffer nil))
    ;; find mail buffers.
    (while vm-dired-buffers
      (with-current-buffer (car vm-dired-buffers)
	(if (eq major-mode 'mail-mode)
	    (setq vm-dired-mail-buffers (cons (car vm-dired-buffers)
					      vm-dired-mail-buffers)))
	(setq vm-dired-buffers (cdr vm-dired-buffers))))
    (setq vm-dired-mail-buffers (nreverse vm-dired-mail-buffers))
    ;; do validation before asking the user anything.
    (if (null vm-dired-mail-buffers)
	(error "No mail buffers; start composing a message first."))
    (if (null vm-send-using-mime)
	(error "MIME attachments disabled in vm, %s"
	       "set vm-send-using-mime non-nil to enable."))
    (let ((tail files))
      (while tail
	(let ((file (car tail)))
	  (if (file-directory-p file)
	      (error "%s is a directory, cannot attach" file))
	  (if (not (file-exists-p file))
	      (error "No such file: %s" file))
	  (if (not (file-readable-p file))
	      (error "You don't have permission to read %s" file)))
	(setq tail (cdr tail))))
    (cond ((null (cdr vm-dired-mail-buffers))
	    (if (yes-or-no-p
		  (format "Attach %s to message %s? "
			  files-description
			  (buffer-name (car vm-dired-mail-buffers))))
		(vm-dired-attach-files-to-message files
						  (car vm-dired-mail-buffers))
		(error "Aborted.")))
	  (t
	    (let* ((default-buffer (car vm-dired-mail-buffers))
		   (chosen-buffer
		     ;; [this sucks; we should select from mail buffers, and use
		     ;; the most recent unsent one as the default.  -- rgr,
		     ;; 13-Sep-04.]
		     (get-buffer
		       (read-buffer
			 (format "Attach %s to message? " files-description)
			 default-buffer t))))
	      (if (not (member chosen-buffer vm-dired-mail-buffers))
		  (error "%S is not a mail buffer." chosen-buffer))
	      (vm-dired-attach-files-to-message files chosen-buffer))))))

;;; vm-mail-mode

;; vm-mail-mode is invoked by "m" in a vm buffer, or M-x vm-mail from anywhere.
;; It calls mail-mode to do most of the setup, so rgr-mail-mode-hook actually
;; does most of the work.  But the strange way it initializes the mode map means
;; we need to resort to this hair in order affect the binding of C-c C-s.  --
;; rgr, 19-Jan-00.

;;;###autoload
(defun rgr-vm-mail-mode-hook ()
  ;; (define-key (lookup-key vm-mail-mode-map "\C-c") "\C-s" nil)
  (define-key vm-mail-mode-map "\C-c\C-s" nil))

;;; Installation hook.

;;;###autoload
(defun rgr-vm-install-hacks ()
  (define-key vm-mode-map "q" 'rgr-vm-quit)
  (define-key vm-mode-map "Q" 'rgr-vm-quit-and-lower-frame)
  (define-key vm-mode-map "x" 'rgr-vm-quit-no-save)
  ;; vm-6.89 binds vm-expunge-folder to "###", which is annoying.  -- rgr,
  ;; 3-Jan-01.
  (define-key vm-mode-map "#" 'vm-expunge-folder)
  ;; Convenient shortcut.  -- rgr, 19-Apr-18.
  (define-key vm-mode-map "\C-c#" 'vm-region-expunge-folder)

  ;; This effectively swaps the notion of "this window" and "other window",
  ;; which seems to be what I need, since I keep scrolling the summary window
  ;; when I mean to scroll the message window.  -- rgr, 27-Nov-00.
  (define-key vm-summary-mode-map [M-next] 'scroll-up)
  (define-key vm-summary-mode-map [M-prior] 'scroll-down)
  (define-key vm-summary-mode-map [next] 'scroll-other-window)
  (define-key vm-summary-mode-map [prior] 'scroll-other-window-down)
  ;; new hack.  -- rgr, 14-Jan-02.
  (define-key vm-summary-mode-map "\C-c=" 'rgr-vm-compare-message-bodies)

  ;; Tune MIME image behavior.  [Except we still get image/jpeg content
  ;; displayed by default.  -- rgr, 9-Aug-08.]
  (setq vm-auto-displayed-mime-content-types
	'("text" "multipart"))
  (setq vm-auto-displayed-mime-content-type-exceptions
	'("text/html" "image/jpeg"))
  (setq vm-mime-use-image-strips nil)
  (cond ((file-executable-p "/shared/emacs/vm-7.19-patched/base64-encode")
	  (setq vm-mime-base64-encoder-program
		"/shared/emacs/vm-7.19-patched/base64-encode")
	  (setq vm-mime-base64-decoder-program
		"/shared/emacs/vm-7.19-patched/base64-decode")))

  ;; Bring URL browsing behavior into the 21st century.
  (setq vm-netscape-program "firefox")

  ;; define charsets.  if these aren't on the list, then i have to click middle
  ;; to seem messages from people whose mailers advertise funny charsets
  ;; (e.g. Hongxian, windows users) even if there are no non-ascii characters in
  ;; the message.  it would be better if vm had as a default rule "use
  ;; 'no-conversion for an unknown charset if the message body contains only
  ;; ascii characters."  -- rgr, 8-Aug-00.
  ;; [not sure if these are still needed in emacs 22/23.  -- rgr, 9-Aug-08.]
  (rgr-vm-mime-define-charset-coding "windows-1252" 'no-conversion)
  (rgr-vm-mime-define-charset-coding "windows-1255" 'no-conversion)
  ;; some mailers seem to generate "x-unknown" for "unknown".  sheesh.
  (rgr-vm-mime-define-charset-coding "x-unknown" 'no-conversion))

(provide 'rgr-vm-hacks)
