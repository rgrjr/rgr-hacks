;;;****************************************************************************
;;;
;;;    Emacs mouse hackery.
;;;
;;;    The rgr-mouse package defines a number of Symbolics-compatible mouse
;;; commands that work under X11 in either FSF emacs 18 or FSF emacs 19 through
;;; 21.  (I haven't tried it with xemacs, which is probably different.)  Of
;;; greatest interest is rgr-mouse-edit-thing, which finds the file, directory,
;;; or source code (using either ilisp, Franz Inc's eli, or tags) of the thing
;;; clicked on, as appropriate.  It is mostly compatible with Symbolics Genera
;;; #\Meta-Mouse-Left click on random objects.  Other features are described
;;; below.
;;;
;;;    rgr-document-mouse-commands -- Print the list of all currently-bound
;;; mouse commands and their gestures.  (In emacs 19, "drag" means a drag event,
;;; and "down" means a down event; otherwise, it's an up event, even though the
;;; names of the commands bound to these events would seem to conflict.  The
;;; triple of down, drag, and up must always be bound consistently.)
;;;
;;;    rgr-mouse-insert-thing -- Insert the thing under the mouse at point.
;;; Sets the mark to the beginning of the insertion, and leaves point at the
;;; end.  This works between windows/buffers, including the minibuffer, and does
;;; not change the current buffer.
;;;
;;;    rgr-mouse-mark-text-down and rgr-mouse-mark-text-up -- Set mark or copy
;;; text to kill ring.  If clicked in a single spot, sets the mark there.  If
;;; clicked in one spot & dragged to and released at another, copies the
;;; intervening text to the kill ring.  Does not change the current buffer.  For
;;; Symbolics hackers, this is like #\C-Mouse-Left.  [unfortunately, it doesn't
;;; highlight during the drag in emacs 19.  -- rgr, 27-Jan-95.]
;;;
;;;    rgr-install-mouse-commands -- binds these to Symbolics-compatible mouse
;;; clicks.  Note that this shadows mouse-set-secondary (on M-left) and moves
;;; mouse-buffer-menu from C-left to M-right (which in turn shadows
;;; mouse-secondary-save-then-kill).
;;;
;;;*****************************************************************************
;;;
;;;    To compile this without error, do the following:
;;;
;;; (mapcar 'require '(tex-mode browse-url))
;;;
;;; To do:
;;;
;;;    1.  Make rgr-mouse-mark-text-down highlight during the drag.
;;;
;;;    Modification history:
;;;
;;; rgr-mouse-edit-thing, other mouse hackery.  -- rgr, 30-Mar-94.
;;; rgr-mouse-insert-thing, better -edit-thing.  -- rgr, 1-Apr-94.
;;; make rgr-thing-around-point deal with pathnames better.  -- rgr, 4-Apr-94.
;;; rgr-mouse-insert-thing of current line.  -- rgr, 6-Apr-94.
;;; rgr-mail-mode-hook (& rmail), rgr-buffer-menu-exit, split rgr-ed out
;;;	of rgr-mouse-edit-thing .  -- rgr, 7-Apr-94.
;;; rgr-buffer-menu-execute-and-select-current command, rgr-with-lisp-syntax
;;;	and related mouse command fixing.  -- rgr, 12-Apr-94.
;;; rgr-mouse-mark-text-down, fix rgr-move-past-prefix bobp bug.  -- rgr,
;;;	27-Apr-94.
;;; *** emacs 19 update ***
;;;	Split out ./rgr-mouse-18.el and ./rgr-rmail-18.el .  -- rgr, 17-Jan-95.
;;; split out of ./rgr-hacks.el as the rgr-mouse package.  -- rgr, 27-Jan-95.
;;; rgr-find-url: teach rgr-ed to find URL's.  -- rgr, 30-Jan-95.
;;; rgr-thing-around-point: downcase lispm pathname host.  -- rgr, 1-Feb-95.
;;; make rgr-ed push stupid mark for edit-definitions-lisp, rgr-web-client-host
;;;	cleanup to rgr-find-url fn.  -- rgr, 8-Mar-95.
;;; rgr-thing-around-point: ftp:/file: URL's -> ange-ftp.  -- rgr, 22-Mar-95.
;;; rgr-find-url: change client -> netscape, minor tweaks.  -- rgr, 14-Mar-96.
;;; rgr-thing-around-point: fix empty-buffer bug.  -- rgr, 29-Mar-96.
;;; rgr-find-url: support netscape -remote hack.  -- rgr, 10-May-96.
;;; rgr-find-url: browse-url version.  -- rgr, 12-Sep-96.
;;; rgr-browse-url-netscape: maybe run on a remote machine.  -- rgr, 19-Sep-96.
;;; rgr-web-client-name: solaris config kludge.  -- rgr, 14-Mar-97.
;;; rgr-find-url-start-browser: (regexp-quote url) hack.  -- rgr, 14-Apr-97.
;;; rgr-web-client-name: remove solaris config kludge.  -- rgr, 19-Sep-97.
;;; rgr-find-url-start-browser: netscape 4.04 kludges.  -- rgr, 17-Feb-98.
;;; rgr-browse-url-netscape: handle 'su' case.  -- rgr, 25-Mar-98.
;;; rgr-browse-url-netscape: "~/.netscape/lock" test.  -- rgr, 13-Apr-98.
;;; rgr-browse-url-netscape: flush rgr-web-client-processes.  -- rgr, 29-Apr-98.
;;; rgr-browse-url-netscape: "~/.netscape/lock" bashing.  -- rgr, 4-May-98.
;;; rgr-init-browse-url-browser-function: update for 19.34.  -- rgr, 20-Oct-98.
;;; rgr-find-url-start-browser: always nohup on alpha.  -- rgr, 24-Mar-99.
;;; rgr-find-url-start-browser: fix URL quoting for rsh.  -- rgr, 29-Jul-99.
;;; rgr-thing-around-point: do the <url:thing> right.  -- rgr, 3-Jan-00.
;;; rgr-find-url: handle "mailto:" URLs within emacs.  -- rgr, 2-Mar-00.
;;; use eval-after-load to call rgr-init-browse-url-browser-function.  -- rgr,
;;;	7-Apr-00.
;;; fix eval-after-load call.  -- rgr, 11-Apr-00.
;;; rgr-ed: make this handle method specs.  -- rgr, 19-May-00.
;;; use rgr-emacs-flavor to check for lucid.  -- rgr, 26-Jul-01.
;;; rgr-ed: support fi:lisp-find-definition interface.  -- rgr, 26-Feb-02.
;;; rgr-edit-function-spec: make this really work for ilisp.  -- rgr, 30-Jul-02.
;;; rgr-ed: make smarter about lisp names in nonlisp bufs.  -- rgr, 11-Aug-02.
;;; split out lisp stuff to new ilisp-mouse.el file.  -- rgr, 8-Apr-03.
;;;

;;;; Loading required code.

(require 'ilisp-mouse)

;;;; Variables

(defvar rgr-web-client-name "/usr/bin/netscape"
  "*Name of WWW client to use for finding URL's.  Note that if you also use
rgr-web-client-host, then this should be a full pathname.")

(defvar rgr-web-client-host nil
  "*If non-nil, the host on which we should run the client via rsh.")

(defvar rgr-mouse-load-hook nil
  "*Hook called after loading the rgr-mouse package.  Useful for
additional customizations.")

;;;; Utilities

(defun rgr-print-table (table)
  "Formatting hack, for documenting mouse commands."
  (let ((min-widths (make-list (length (car table)) 0))
	(table-tail table))
    (while table-tail
      (let ((line-tail (car table-tail)) (width-tail min-widths))
	(while (and line-tail width-tail)
	  (setcar width-tail (max (car width-tail)
				  (length (car line-tail))))
	  (setq width-tail (cdr width-tail) line-tail (cdr line-tail))))
      (setq table-tail (cdr table-tail)))
    (setq table-tail table)
    (while table-tail
      (let ((line-tail (car table-tail)) (width-tail min-widths))
	(while line-tail
	  (let* ((item (car line-tail)) (min-width (or (car width-tail) 0))
		 (len (length item)))
	    (setq width-tail (cdr width-tail) line-tail (cdr line-tail))
	    (if item
		(princ item))
	    (cond (line-tail
		    (while (< len min-width)
		      (write-char ?\ )
		      (setq len (1+ len)))
		    (princ "  "))))))
      (setq table-tail (cdr table-tail))
      (terpri))))

;;;; Finding and editing things.

;; [We might profitably give this a keyboard interface, too.  -- rgr,
;; 17-Feb-98.]

(defun rgr-shell-quote-argument (argument)
  ;; based on shell-quote-argument, but not as greedy:  quotes only actual shell
  ;; metacharacters that are likely to appear in URLs.
  (let ((result "") (start 0))
    (while (string-match "[*?&<>]" argument start)
      (let ((end (match-beginning 0)))
	(setq result (concat result (substring argument start end)
			     "\\" (substring argument end (1+ end))))
	(setq start (1+ end))))
    (concat result (substring argument start))))

(defun rgr-find-url-start-browser (url)
  (let* ((short-name (file-name-nondirectory rgr-web-client-name))
         (process-buffer-name (concat "*" short-name "*"))
         (process nil)
         (start-message (concat "Starting " short-name
                                (and rgr-web-client-host
                                     (format " on %s" rgr-web-client-host)))))
    (message "%s..." start-message)
    (if rgr-web-client-host
	;; remote startup
        (let ((display (getenv "DISPLAY")))
          (if (= (aref display 0) ?\:)
              ;; braindead initializations
              (setq display (concat (system-name) display)))
	  (let ((commands
		  (concat
		    (format "setenv DISPLAY %s; " display)
		    rgr-web-client-name " "
		    ;; According to the rsh man page,
		    ;;
		    ;;	  Shell metacharacters which are not quoted are
		    ;;	  interpreted on the local machine, while quoted
		    ;;	  metacharacters are interpreted on the remote
		    ;;	  machine.  See EXAMPLES.
		    ;;
		    ;; (This is the current SunOS/Solaris version; the Alphas
		    ;; say much the same.)  We don't want shell metacharacters
		    ;; interpreted at all, especially "?" and "&", which are
		    ;; needed to pass interactive parameters to scripts.  So, we
		    ;; quote doubly.  -- rgr, 29-Jul-99.
		    "'" (rgr-shell-quote-argument url) "'")))
	    ;; [debugging.  -- rgr, 29-Jul-99.]
	    '(message "Command is:  %s" commands)
	    (setq process (start-process short-name process-buffer-name
					 "rsh" rgr-web-client-host commands))))
        ;; Local start.  Inherit DISPLAY and CLASSPATH environment.
        (setq process (start-process short-name process-buffer-name
                                     rgr-web-client-name url)))
    (if (or (not (string-match "netscape\\|communicator" rgr-web-client-name))
	    (string-match "^alpha" system-configuration))
	;; No point in querying, since netscape nohups itself anyway.  [but this
	;; doesn't seem to work on the alpha.  -- rgr, 24-Mar-99.]
	(process-kill-without-query process))
    (message "%s...done." start-message)))

(defun rgr-browse-url-netscape-remote (url &optional new-window-p)
  ;; Fire up a local netscape that will tell the running netscape to find this
  ;; url via the -remote command.  Returns non-nil if successful.  [code taken
  ;; from the browse-url-netscape function.  -- rgr, 25-Mar-98.]
  (let* ((netscape-command (format "openURL(%s%s)"
				   url (if new-window-p ",new-window" "")))
	 (result (apply 'call-process "netscape" nil nil nil
			(append browse-url-netscape-arguments
				(if new-window-p '("-noraise"))
				(list "-remote" netscape-command)))))
    (cond ((stringp result)
	    (error "netscape got signal: %s" result))
	  ((zerop result)
	    t)
	  (t
	    ;; [at this point browse-url-netscape tries to fire up a browser.
	    ;; -- rgr, 25-Mar-98.]
	    nil))))

(defun rgr-browse-url-netscape (url &optional new-window-p)
  ;; We're trying to add a "create browser on remote host if necessary" daemon
  ;; to the browse-url-netscape function.  We do this by encapsulating this
  ;; function (albeit clumsily).  [Now intercepts "mailto:" URLs so that they
  ;; can be handled within emacs.  -- rgr, 9-Apr-03.]
  (cond ((string-match "^mailto:" url)
	  ;; [bug: this doesn't handle e.g. "&subject=auto+reply" parameters,
	  ;; undoing urlencoding.  -- rgr, 8-Apr-03.]
	  (let ((address (substring url (match-end 0))))
	    ;; prefer vm if it's loaded (or autoloadable).
	    (if (fboundp 'vm-mail)
		(vm-mail address)
		(mail t address))))
	((rgr-browse-url-netscape-remote url new-window-p)
	  ;; don't argue with success.
	  )
	((not (equal (user-login-name) (user-real-login-name)))
	  ;; We are su, so we mustn't try to fire up a browser under this UID
	  ;; when there is probably already one running on this console under
	  ;; user-real-login-name anyway.
	  (error "Can't start a browser when 'su %s'."
		 (user-real-login-name)))
	((not (file-symlink-p "~/.netscape/lock"))
	  ;; no running netscape; fire one up (and browse-url-netscape would be
	  ;; redundant).
	  (rgr-find-url-start-browser url))
	((yes-or-no-p (format "No netscape, but lock is %S; bash lock? "
			      (file-symlink-p "~/.netscape/lock")))
	  ;; [accessing the netscape locking mechanism allows multiple emacs
	  ;; instantiations on multiple machines to work, though it is something
	  ;; of a kludge, since it hacks into netscape internals.  the value of
	  ;; file-symlink-p (if not nil) is a string with the format
	  ;; "IPaddress:processID", which in principle we could check to see if
	  ;; it's still running.  -- rgr, 13-Apr-98.]  [but we don't need to,
	  ;; since rgr-browse-url-netscape-remote effectively does that for us.
	  ;; -- rgr, 4-May-98.]
	  (delete-file "~/.netscape/lock")
	  (rgr-find-url-start-browser url))
	(t
	  (message "Aborted."))))

(setq browse-url-browser-function 'rgr-browse-url-netscape)

(defun rgr-find-url (url)
  ;; New version that interfaces to the browse-url package.  [something of a
  ;; legacy function, kept to avoid changing old code.  -- rgr, 9-Apr-03.]
  (require 'browse-url)
  (funcall browse-url-browser-function url))

;;; Load the version-specific commands.  This is why we need to be on the
;;; load-path.
(load (format "rgr-mouse-%s" rgr-emacs-major-version))

(provide 'rgr-mouse)

(run-hooks 'rgr-mouse-load-hook)

