;;;****************************************************************************
;;;
;;;    GNU Emacs hackery.
;;;
;;;    To compile this without error, do the following:
;;;
;;;	  (mapcar 'require '(shell sh-script compile telnet))
;;;
;;;    Modification history:
;;;
;;; cdgrep command.  -- rgr, 4-Apr-94.
;;; rgr-shell-mode-hook and rgr-squelch-password hacks.  -- rgr, 23-Feb-95.
;;; added cdcgrep command.  -- rgr, 18-May-95.
;;; split out of ./rgr-hacks.el file.  -- rgr, 26-Mar-96.
;;; rgr-sh-mode-hook: new.  -- rgr, 16-Aug-96.
;;; rgr-recompile: new.  -- rgr, 30-Aug-96.
;;; rgr-recompile: fix "Invoke compile afresh" arg bug.  -- rgr, 14-Nov-96.
;;; rgr-recompile: make "last compile in cd" really work.  -- rgr, 25-Nov-96.
;;; cdgrep & rgr-recompile to ./rgr-compile-hacks.el.  -- rgr, 26-Nov-96.
;;; rgr-sh-mode-hook: use rgr-c-electric-dash fn.  -- rgr, 13-Dec-96.
;;; rgr-find-shell: new hack, handles multiple shells.  -- rgr, 17-Apr-97.
;;; rgr-shell-buffer-major-modes: new, add telnet-mode.  -- rgr, 8-May-97.
;;; new rgr-comint-get-old-input-with-continuation-lines implementation,
;;;	new rgr-shell-insert-previous-input command.  -- rgr, 20-Aug-97.
;;; rgr-telnet-mode-hook, split rgr-shell-mode-hook.  -- rgr, 24-Mar-99.
;;; rgr-find-shell: new numeric arg.  -- rgr, 13-Sep-99.
;;; rgr-shell-mode-hook: orig. 20.3 comint-prompt-regexp.  -- rgr, 9-Dec-99.
;;; ssh: new.  -- rgr, 13-Dec-99.
;;; rgr-shell-set-display: new.  -- rgr, 20-Jan-00.
;;; rgr-shell-set-display: ensure host.  -- rgr, 28-Jan-00.
;;; ssh: support rgr-ssh-default-destination var.  -- rgr, 31-Jan-00.
;;; ssh: try history, switch instead of pop.  -- rgr, 16-Jun-00.
;;; ssh: build on comint-mode instead of telnet-mode, so that
;;;	comint-watch-for-password-prompt works.  -- rgr, 2-Mar-01.
;;; rgr-shell-set-xauthority: new hack.  -- rgr, 5-Nov-02.
;;; better ssh-host-history behavior.  -- rgr, 15-Jan-03.
;;;

(require 'comint)

;;;; Shell hackery

(defvar rgr-shell-buffer-major-modes '(shell-mode telnet-mode ssh-mode)
  "*List of major-mode names for buffers to be found by \\[rgr-find-shell].")

(defun rgr-find-nth-recent-shell-buffer (skip-n)
  ;; The passed index is zero-based, so we call it "skip-n" instead of n.
  ;; Helper function for rgr-find-shell, below.
  (let (;; Eccentric names to avoid set-buffer scope changes.
	(rgr-shell-buffer nil)
	(rgr-skip-n (or skip-n 1))
	;; cdr to omit the current buffer.
	(rgr-tail (buffer-list)))
    (save-excursion
      (while rgr-tail
	(set-buffer (car rgr-tail))
	(cond ((not (memq major-mode rgr-shell-buffer-major-modes))
		(setq rgr-tail (cdr rgr-tail)))
	      ((zerop rgr-skip-n)
		;; found it.
		(setq rgr-shell-buffer (current-buffer)
		      rgr-tail nil))
	      (t
		(setq rgr-skip-n (1- rgr-skip-n))
		(setq rgr-tail (cdr rgr-tail))))))
    rgr-shell-buffer))

;;;###autoload
(defun rgr-find-shell (&optional arg)
  "Like \\[shell], but deals gracefully with multiple shell buffers.  If
given a numeric argument, finds the nth most recent shell or telnet (or
rsh, etc) buffer.  The default is the last visited shell buffer, unless
you're already in it, in which case the default is the previous shell
buffer.  If the chosen shell buffer is already visible in a window, the
window is selected, else the buffer is selected in the current window."
  (interactive "P")
  (let* (;; implement alternative default.
	 (n (cond (arg (prefix-numeric-value arg))
		  ((memq major-mode rgr-shell-buffer-major-modes) 2)
		  (t 1)))
	 (shell-buffer (rgr-find-nth-recent-shell-buffer (1- n))))
    (cond (shell-buffer
	    (let ((window (get-buffer-window shell-buffer)))
	      (if window
		  (select-window window)
		  (switch-to-buffer shell-buffer)))
	    (or (comint-check-proc shell-buffer)
		;; restart the process.  [this is nontrivial, given that the
		;; shell and make-comint functions have hardwired names or
		;; naming conventions.  -- rgr, 17-Apr-97.]
		(error "Process died, must be restarted manually.")))
	  ((null arg)
	    ;; start a new one (but only if not told to search for the nth).
	    (shell)))))

(defun rgr-comint-get-old-input-with-continuation-lines ()
  ;; replacement for comint-get-old-input-default
  "Default for `comint-get-old-input' [modified.  -- rgr, 20-Aug-97].
Take the current line, plus any previous lines ending in '\\'
\(discarding any initial text matching `comint-prompt-regexp'), plus the
next (and subsequent) line if the current (previous) line ends in '\\'."
  (save-excursion
    (let ((original-point (point)) (start nil))
      (beginning-of-line)
      ;; Skip backward while previous lines end in "\".
      (while (and (not (bobp))
		  (save-excursion
		    (forward-char -2)
		    (looking-at "\\\\\n")))
	;; Continuation line -- keep going.
	(forward-line -1))
      (comint-skip-prompt)
      (setq start (point))
      ;; Skip forward while
      (goto-char original-point)
      (end-of-line)
      (while (and (not (eobp))
		  (= (char-after (1- (point))) ?\\))
	;; this is equivalent to (progn (forward-line) (end-of-line)).
	(end-of-line 2))
      (buffer-substring start (point)))))

(defvar rgr-secure-shell-program "ssh")
(defvar ssh-host-history
	(list (if (eq rgr-site 'mgi)
		  "rogers@rgrjr.dyndns.org"
		  "rogers@modulargenetics.dnsalias.com")
	     "rogers@huxley.bu.edu"))
(defvar rgr-ssh-default-destination "rogers@huxley.bu.edu")
(defvar ssh-mode-map (if (eq rgr-emacs-flavor 'xemacs)
			 ;; keymaps are not lists in xemacs.
			 (copy-keymap comint-mode-map)
			 ;; fsf version.
			 (append (make-sparse-keymap) comint-mode-map))
  "Keymap for ssh-mode.  There are no special bindings for ssh-mode
beyond what comint-mode already provides, but we define the keymap
anyway for consistency.")

(defun ssh-mode ()
  "This mode is for using ssh from a buffer to another host.
It is essentially the same as comint-mode.
Data is sent to the remote host when RET is typed.

\\{ssh-mode-map}
"
  (interactive)
  (comint-mode)
  (setq major-mode 'ssh-mode
	mode-name "SSH")
  (use-local-map ssh-mode-map)
  (run-hooks 'ssh-mode-hook))

;;;###autoload
(defun ssh (host)
  "Open a secure login connection to host named HOST (a string) via ssh.
Communication with HOST is recorded in a buffer `*ssh-HOST*'.
Normally input is edited in Emacs and sent a line at a time."
  (interactive
    (list (read-string "Open ssh connection to host or user@host: "
		       nil 'ssh-host-history)))
  (require 'comint)	;; [redundant?  -- rgr, 2-Mar-01.]
  (require 'shell)
  (let ((name (concat "ssh-" host)))
    (switch-to-buffer (make-comint name rgr-secure-shell-program nil "-X" host))
    (set-process-filter (get-process name) 'comint-output-filter)
    (ssh-mode)
    ;; Remember where we went for the next time.
    (setq rgr-ssh-default-destination host)))

(defun rgr-shell-set-display ()
  "Insert a 'setenv' command that sets the DISPLAY variable.
Useful in telnet sessions for propagating $DISPLAY."
  (interactive)
  (let* ((variable-name "DISPLAY")
	 (proc (or (get-buffer-process (current-buffer))
		   (error "Current buffer has no process")))
	 (display (or (getenv variable-name)
		      (error "%S is not defined." variable-name))))
    (if (string-match "^:" display)
	;; braindead initializations
	(setq display (concat (system-name) display)))
    ;; a cleverer version would figure out which shell it had, and pick the
    ;; syntax accordingly.  -- rgr, 20-Jan-00.
    (goto-char (process-mark proc))
    (insert (format "setenv %s %s" variable-name display))))

(defun rgr-shell-define-environment-variable (variable-name value)
  (let ((proc (or (get-buffer-process (current-buffer))
		  (error "Current buffer has no process"))))
    ;; a cleverer version would figure out which shell it had, and pick the
    ;; syntax accordingly.  -- rgr, 20-Jan-00.
    (goto-char (process-mark proc))
    (insert (format (if nil
			"setenv %s %S"
			"export %s=%S")
		    variable-name value))))

(defun rgr-shell-set-xauthority ()
  "Insert an 'export' command that sets the XAUTHORITY variable.
Useful in telnet/ssh sessions for propagating $XAUTHORITY."
  (interactive)
  (let* ((variable-name "XAUTHORITY")
	 (xauthority
	   (or (getenv variable-name)
	       (expand-file-name
		 (concat "~" (getenv "LOGNAME") "/.Xauthority")))))
    (cond ((not (file-readable-p xauthority))
	    (message "Note:  %s is not readable." xauthority)))
    (rgr-shell-define-environment-variable variable-name xauthority)))

;;;###autoload
(defun rgr-comint-mode-hook ()
  "comint-mode is what shell-mode and telnet-mode are built on."
  ;; as if there aren't enough useless funky special characters to have to
  ;; remember to escape.  [This is not necessary in emacs 18.  -- rgr,
  ;; 21-Dec-94.]
  (setq comint-input-autoexpand nil)
  ;; Make comint-based modes smarter about continued command lines.  -- rgr,
  ;; 20-Aug-97.
  (setq comint-get-old-input 'rgr-comint-get-old-input-with-continuation-lines))

;;;###autoload
(defun rgr-shell-mode-hook ()
  "shell-mode is for running a shell under emacs via M-x shell."
  ;; [oops; this is redundant.  -- rgr, 20-Jan-00.]
  ;; (define-key shell-mode-map "\M-\r" 'rgr-shell-insert-previous-input)
  (define-key shell-mode-map "\C-cx" 'rgr-shell-set-display)
  (define-key shell-mode-map "\C-cy" 'rgr-shell-set-xauthority)
  ;; Set this to the same thing ange-ftp-gateway-prompt-pattern will use (after
  ;; default.el gets loaded).  [And allow csh "? " prompts (e.g. "foreach? ").
  ;; -- rgr, 20-Aug-97.]  [the telnet-mode runs comint-mode-hook before
  ;; text-mode-hook, but bashes this in the interval, so we can't share this
  ;; with rgr-telnet-mode-hook by moving it to rgr-comint-mode-hook, alas.  --
  ;; rgr, 24-Mar-99.]  [this causes problems with the default csh prompt under
  ;; linux, so fix it back (modulo adding "?" as above) so we don't have to
  ;; change the root prompt.  -- rgr, 9-Dec-99.]
  ;; (setq comint-prompt-regexp "^[a-z]*[#$%>?] +")
  (setq comint-prompt-regexp "^[^#$%>?\n]*[#$%>?] *"))

;;;###autoload
(defun rgr-telnet-mode-hook ()
  "telnet-mode is for running a shell under emacs via M-x rsh or M-x telnet."
  (define-key telnet-mode-map "\C-cx" 'rgr-shell-set-display)
  (define-key shell-mode-map "\C-cy" 'rgr-shell-set-xauthority)
  ;; [oops; this is redundant.  -- rgr, 20-Jan-00.]
  ;; (define-key telnet-mode-map "\M-\r" 'rgr-shell-insert-previous-input)
  ;; See the comment in rgr-shell-mode-hook, above.  -- rgr, 24-Mar-99.
  (setq comint-prompt-regexp "^[a-z]*[#$%>?] +"))

;;;###autoload
(defun rgr-add-to-shell-modification-history ()
  "Add to a modification history near the top of the file.
Sets the mark before moving there, and starts a new line before the end
of the comment.  If no history exists (which it determines by searching
for the string in the rgr-modification-history-herald variable), then
you are asked about starting one.  (If you are asked this when there
already is one, then somebody probably inserted extra crud at the
beginning of the file.)"
  (interactive)
  (rgr-add-to-modification-history-internal "#    " "^# *" "#" "^#* *$"))

;;;###autoload
(defun rgr-fill-script-comment (arg)
  "Fill the shell script comment around point.
Just does the regular M-q (fill-paragraph) if it can't find a comment."
  (interactive "P")
  (or (rgr-fill-prefix-comment "#")
      (fill-paragraph arg)))

;;;###autoload
(defun rgr-sh-mode-hook ()
  "sh-mode (aka shell-script-mode) is for editing shell scripts."
  (define-key sh-mode-map "\M-q" 'rgr-fill-script-comment)
  (define-key sh-mode-map "\M-*" 'rgr-add-to-shell-modification-history)
  (define-key sh-mode-map "-" 'rgr-c-electric-dash)
  ;; Reinstall the global M-a and M-e bindings, since moving by commands is not
  ;; enough different from moving by lines to be useful.
  (define-key sh-mode-map "\M-e" 'forward-sentence)
  (define-key sh-mode-map "\M-a" 'backward-sentence))

