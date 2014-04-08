;;;****************************************************************************
;;;
;;;;    GNU Emacs hackery.
;;;
;;; $Id$
;;;
;;;    [old] Modification history:
;;;
;;; ssh: new.  -- rgr, 13-Dec-99.
;;; ssh: support rgr-ssh-default-destination var.  -- rgr, 31-Jan-00.
;;; ssh: try history, switch instead of pop.  -- rgr, 16-Jun-00.
;;; ssh: build on comint-mode instead of telnet-mode, so that
;;;	comint-watch-for-password-prompt works.  -- rgr, 2-Mar-01.
;;; better ssh-host-history behavior.  -- rgr, 15-Jan-03.
;;; flush rgr-ssh-default-destination var.  -- rgr, 19-Apr-03.
;;;

(require 'comint)

(eval-when-compile
  (require 'shell)
  (require 'sh-script))

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
    (while rgr-tail
      (with-current-buffer (car rgr-tail)
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
Take the current line, plus any previous lines ending in '\\', plus the
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
      (buffer-substring-no-properties start (point)))))

;; Truncating output to the last "uptime" command.

(defun rgr-shell-truncate-output ()
  "Delete from point back to the command after the last \"uptime\" command."
  (interactive)
  (let* ((end (point))
	 (last end)
	 (start nil))
    (while (and (not start)
		(progn (comint-previous-prompt 1)
		       (< (point) last)))
      (cond ((looking-at "uptime")
	      (comint-next-prompt 1)
	      (setq start (point))))
      (setq last (point)))
    (if (not start)
	(error "Couldn't find \"uptime\" command landmark."))
    (delete-region start end)
    (message "Removed %d characters." (1+ (- end start)))))

;; Changing "diff" into "cmp" and vice versa.

(defvar rgr-diff-command-re
        (let* ((lwsp "[ \t]+")
	       (word "[^ \t\n]+")
	       (capture-word (concat "\\(" word "\\)")))
	  (concat "\\(cmp\\|diff\\|cp\\)" lwsp
		  ;; optional options
		  "\\(-" word lwsp "\\)*"
		  ;; file name arguments
		  capture-word lwsp capture-word))
  "Regexp for recognizing diff and cmp commands.  Captures are (1)
command name, (2) options, (3) first file, and (4) second file.")

(defvar rgr-differ-line-re
        (let* ((lwsp "[ \t]+")
	       (word "[^ \t\n]+")
	       (capture-word (concat "\\(" word "\\)")))
	  (concat capture-word lwsp capture-word " differ:"))
  "Regexp for recognizing the 'differ' output of cmp commands.  Captures
are (1) first file, and (2) second file.")

(defun rgr-frob-diff-remake-command (old-cmd file1 file2)
  ;; Given parsed bits of the old command, produce the new command as a string.
  (let* ((new-cmd (if (equal old-cmd "cmp") 'diff 'cmp))
	 (command (if (eq new-cmd 'diff)
		       diff-command
		       "cmp"))
	 (switches (if (eq new-cmd 'diff)
		       diff-switches
		       nil)))
    (mapconcat 'identity
	       (append (list command)
		       (if (listp switches)
			   switches
			   (list switches))
		       (list file1 file2))
	       " ")))

(defun rgr-frob-diff-internal (command-maker)
  ;; Given a command maker, try to find the command on the line, call the maker
  ;; to build a new command, and insert it.
  (let* ((end (save-excursion
		(end-of-line)
		(point)))
	 (command
	   (save-excursion
	     (beginning-of-line)
	     (cond ((re-search-forward rgr-diff-command-re end t)
		     (funcall command-maker
			      (match-string-no-properties 1)
			      (match-string-no-properties 3)
			      (match-string-no-properties 4)))
		   ((re-search-forward rgr-differ-line-re end t)
		     (funcall command-maker
			      "cmp"
			      (match-string-no-properties 1)
			      (match-string-no-properties 2)))
		   (t
		     (error "Not on a diff or cmp command line."))))))
    ;; (message "got command %S" command)
    (goto-char (process-mark
		 (or (get-buffer-process (current-buffer))
		     (error "Current buffer has no process"))))
    (insert command)))

(defun rgr-frob-diff ()
  "Change 'diff' to 'cmp' and vice versa using the current line.
With point on a command line or the 'differ' output of cmp, changes to
the other command, obeying diff-command and diff-switches, and inserts
it as input at the command prompt."
  (interactive)
  (rgr-frob-diff-internal 'rgr-frob-diff-remake-command))

(defun rgr-frob-diff-to-mv (command from to)
  (concat "mv " to " " from))

(defun rgr-diff-to-update ()
  "Change 'diff' or 'cmp' on the current line to an 'mv' that updates.
With point on a command line or the 'differ' output of cmp, changes it
to an 'mv' command that swaps the first and second args, and inserts
it as input at the command prompt."
  (interactive)
  (rgr-frob-diff-internal 'rgr-frob-diff-to-mv))

;;;; SSH support.

(defvar rgr-secure-shell-program "ssh")
(defvar ssh-host-history nil
  "*History of recent SSH hosts.")
(defvar ssh-mode-map (append (make-sparse-keymap) comint-mode-map)
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

(defvar ssh-per-host-option-alist nil
  "Alist of (host-regexp . extra-ssh-options).")

(defun rgr-ssh-start-session-internal (host &optional command-args)
  (require 'comint)	;; [redundant?  -- rgr, 2-Mar-01.]
  (require 'shell)
  (let ((name (concat "ssh-" host)) (other-options nil))
    ;; find any other options we should add.
    (let ((tail ssh-per-host-option-alist))
      (while tail
	(let ((entry (car tail)))
	  (if (string-match (car entry) host)
	      (setq other-options (cdr entry)
		    tail nil)
	      (setq tail (cdr tail))))))
    (switch-to-buffer (apply (function make-comint)
			     name rgr-secure-shell-program nil "-X"
			     (append other-options (list host) command-args)))
    (set-process-filter (get-process name) 'comint-output-filter)
    (ssh-mode)))

;;;###autoload
(defun ssh (host)
  "Open a secure login connection to host named HOST (a string) via ssh.
Communication with HOST is recorded in a buffer `*ssh-HOST*'.
Normally input is edited in Emacs and sent a line at a time."
  (interactive
    (list (read-string "Open ssh connection to host or user@host: "
		       nil 'ssh-host-history)))
  (rgr-ssh-start-session-internal host))

;;;###autoload
(defun ssh-emacs (host)
  "Open a secure login connection to host named HOST (a string) via ssh.
Communication with HOST is recorded in a buffer `*ssh-HOST*'."
  (interactive
    (list (read-string "Open emacs session via ssh on host or user@host: "
		       nil 'ssh-host-history)))
  (rgr-ssh-start-session-internal host '("emacs"))
  ;; this timeout needs to be long enough for a password dialog (which happens
  ;; in the process filter, so it doesn't force the end of the sit-for).
  (sit-for 10)
  (bury-buffer))

;;;; Initialization and hook functions.

;;;###autoload
(defun rgr-comint-mode-hook ()
  "comint-mode is what shell-mode and telnet-mode are built on."
  ;; always look for passwords.  this should be the global default, but it's
  ;; not, and i can't always configure the site properly.  -- rgr, 28-Apr-05.
  (add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)
  ;; as if there aren't enough useless funky special characters to have to
  ;; remember to escape.  [This is not necessary in emacs 18.  -- rgr,
  ;; 21-Dec-94.]
  (setq comint-input-autoexpand nil))

;;;###autoload
(defun rgr-shell-mode-hook ()
  "shell-mode is for running a shell under emacs via M-x shell."
  ;; [introduced in Emacs 22.  -- rgr, 7-Jun-06.]
  (setq comint-scroll-show-maximum-output nil)
  ;; New hack.  -- rgr, 11-Jul-07.
  (define-key shell-mode-map "\C-c*d" 'rgr-frob-diff)
  (define-key shell-mode-map "\C-c*u" 'rgr-diff-to-update))

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
  (define-key sh-mode-map "\r" 'newline-and-indent)
  (define-key sh-mode-map "\M-q" 'rgr-fill-script-comment)
  (define-key sh-mode-map "-" 'rgr-c-electric-dash)
  ;; Reinstall the global M-a and M-e bindings, since moving by commands is not
  ;; enough different from moving by lines to be useful.
  (define-key sh-mode-map "\M-e" 'forward-sentence)
  (define-key sh-mode-map "\M-a" 'backward-sentence))

