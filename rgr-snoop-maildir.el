;;;*****************************************************************************
;;;
;;;;   rgr-snoop-maildir:  A mode to preview maildir content.
;;;
;;; [Created.  -- rgr, 27-Dec-16.]
;;;

(defvar snoop-maildir-spam-folder nil
  "Spam folder, the destination of messages with the 's' mark.")
(defvar snoop-maildir-move-folder (expand-file-name "~/Maildir/new/")
  "'Move' folder, the destination of messages with the 'm' mark.")

(defvar snoop-maildir-message-line "^\\[\\(.\\)\\] +\\([^ \t\n]*\\)")

(defun snoop-maildir-next-msg (&optional n unmarked-only-p)
  "Move forward by N messages, or backward if N is negative."
  (interactive "P")
  (or n (setq n 1))
  (cond ((= n 0))
	((>= n 0)
	  (and (looking-at snoop-maildir-message-line)
	       (goto-char (match-end 0)))
	  (while (and (> n 0)
		      (re-search-forward snoop-maildir-message-line nil t))
	    (when (or (not unmarked-only-p)
		      (= (char-after (match-beginning 1)) ? ))
	      (setq n (1- n)))))
	(t
	  (when (and (not (bobp))
		     (looking-at snoop-maildir-message-line))
	    (forward-char -1))
	  (while (and (< n 0)
		      (re-search-backward snoop-maildir-message-line nil t))
	    (when (or (not unmarked-only-p)
		      (= (char-after (match-beginning 1)) ? ))
	      (setq n (1+ n))))))
  (and (match-beginning 0)
       (goto-char (match-beginning 0)))
  n)

(defun snoop-maildir-prev-msg (&optional n)
  "Move backward by N messages, or forward if N is negative."
  (interactive "P")
  (or n (setq n 1))
  (snoop-maildir-next-msg (- n)))

(defun snoop-maildir-get-msg ()
  "Extract the message file name from the current line."
  (save-excursion
    (beginning-of-line)
    (and (looking-at snoop-maildir-message-line)
	 (let ((file-name (match-string 2)))
	   (and (length file-name)
		(file-exists-p file-name)
		file-name)))))

(defun snoop-maildir-find-file ()
  "Open the current message in a new buffer, as by find-file."
  (interactive)
  (let ((msg (snoop-maildir-get-msg)))
    (and msg (switch-to-buffer (find-file-noselect msg)))))

(defun snoop-maildir-set-state (&optional new-state n)
  ;; Move backwards or forwards (according to the sign of n, setting the state
  ;; field in the "[ ]" to the new-state character value.
  (or new-state (setq new-state ?\ ))
  (or n (setq n 1))

  (let ((sign (if (< n 0) -1 1)))
    ;; If going forward, mark the first message (on the current line).
    (unless (= n 0)
      (when (> n 0)
	(let ((buffer-read-only nil))
	  (save-excursion
	    (or (snoop-maildir-get-msg)
		(error "No message [1]."))
	    (replace-match (string new-state) t t nil 1)))
	(setq n (- n sign)))
      ;; Move to the next message after marking it, or (if going backwards) the
      ;; previous message before marking it.
      (unless (zerop (snoop-maildir-next-msg sign))
	(setq n 0)))
    ;; Mark subsequent messages.
    (while (not (zerop n))
      (let ((buffer-read-only nil))
	(save-excursion
	  (or (snoop-maildir-get-msg)
	      (error "No message."))
	  (replace-match (string new-state) t t nil 1)))
      (setq n (- n sign))
      (when (or (> sign 0) (not (= n 0)))
	(unless (zerop (snoop-maildir-next-msg sign))
	  (setq n 0))))))

(defun snoop-maildir-set-state-matching (&optional new-state)
  ;; Set the state field of the current message in the "[ ]" to the new-state
  ;; character value, then set all subsequent messages with the same sender and
  ;; initial state to new-state.
  (or new-state (setq new-state ?\ ))

  (let ((sign 1)
	(old-state ?\ )
	(count 0)
	(sender nil) (old-point (point)))
    ;; Mark the message on the current line.
    (let ((buffer-read-only nil))
      (save-excursion
	(or (snoop-maildir-get-msg)
	    (error "No message [1]."))
	(setq old-state (aref (match-string 2) 0))
	(replace-match (string new-state) t t nil 1)
	(goto-char (match-end 2))
	(skip-chars-forward " \t")
	(let ((start (point)))
	  (end-of-line)
	  (setq sender (buffer-substring-no-properties start (point))))))
    ;; Move to the next message.
    (snoop-maildir-next-msg sign)
    (setq count (1+ count))
    ;; Mark subsequent messages.
    (while (search-forward sender nil t)
      (beginning-of-line)
      (let ((buffer-read-only nil))
	(save-excursion
	  (or (snoop-maildir-get-msg)
	      (error "No message [2]."))
	  (and (= (aref (match-string 2) 0) old-state)
	       (replace-match (string new-state) t t nil 1))))
      (end-of-line)
      (setq count (1+ count)))
    (beginning-of-line)
    (unless (equal old-point (point))
      ;; If we haven't moved, then there were no other matches; otherwise, move
      ;; to the next unmarked message after the start.
      (goto-char old-point)
      (snoop-maildir-next-msg sign))
    (message "Marked %d message%s" count (if (= count 1) "" "s"))))

(defun snoop-maildir-set-del (&optional n)
  (interactive "p")
  (snoop-maildir-set-state ?d n))

(defun snoop-maildir-set-cur (&optional n)
  (interactive "p")
  (snoop-maildir-set-state ?c n))

(defun snoop-maildir-set-cur-matching (&optional n)
  (interactive "p")
  (snoop-maildir-set-state-matching ?c))

(defun snoop-maildir-set-move (&optional n)
  (interactive "p")
  (snoop-maildir-set-state ?m n))

(defun snoop-maildir-set-spam (&optional n)
  (interactive "p")
  (snoop-maildir-set-state ?s n))

(defun snoop-maildir-set-spam-matching (&optional n)
  (interactive "p")
  (snoop-maildir-set-state-matching ?s))

(defun snoop-maildir-forward-unset (&optional n)
  (interactive "p")
  (snoop-maildir-set-state ?\  n))

(defun snoop-maildir-backward-unset (&optional n)
  (interactive "p")
  (snoop-maildir-set-state ?\  (- n)))

;;; Mode definition.

(defvar snoop-maildir-truncate-lines t
  "*If value is true, truncate lines in the snoop-maildir-mode
buffer to the current window size.")

(defun snoop-maildir-refresh-buffer ()
  ;; Refresh in the current buffer.
  (setq buffer-read-only nil)
  (erase-buffer)
  (setq truncate-lines snoop-maildir-truncate-lines) ;; buffer-local.
  (insert
    (shell-command-to-string
      "perl /home/rogers/projects/system/scripts/email/snoop-maildir.pl ."))
  (goto-char (point-min))
  (message "%d messages." (count-lines (point-min) (point-max)))
  (setq buffer-read-only t))

(defun snoop-maildir-revert-buffer ()
  "Reread buffer contents from the Maildir."
  ;; bug:  this ought to preserve markings.
  (interactive)
  (snoop-maildir-refresh-buffer))

(defun snoop-maildir-capture-state ()
  (let ((action-alist nil) (msg nil))
    (save-excursion
      (goto-char (point-min))
      (unless (looking-at snoop-maildir-message-line)
	(when (re-search-forward snoop-maildir-message-line)
	  (goto-char (match-beginning 0))
	  (snoop-maildir-get-msg)))
      (while (setq msg (snoop-maildir-get-msg))
	(let* ((action (aref (match-string 1) 0))
	       (entry (assoc action action-alist)))
	  (unless (eq action ?\ )
	    (unless entry
	      (setq entry (list action))
	      (push entry action-alist))
	    ;; (message "have msg %S action %S" msg action)
	    (push msg (cdr entry))))
	;; (message "search: before next")
	(unless (zerop (snoop-maildir-next-msg))
	  (forward-line))))
    action-alist))

(defun snoop-maildir--move-to-folder (messages folder folder-name)
  (cond ((null folder)
	  (format "no %s folder" folder-name))
	((not (file-directory-p folder))
	  (format "folder %s does not exist" folder-name))
	((not (file-readable-p folder))
	  (format "folder %s does not exist" folder-name))
	(t
	  (dolist (message messages)
	    (rename-file message (concat folder message)))
	  (format "%d messages moved to %s" (length messages) folder-name))))

(defun snoop-maildir-execute ()
  (interactive)
  (let ((actions (or (snoop-maildir-capture-state)
		     (error "Nothing marked.")))
	(current-msg (snoop-maildir-get-msg))
	(summary-messages nil))
    ;; (message "Have actions %S" actions)
    (dolist (pair actions)
      (let* ((action-letter (car pair))
	     (messages (cdr pair))
	     (n-messages (length messages)))
	(cond ((eq action-letter ?c)
	        (push (snoop-maildir--move-to-folder messages "../cur/" "cur")
		      summary-messages))
	      ((eq action-letter ?s)
	        (push (snoop-maildir--move-to-folder
		        messages snoop-maildir-spam-folder "spam")
		      summary-messages))
	      ((eq action-letter ?d)
		(dolist (message messages)
		  (delete-file message))
	        (push (format "%d messages deleted" n-messages)
		      summary-messages))
	      ((eq action-letter ?m)
		 (push (snoop-maildir--move-to-folder
			 messages snoop-maildir-move-folder "main")
		       summary-messages))
	      (t
		(push (format "unknown action letter '%c'" action-letter)
		      summary-messages)))))
    (let* ((cap-first (let ((first (car summary-messages)))
			(concat (capitalize (substring first 0 1))
				(substring first 1))))
	   (message (cond ((null (cdr summary-messages))
			    cap-first)
			  ((null (cddr summary-messages))
			    (concat cap-first " and "
				    (car (cdr summary-messages))))
			  (t
			    (let* ((tail (reverse (cdr summary-messages)))
				   (butlast (nreverse (cdr tail))))
			      (concat (mapconcat #'identity
						 (cons cap-first butlast)
						 ", ")
				      ", and " (car tail)))))))
      (snoop-maildir-revert-buffer)
      (message "%s." message))
    ;; Restore the cursor if possible.
    (and (search-forward current-msg nil t)
	 (beginning-of-line))))

;;;###autoload
(defun snoop-maildir (dir-name)
  "Enter snoop-maildir-mode in the specified directory."
  (interactive (list (read-directory-name "Snoop maildir (directory): "
					  nil default-directory nil)))
  (switch-to-buffer (get-buffer-create (concat "*snoop-" dir-name "*")))
  (setq default-directory dir-name)
  (snoop-maildir-refresh-buffer)
  (snoop-maildir-mode))

;; (setq debug-on-error t)
;; (makunbound 'snoop-maildir-mode-map)
(easy-mmode-defmap snoop-maildir-mode-map
  '(("q" . quit-window)
    ("K" . kill-this-buffer)
    ("f" . snoop-maildir-find-file)
    ("g" . snoop-maildir-revert-buffer)
    (" " . snoop-maildir-forward-unset)
    ("\" . snoop-maildir-backward-unset)
    ("c" . snoop-maildir-set-cur)
    ("C" . snoop-maildir-set-cur-matching)
    ("d" . snoop-maildir-set-del)
    ("m" . snoop-maildir-set-move)
    ("s" . snoop-maildir-set-spam)
    ("S" . snoop-maildir-set-spam-matching)
    ("n" . snoop-maildir-next-msg)
    ("p" . snoop-maildir-prev-msg)
    ("x" . snoop-maildir-execute))
  "Snoop-Maildir's keymap."
  :group 'snoop-maildir)

(define-derived-mode snoop-maildir-mode special-mode "Snoop-Maildir"
  "A major mode for previewing maildir content."
  :syntax-table nil
  :abbrev-table nil)
