;;;; Hacks for CVS.
;;;
;;; [created.  -- rgr, 4-Dec-03.]
;;;
;;; Put
;;;
;;;	(define-key text-mode-map "\C-c+" 'rgr-cvs-plus)
;;;	(add-hook 'log-edit-mode-hook 'rgr-cvs-log-edit-hook)
;;;
;;; somewhere in your .emacs to use this.
;;;
;;; $Id$

(require 'vc)

(defun rgr-find-more-recent-buffer (&rest buffers)
  ;; buffers is a list of buffers, some of which may be nil.  if there is more
  ;; than one non-nil buffer, pick the one that the user visited more recently.
  ;; the order of buffers in the list is not significant.
  (let ((best nil) (tail buffers))
    (while tail
      (let ((buffer (car tail)))
	(cond ((null buffer))
	      ((null best)
	        (setq best buffer))
	      ((member best (member buffer (buffer-list)))
	        (setq best buffer)))
	(setq tail (cdr tail))))
    best))

;;;###autoload
(defun rgr-cvs-recent-changes (&optional number-of-days)
  "Show a summary of 'cvs log' output for the last three days.  If you
give a C-u, it shows the last week's worth; if C-u C-u, then the last
month (30 days, actually).  Any other numeric argument shows the log for
that many days."
  (interactive "P")
  (require 'time-date)		;; part of gnus
  (let* ((number-of-days
	   (cond ((integerp number-of-days) number-of-days)
		 ((not number-of-days) 3)
		 ((equal number-of-days '(4))
		   ;; control-U
		   7)
		 ((equal number-of-days '(16))
		   ;; control-U control-U
		   30)
		 ((consp number-of-days) (car number-of-days))
		 (t (error "got number-of-days %S" number-of-days))))
	 (n-days-ago (subtract-time (current-time)
				    (seconds-to-time
				      (* number-of-days 24 60 60))))
	 (n-days-ago-string
	   (format-time-string "%Y-%m-%d %H:%M" n-days-ago)))
    ;; (error "Date '%s'." n-days-ago-string)
    (shell-command (format "cvs -q log -d '>%s' | cvs-chrono-log.pl"
			   n-days-ago-string))))

;;;###autoload
(defun rgr-vc-project-diff ()
  "Diff for the 'project' rooted at the current directory non-interactively.
This would be just a shorthand for the vc-diff command (\\[vc-diff])
when asked to compare a working directory to the original CVS version
\(e.g. 'C-u \\[vc-diff] \".\" RET RET RET'), but it also renames the
output buffer from '*vc-diff*' to '*vc-project-diff*'.  This is so that
the \\[rgr-cvs-insert-log-skeleton] command can use this output."
  (interactive)
  ;; need an explicit require, because vc-version-diff is not autoloaded.
  (require 'vc)
  (vc-version-diff (expand-file-name ".") nil nil)
  (save-excursion
    (set-buffer "*vc-diff*")
    (let ((old-buf (get-buffer "*vc-project-diff*")))
      (and old-buf
	   (kill-buffer old-buf)))
    (rename-buffer "*vc-project-diff*")))

;;;###autoload
(defun rgr-cvs-insert-log-skeleton ()
  "Insert a '* filename:' line for each 'Index: filename' line in diff output.
This goes through a diff buffer and inserts one line for each file at
the end of the current buffer, which is assumed to be something like a
CVS log comment.  The diff buffer is the most recently visited one of
'*vc-project-diff*' (created by the \\[rgr-vc-project-diff] command),
'*VC-diff*' (created by the \\[vc-diff] command), or '*Shell Command
Output*' (assumed to be the output of 'cvs diff'.)  Files that are being
added or deleted are noted as such."
  (interactive)
  (let ((original-buffer (current-buffer))
	(other-buffer
	  (or (rgr-find-more-recent-buffer
		(get-buffer "*vc-project-diff*")
		(get-buffer "*Shell Command Output*")
		(get-buffer "*VC-diff*"))
	      (error "Can't find %S, %S, or %S buffer."
		     "*VC-diff*" "*vc-project-diff*" "*Shell Command Output*")))
	(match-re (concat "^Index: \\([^ \t\n]*\\)$"
			  "\\|^cvs \\(diff\\|server\\): \\([^ \t\n]*\\) "
			  "\\(is a new entry\\|was removed\\)")))
    (save-excursion
      (set-buffer other-buffer)
      (goto-char (point-min))
      (while (re-search-forward match-re nil t)
	(let* ((file-name (or (match-string 1) (match-string 3)))
	       (extra (match-string 4))
	       (insertion
		 (concat "* " file-name
			 (cond ((equal extra "was removed") " (deleted)")
			       (extra " (added)")
			       (t ""))
			 ":")))
	  ;; (message "found %S" file-name)
	  (save-excursion
	    (set-buffer original-buffer)
	    (goto-char (point-max))
	    (if (not (re-search-backward
		       (concat "^" (regexp-quote insertion) "$")
		       nil t))
		(insert insertion "\n"))))))
    ;; move point to start adding comments.
    (or (eobp)
	(forward-line 1))))

(defun rgr-mode-definition-name ()
  ;; Total kludge.
  (cond ((member major-mode '(emacs-lisp-mode lisp-mode))
	  (save-excursion
	    (if (not (looking-at "^("))
		(beginning-of-defun))
	    (rgr-lisp-def-name t)))
	((eq major-mode 'perl-mode)
	  (rgr-perl-definition-name))
	((eq major-mode 'pir-mode)
	  (save-excursion
	    (and (re-search-backward "^\\.sub[ \t]+\\([^ \t\n]+\\)" nil t)
		 (match-string 1))))
	(t
	  (message "Can't find definitions for %S mode." major-mode)
	  (sit-for 2)
	  nil)))

(defun rgr-vc-current-comment-file ()
  ;; Get the name of the current file comment, or nil if before it
  (save-excursion
    (or (bobp)
	(forward-char -1))
    (and (re-search-backward "^\\* +" nil t)
	 (let ((start (match-end 0))
	       (end (progn (goto-char (match-end 0))
			   (skip-chars-forward "^,: \t\n")
			   (point))))
	   (expand-file-name (buffer-substring start end))))))

(defun rgr-vc-find-file-comment (file-name)
  ;; Get the name of the current file comment, or nil if before it
  (let ((result nil))
    (save-excursion
      (goto-char (point-min))
      (while (and (not result)
		  (re-search-forward "^\\* +" nil t))
	(let ((star (match-beginning 0))
	      (start (match-end 0))
	      (end (progn (goto-char (match-end 0))
			  (skip-chars-forward "^,: \t\n")
			  (point))))
	  (if (equal (expand-file-name (buffer-substring start end)) file-name)
	      (setq result star))))
      result)))

;;;###autoload
(defun rgr-diff-add-definition-comment (&optional other-file)
  "Find the definition name of the corresponding source line.
`diff-jump-to-old-file' (or its opposite if the OTHER-FILE prefix arg
is given) determines whether to jump to the old or the new file.
If the prefix arg is bigger than 8 (for example with \\[universal-argument] \\[universal-argument])
  then `diff-jump-to-old-file' is also set, for the next invocations."
  (interactive "P")
  (let* ((rev (not (save-excursion (beginning-of-line) (looking-at "[-<]"))))
	 ;; this is a list of (buf line-offset pos src dst &optional switched).
	 (loc (diff-find-source-location other-file rev))
	 (buf (car loc))
	 (pos (nth 2 loc))
	 (src (nth 3 loc))
	 (name (save-excursion
		 (set-buffer buf)
		 (goto-char (+ pos (cdr src)))
		 (rgr-mode-definition-name))))
    (switch-to-buffer-other-window
      (or (rgr-find-more-recent-buffer (get-buffer "*VC-log*")
				       (get-file-buffer "comment.text"))
	  (find-file-noselect "comment.text")))
    (let ((current-comment-file (rgr-vc-current-comment-file))
	  (changed-file (buffer-file-name buf))
	  (comment-start nil))
      (cond ((equal changed-file current-comment-file)
	      ;; assume we're in the right place.
	      )
	    ((setq comment-start (rgr-vc-find-file-comment changed-file))
	      '(error "Changed file %S but buffer %S."
		     changed-file current-comment-file)
	      (goto-char comment-start)
	      (forward-line))
	    (t
	      ;; assume we should add a new entry here.
	      (beginning-of-line))))
    (rgr-cvs-plus)
    (if name
	(insert "(" name "):  "))))

;;;###autoload
(defun rgr-cvs-plus ()
  "Insert a '  + ' at point, starting a new line if not at BOL."
  (interactive)
  (cond ((and (eolp) (bolp))
	  ;; empty line, no adjustment needed.
	  )
	((and (bolp) (looking-at "^[ \t]*[*+]"))
	  ;; beginning of non-empty line with stuff already on it; move it to
	  ;; the next line.
	  (insert "\n")
	  (forward-char -1))
	((not (bolp))
	  ;; in the middle or end of a non-empty line
	  (insert "\n")))
  (insert "   + "))

(defun rgr-cvs-join-consecutive-file-headings ()
  "Join consecutive '* foo:' lines with a comma."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward ":\n*" nil t)
      (replace-match "," t t)
      (end-of-line)
      ;; see if we need to break the line.
      (if auto-fill-function
	  (funcall auto-fill-function))
      ;; move back before the colon, so we can join this with the next line.
      (forward-char -1))))

;;;###autoload
(defun rgr-cvs-log-edit-hook ()
  (define-key log-edit-mode-map "\C-c+" 'rgr-cvs-plus))

;;;###autoload
(defun rgr-change-log-insert-plus ()
  "Insert a '  + ' at the beginning of the current line."
  (interactive)
  (cond ((not (bolp))
	  (save-excursion
	    (beginning-of-line)
	    (rgr-change-log-insert-plus)))
	(t
	  (cond ((looking-at "^[ \t]*\\* ")
		  (insert "\n")
		  (forward-line -1)))
	  (skip-chars-forward " \t")
	  (indent-to 11)
	  (insert "+ "))))

;;;###autoload
(defun rgr-change-log-edit-hook ()
  (define-key change-log-mode-map "\C-c+" 'rgr-change-log-insert-plus))

;;;###autoload
(defun rgr-diff-mode-hook ()
  (define-key diff-mode-map "\C-c!" 'rgr-diff-add-definition-comment))

(provide 'rgr-cvs-hacks)
