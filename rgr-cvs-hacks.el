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

;;;###autoload
(defun rgr-cvs-recent-changes (&optional number-of-days)
  "Show a summary of 'cvs log' output for the last three days.  If you
give a C-u, it shows the last week's worth; if C-u C-u, then the last
month (30 days, actually).  Any other numeric argument shows the log for
that many days."
  (interactive "P")
  (require 'discus)
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
	 (today (car (discus-parse-date (current-time-string))))
	 (n-days-ago (discus-print-date (cons (- today number-of-days) nil))))
    ;; (error "Date '%s'." n-days-ago)
    (shell-command (format "cvs -q log -d '>%s' | cvs-chrono-log.pl"
			   n-days-ago))))

;;;###autoload
(defun rgr-vc-project-diff ()
  "Diff for the 'project' rooted at the current directory non-interactively.
This would be just a shorthand for the vc-diff command (\\[vc-diff])
when asked to compare a working directory to the original CVS version
\(e.g. 'C-u \\[vc-diff] \".\" RET RET RET'), but it also renames the
output buffer from '*vc-diff*' to '*vc-project-diff*'.  This is so that
the \\[rgr-cvs-insert-log-skeleton] command can use this output."
  (interactive)
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
This goes through the '*vc-project-diff*' buffer (created by the
\\[rgr-vc-project-diff] command), and inserts one line for each file at
the end of the current buffer, which is assumed to be something like a
CVS log comment.  If there is no '*vc-project-diff*' buffer but '*Shell
Command Output*' exists, then we assume that contains the output of 'cvs
diff', and use that instead.  Files that are being added or deleted are
noted as such."
  (interactive)
  (let ((original-buffer (current-buffer))
	(other-buffer
	  ;; don't look at "*vc-diff*" because that is often for just one file.
	  (or (get-buffer "*vc-project-diff*")
	      (get-buffer "*Shell Command Output*")
	      (error "Can't find \"*vc-project-diff*\" or %S buffer."
		     "*Shell Command Output*")))
	(match-re (concat "^Index: \\([^ \t\n]*\\)$"
			  "\\|^cvs \\(diff\\|server\\): \\([^ \t\n]*\\) "
			  "\\(is a new entry\\|was removed\\)")))
    (save-excursion
      (set-buffer other-buffer)
      (goto-char (point-min))
      (while (re-search-forward match-re nil t)
	(let ((file-name (or (match-string 1) (match-string 3)))
	      (extra (match-string 4)))
	  ;; (message "found %S" file-name)
	  (save-excursion
	    (set-buffer original-buffer)
	    (goto-char (point-max))
	    (insert "* " file-name
		    (cond ((equal extra "was removed") " (deleted)")
			  (extra " (added)")
			  (t ""))
		    ":\n")))))
    ;; move point to start adding comments.
    (or (eobp)
	(forward-line 1))))

;;;###autoload
(defun rgr-cvs-plus ()
  "Insert a '  + ' at the beginning of the current line."
  (interactive)
  (cond ((not (bolp))
	  (save-excursion
	    (beginning-of-line)
	    (rgr-cvs-plus)))
	(t
	  (cond ((looking-at "^ *\\* ")
		  (insert "\n")
		  (forward-line -1)))
	  (insert "   + "))))

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

(provide 'rgr-cvs-hacks)
