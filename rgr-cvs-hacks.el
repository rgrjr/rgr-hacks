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

(defun rgr-comment-buffers (&optional buffer-directory)
  (let ((tail (buffer-list))
	(dir (or buffer-directory default-directory))
	(result nil))
    (while tail
      (let ((buffer (car tail)))
	(save-excursion
	  (set-buffer buffer)
	  ;; (message "[dir %S default %S]" dir default-directory)
	  (if (and (eq major-mode 'text-mode)
		   (string-match "^comment" (buffer-name))
		   (let ((dd-len (length default-directory)))
		     (and (<= dd-len (length dir))
			  (equal default-directory (substring dir 0 dd-len)))))
	      (setq result (cons buffer result))))
	(setq tail (cdr tail))))
    (nreverse result)))

;; (rgr-comment-buffers)

(defvar rgr-vc-backend-to-log-command
	'((CVS "cvs -q log -d '>%s' | cvs-chrono-log.pl")
	  (SVN "svn log --xml --revision '{%s}:HEAD' | svn-chrono-log.pl"))
  "Alist mapping backend names to log summary commands for handled
version control back ends.")
 
;;;###autoload
(defun rgr-vc-recent-changes (&optional number-of-days)
  "Show a reverse-chronological summary of 'cvs log' or 'svn log' with
changed files added where possible.  By default it covers the last three
days.  If you give a C-u, it shows the last week's worth; if C-u C-u,
then the last month (30 days, actually).  Any other numeric argument
shows the log for that many days."
  (interactive "P")
  (require 'time-date)		;; part of gnus
  (let* ((backend (vc-responsible-backend default-directory))
	 (command-format
	   (cond ((null backend)
		   (error "The directory %S is not under version control."
			  default-directory))
		 ((car (cdr (assoc backend rgr-vc-backend-to-log-command))))
		 (t
		   (error "Don't know how to deal with backend '%S'."
			  backend))))
	 (number-of-days
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
	   ;; this is an easy-to-parse format that is understood by all the VC
	   ;; backends I use.  -- rgr, 26-Nov-05.
	   (format-time-string "%Y-%m-%d %H:%M" n-days-ago)))
    ;; (error "Date '%s'." n-days-ago-string)
    (shell-command (format command-format n-days-ago-string))))

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
(defun rgr-vc-commit-with-comments ()
  ;; Based on log-edit-done and vc-next-action-dired fns.  -- rgr, 15-Feb-05.
  "Do the next logical version control operation (as by \\[vc-next-action]) on
the files named in the current buffer, using its contents as the log comment.
Only those files mentioned explicitly in the buffer in style of the
\\[rgr-cvs-insert-log-skeleton] command will be included."
  (interactive)
  (require 'vc)
  (require 'log-edit)
  (let ((was-saved-p (not (buffer-modified-p)))
	(files-to-commit (or (rgr-vc-all-comment-files)
			     (error "Can't find any comment files in %s."
				    (current-buffer)))))
    ;; Get rid of trailing empty lines
    (goto-char (point-max))
    (skip-syntax-backward " ")
    (when (equal (char-after) ?\n) (forward-char 1))
    (delete-region (point) (point-max))
    ;; Check for final newline
    (if (and (> (point-max) (point-min))
	     (/= (char-before (point-max)) ?\n)
	     (or (eq log-edit-require-final-newline t)
		 (and log-edit-require-final-newline
		      (y-or-n-p
		       (format "Buffer %s does not end in newline.  Add one? "
			       (buffer-name))))))
	(save-excursion
	  (goto-char (point-max))
	  (insert ?\n)))
    (if (and (buffer-modified-p)
	     (or was-saved-p
		 (y-or-n-p (format "Save %s? " (buffer-name)))))
	(save-buffer))
    ;; (error "Going to commit %S with %S" files-to-commit (current-buffer))
    (let ((comment (buffer-string)))
      (let ((comment-ring
	      (cond ((boundp 'log-edit-comment-ring)
		      (symbol-value 'log-edit-comment-ring))
		    ((boundp 'vc-comment-ring)
		      ;; [old name (21.3 at least).  -- rgr, 12-Dec-05.]
		      (symbol-value 'vc-comment-ring)))))
	(when (and comment-ring
		   (or (ring-empty-p comment-ring)
		       (not (equal comment (ring-ref comment-ring 0)))))
	  (ring-insert comment-ring comment)))
      (if (let ((win (get-buffer-window log-edit-files-buf)))
	    (unwind-protect
		 (or (not log-edit-confirm)
		     ;; (and (eq log-edit-confirm 'changed)
		     ;;      (equal files-to-commit log-edit-initial-files))
		     (let ((log-edit-listfun
			    (function (lambda () files-to-commit))))
		       (log-edit-show-files)
		       (y-or-n-p (format "Commit these %d files? "
					 (length files-to-commit)))))
	      (or win
		  (log-edit-hide-buf))))
	  ;; yes!
	  (while files-to-commit
	    (let ((file (car files-to-commit)))
	      (message "Processing %s..." file)
	      (vc-next-action-on-file file nil comment)
	      (message "Processing %s...done" file))
	    (setq files-to-commit (cdr files-to-commit)))
	  ;; no.
	  (message "Oh, well!  Later maybe?")))))

(defvar rgr-cvs-diff-skeleton-entry-regexp
	(concat "^\\+\\+\\+ \\([^ \t\n]*\\)\t" 
		"\\|^cvs \\(diff\\|server\\): \\([^ \t\n]*\\) "
		"\\(is a new entry\\|was removed\\)")
  "Regexp matching things of interest to the rgr-cvs-insert-log-skeleton cmd.")

;;;###autoload
(defun rgr-cvs-insert-log-skeleton ()
  "Insert a '* filename:' line for each file that appears in diff output.
This inserts one line at the end of the current buffer for each file
mentioned in a diff buffer that doesn't already appear in the current
buffer (allowing for \\[rgr-cvs-join-consecutive-file-headings]).  The
current buffer is assumed to be something like a CVS log comment.  The
diff buffer is the most recently visited one of '*vc-project-diff*'
\(created by the \\[rgr-vc-project-diff] command\), '*VC-diff*' (created
by the \\[vc-diff] command), or '*Shell Command Output*' (assumed to be
the output of 'cvs diff'.)  Files that are being added or deleted are
noted as such."
  (interactive)
  (let* ((comment-buffer (current-buffer))
	 (commented-files (rgr-vc-all-comment-files))
	 (extra-files (append commented-files nil))
	 (other-buffer
	   (or (rgr-find-more-recent-buffer
		 (get-buffer "*vc-project-diff*")
		 (get-buffer "*Shell Command Output*")
		 (get-buffer "*VC-diff*"))
	       (error "Can't find %S, %S, or %S buffer." "*VC-diff*"
		      "*vc-project-diff*" "*Shell Command Output*"))))
    (save-excursion
      (set-buffer other-buffer)
      (goto-char (point-min))
      (while (re-search-forward rgr-cvs-diff-skeleton-entry-regexp nil t)
	(let* ((file-name (or (match-string 1) (match-string 3)))
	       (full-file-name (expand-file-name file-name))
	       (extra (match-string 4)))
	  ;; (message "found %S" file-name)
	  (setq extra-files (delete full-file-name extra-files))
	  (or (member full-file-name commented-files)
	      (save-excursion
		(set-buffer comment-buffer)
		(goto-char (point-max))
		(insert "* " file-name
			(cond ((equal extra "was removed") " (deleted)")
			      (extra " (added)")
			      (t ""))
			":\n")
		(setq commented-files
		      (cons full-file-name commented-files)))))))
    (let ((tail extra-files) (n (length extra-files)))
      (while tail
	(message "%S does not appear in the diff." (car tail))
	(sit-for 1)
	(setq tail (cdr tail)))
      (if (> n 1)
	  (message "Total of %d extra files." n)))
    ;; move point to start adding comments, but not if already someplace useful.
    (if (bobp)
	(forward-line 1))
    commented-files))

(defun rgr-makefile-definition-name ()
  (if (re-search-backward "^\\([^: \t\n]+\\):" nil t)
      (match-string 1)))

(defun rgr-mode-definition-name ()
  ;; Total kludge.
  (save-excursion
    (cond ((member major-mode '(emacs-lisp-mode lisp-mode))
	    (if (not (looking-at "^("))
		(beginning-of-defun))
	    (rgr-lisp-def-name t))
	  ((eq major-mode 'perl-mode)
	    (rgr-perl-definition-name))
	  ((eq major-mode 'makefile-mode)
	    (rgr-makefile-definition-name))
	  ((eq major-mode 'pir-mode)
	    (and (re-search-backward "^\\.sub[ \t]+\\([^ \t\n]+\\)" nil t)
		 (match-string 1)))
	  (t
	    (message "Can't find definitions for %S mode." major-mode)
	    (sit-for 2)
	    nil))))

(defun rgr-vc-comment-file-names ()
  ;; Return the comment file names at point, skipping past them.
  (let ((result nil) (give-up-p nil))
    (skip-chars-forward "* \t\n")
    (while (not (or give-up-p (eobp) (looking-at ":")))
      (let ((start (point))
	    (end (progn (skip-chars-forward "^,:() \t\n")
			(point))))
	(cond ((= start end)
		(setq give-up-p t))
	      (t
		(setq result (cons (expand-file-name
				     (buffer-substring start end))
				   result))))
	(skip-chars-forward ", \t\n")
	(while (looking-at "(")
	  (forward-sexp 1)
	  (skip-chars-forward ", \t\n"))))
    (and (looking-at ":[ \t]*")
	 (goto-char (match-end 0)))
    (nreverse result)))

(defun rgr-vc-current-comment-files ()
  ;; Get the name of the files in the current file comment, or nil if before it.
  (save-excursion
    (or (bobp)
	(forward-char -1))
    (and (re-search-backward "^\\* +" nil t)
	 (rgr-vc-comment-file-names))))

(defun rgr-vc-all-comment-files ()
  ;; Get the name of all files commented in this buffer.
  (save-excursion
    (goto-char (point-min))
    (let ((result nil))
    (while (re-search-forward "^\\* +" nil t)
      (setq result (nconc result (rgr-vc-comment-file-names))))
    result)))

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

(defun rgr-current-comment-buffer (&optional for-buffer)
  ;; Given a buffer (which defaults to the current buffer), find the appropriate
  ;; comment buffer, if one exists.  [we may need a better theory for how to
  ;; identify the right comment buffer.  -- rgr, 25-Feb-05.]
  (save-excursion
    (and for-buffer
	 (set-buffer for-buffer))
    (apply (function rgr-find-more-recent-buffer)
	   (get-buffer "*VC-log*")
	   (rgr-comment-buffers))))

(defun rgr-add-definition-comment-internal (name &optional source-buffer)
  ;; Given a name in source-buffer (which defaults to the current buffer),
  ;; insert "   + (def-name): " into to the current comment buffer.
  (let* ((source-buffer (or source-buffer (current-buffer)))
	 (changed-file (buffer-file-name source-buffer)))
    (switch-to-buffer-other-window
      (or (rgr-current-comment-buffer source-buffer)
	  (find-file-noselect "comment.text")))
    (let ((current-comment-files (rgr-vc-current-comment-files))
	  (comment-start nil))
      (cond ((member changed-file current-comment-files)
	      ;; assume we're in the right place.
	      (beginning-of-line))
	    ((setq comment-start (rgr-vc-find-file-comment changed-file))
	      '(error "Changed file %S but buffer is at files %S."
		     changed-file current-comment-files)
	      (goto-char comment-start)
	      (forward-line))
	    (t
	      ;; assume we should add a new entry here.
	      (beginning-of-line))))
    (rgr-cvs-plus)
    (if name
	(insert "(" name "):  "))
    name))

;;;###autoload
(defun rgr-add-definition-comment ()
  "Find the current definition name and add it to the current patch comment."
  (interactive)
  (rgr-add-definition-comment-internal (rgr-mode-definition-name)))

;;;###autoload
(defun rgr-diff-add-definition-comment (&optional other-file)
  "Find the definition name of the corresponding source line.
`diff-jump-to-old-file' (or its opposite if the OTHER-FILE prefix arg
is given) determines whether to jump to the old or the new file.
This is useful, for instance, when a definition has been deleted."
  (interactive "P")
  (let* ((rev (not (save-excursion (beginning-of-line) (looking-at "[-<]"))))
	 ;; loc is a list of (buf line-offset pos src dst &optional switched).
	 (loc (diff-find-source-location other-file rev))
	 (source-buffer (car loc))
	 (pos (nth 2 loc))
	 (src (nth 3 loc))
	 (name (save-excursion
		 (set-buffer source-buffer)
		 (goto-char (+ pos (cdr src)))
		 (rgr-mode-definition-name))))
    (rgr-add-definition-comment-internal name source-buffer)))

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

(provide 'rgr-cvs-hacks)
