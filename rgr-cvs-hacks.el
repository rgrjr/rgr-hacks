;;;; Hacks for CVS.
;;;
;;; [created.  -- rgr, 4-Dec-03.]
;;;
;;; Put
;;;
;;;	(define-key text-mode-map "\C-c+" 'rgr-vc-log-plus)
;;;	(add-hook 'log-edit-mode-hook 'rgr-vc-log-edit-hook)
;;;
;;; somewhere in your .emacs to use this.
;;;
;;; $Id$

(require 'vc)

(eval-when-compile
  (require 'add-log)
  (require 'log-edit)
  (require 'vc))

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

;;;; Log stuff.

(defvar rgr-vc-backend-to-log-command
  (let ((cvs-command "cvs -q log -d '>%s' | ")
	(svn-command "svn log --xml --verbose --revision '{%s}:HEAD' | "))
    ;; Prefer the Ruby version if Ruby is available, else fall back to the Perl
    ;; scripts.
    (if (executable-find "ruby")
      `((CVS ,(concat cvs-command "vc-chrono-log.rb"))
	(SVN ,(concat svn-command "vc-chrono-log.rb")))
      `((CVS ,(concat cvs-command "cvs-chrono-log.pl"))
	(SVN ,(concat svn-command "svn-chrono-log.pl")))))
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
  (let* ((original-directory default-directory)
	 (backend (vc-responsible-backend original-directory))
	 (command-format
	   (cond ((null backend)
		   (error "The directory %S is not under version control."
			  original-directory))
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
    (let ((output (get-buffer-create "*vc-recent-changes*")))
      (shell-command (format command-format n-days-ago-string) output)
      (save-excursion
	(set-buffer output)
	;; must preserve the default directory so that vc-history-diff knows
	;; where to operate.
	(setq default-directory original-directory)
	(vc-history-mode)))))

;;;###autoload
(defun rgr-vc-project-diff ()
  "Diff for the 'project' rooted at the current directory non-interactively.
This would be just a shorthand for the vc-diff command (\\[vc-diff])
when asked to compare a working directory to the original CVS version
\(e.g. 'C-u \\[vc-diff] \".\" RET RET RET'), but it also renames the
output buffer from '*vc-diff*' to '*vc-project-diff*'."
  (interactive)
  (if (>= rgr-emacs-major-version 23)
      ;; The new way uses filesets (and vc-version-diff ignores its first arg).
      (let* ((files (list (expand-file-name ".")))
	     (backend (vc-responsible-backend default-directory))
	     (fileset (list backend files files nil nil)))
	(vc-diff-internal t fileset nil nil))
      ;; The old way just requires us to pass the directory name.
      (vc-version-diff (expand-file-name ".") nil nil))
  ;; Rename the buffer to "*vc-project-diff*".
  (with-current-buffer "*vc-diff*"
    (let ((old-buf (get-buffer "*vc-project-diff*")))
      (and old-buf
	   (kill-buffer old-buf)))
    (rename-buffer "*vc-project-diff*")))

;;;; vc-history mode.

;; This used for navigating and operating on the output of the
;; rgr-vc-recent-changes command (q.v.).  Conceptually, it works like
;; log-view-mode, except that each entry describes a set of changes that were
;; committed at the same time, where as log-view-mode only works on a file at a
;; time.

(require 'log-view)

(defvar vc-history-message-re "^\\([0-9]+-[0-9]+-[0-9]+ [0-9:]+\\):")

(defgroup vc-history nil
  "Major mode for browsing M-x rgr-vc-recent-changes summaries of VC history."
  :group 'pcl-cvs
  :prefix "vc-history-")

(easy-mmode-defmap vc-history-mode-map
  '(("q" . quit-window)
    ("z" . kill-this-buffer)
    ("m" . set-mark-command)
    ("d" . vc-history-diff)
    ;; ("f" . log-view-find-version)
    ("n" . log-view-msg-next)
    ("p" . log-view-msg-prev))
  "VC-History's keymap."
  :group 'vc-history)

;;;###autoload
(define-derived-mode vc-history-mode log-view-mode "VC-History"
  "Major mode for browsing VC summary log output."
  (set (make-local-variable 'log-view-message-re) vc-history-message-re))

(defun vc-history-current-tag (&optional where)
  "Return a tag that describes the revision at point, or at WHERE if supplied.
For Subversion, this will be the revision number; otherwise, it is the date."
  (save-excursion
    (when where
      (goto-char where))
    (forward-line 1)
    (if (re-search-backward vc-history-message-re nil t)
	(let ((date-tag (match-string 1)))
	  ;; try to do better for SVN.
	  (forward-line 1)
	  (if (looking-at "^ *revision: *\\([0-9]+\\)")
	      (match-string 1)
	      date-tag)))))

(defun vc-history-diff (beg end)
  "Get the diff between two revision summaries.
If the mark is not active or the mark is on the revision at point,
get the diff between the revision at point and its previous revision.
Otherwise, get the diff between the revisions where the region starts
and ends.

\[This works better for Subversion than it does for CVS, simply because
Subversion has well-defined revision numbers, and CVS has fuzzier dates.
-- rgr, 12-Mar-06.]"
  (interactive
    (list (if mark-active (region-beginning) (point))
	  (if mark-active (region-end) (point))))
  (let ((fr (vc-history-current-tag beg))
        (to (vc-history-current-tag end)))
    (when (string-equal fr to)
      (save-excursion
        (goto-char end)
        (log-view-msg-next)
        (setq to (vc-history-current-tag))))
    (message "[default dir %S backend %S]"
	     default-directory (vc-responsible-backend default-directory))
    (vc-version-diff default-directory to fr)))

;;;; Committing multiple files via the revision comment.

;;;###autoload
(defun rgr-vc-commit-with-comments ()
  ;; Based on log-edit-done and vc-next-action-dired fns.  -- rgr, 15-Feb-05.
  "Do the next logical version control operation (as by \\[vc-next-action]) on
the files named in the current buffer, using its contents as the log comment.
Only those files will be included that are mentioned explicitly in the
following style:

    * foo.el:
    * quux.el, ruux.el:
    * bar.el (added), baz.c (deleted):

The '*' must be at the start of the line.  Other comments are ignored."
  (interactive)
  (require 'log-edit)
  (let ((files-to-commit (or (rgr-vc-all-comment-files)
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
    ;; (error "Going to commit %S with %S" files-to-commit (current-buffer))
    (let ((comment (buffer-string)))
      (let ((comment-ring
	      (cond ((boundp 'log-edit-comment-ring)
		      ;; [emacs 22.1 and later.]
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
	  (if (fboundp 'vc-perform-checkin)
	      (vc-perform-checkin files-to-commit nil comment)
	      (while files-to-commit
		(let ((file (car files-to-commit)))
		  (message "Processing %s..." file)
		  (vc-next-action-on-file file nil comment)
		  (message "Processing %s...done" file))
		(setq files-to-commit (cdr files-to-commit))))
	  ;; no.
	  (message "Oh, well!  Later maybe?")))))

(defun rgr-makefile-definition-name ()
  (if (re-search-backward "^\\([^: \t\n]+\\):" nil t)
      (match-string 1)))

(put 'makefile-mode 'mode-definition-name 'rgr-makefile-definition-name)
(put 'makefile-gmake-mode 'mode-definition-name 'rgr-makefile-definition-name)

(defun rgr-pir-mode-definition-name ()
  (and (re-search-backward "^\\.sub[ \t]+\\([^ \t\n]+\\)" nil t)
       (match-string 1)))

(put 'pir-mode 'mode-definition-name 'rgr-pir-mode-definition-name)

(defun rgr-mode-definition-name ()
  "Find the name of the current definition."
  (let ((mode-finder (get major-mode 'mode-definition-name)))
    (save-excursion
      (cond (mode-finder
	      (funcall mode-finder))
	    (t
	      (message "Can't find definitions for %S mode." major-mode)
	      (sit-for 2)
	      nil)))))

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
      (or (and (fboundp 'vc-log-buffer-for-file)
	       (save-excursion
		 (set-buffer source-buffer)
		 (vc-log-buffer-for-file buffer-file-name)))
	  (rgr-current-comment-buffer source-buffer)
	  (find-file-noselect "comment.text")))
    (let ((current-comment-files (rgr-vc-current-comment-files))
	  (comment-start nil))
      (cond ((member changed-file current-comment-files)
	      ;; assume we're in the right place.
	      )
	    ((setq comment-start (rgr-vc-find-file-comment changed-file))
	      '(error "Changed file %S but buffer is at files %S."
		     changed-file current-comment-files)
	      (goto-char comment-start)
	      (forward-line))))
    (rgr-vc-log-plus)
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
	 ;; [up to emacs 21.3, pos is the start of the src text in buffer; in
	 ;; emacs 22+, pos is a (buf-start . buf-end) pair.  -- rgr, 23-Apr-06.]
	 (loc (diff-find-source-location other-file rev))
	 (source-buffer (car loc))
	 (pos (nth 2 loc))
	 (src-buf-start (if (consp pos) (car pos) pos))
	 (src (nth 3 loc))
	 (name (save-excursion
		 (set-buffer source-buffer)
		 (goto-char (+ src-buf-start (cdr src)))
		 (rgr-mode-definition-name))))
    (rgr-add-definition-comment-internal name source-buffer)))

;;;###autoload
(defun rgr-vc-log-plus ()
  "Insert a '  + ' at point, starting a new line if not at BOL."
  (interactive)
  ;; (message "wogga [1] point %s bolp %s eolp %s" (point) (bolp) (eolp))
  (cond ((and (eolp) (bolp))
	  ;; empty line, no adjustment needed.
	  )
	((looking-at paragraph-start)
	  ;; beginning of non-empty line with stuff already on it; move it to
	  ;; the next line.
	  (insert "\n")
	  (forward-char -1))
	((not (bolp))
	  ;; in the middle or end of a non-empty line
	  (forward-paragraph 1)
	  (insert "\n")
	  (forward-char -1)))
  (insert "   + "))

(defun rgr-vc-log-join-consecutive-file-headings ()
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
(defun rgr-vc-log-edit-hook ()
  ;; This makes fill-paragraph operate on each comment individually.
  (set (make-local-variable 'paragraph-start) "^[ \t]*\\($\\|[*+]\\)")
  (define-key log-edit-mode-map "\C-cj"
    'rgr-vc-log-join-consecutive-file-headings)
  (define-key log-edit-mode-map "\C-c+" 'rgr-vc-log-plus))

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

;; [historical . . . ]
(provide 'rgr-cvs-hacks)
;; [ . . . and modern.  -- rgr, 15-Feb-06.]
(provide 'rgr-vc-hacks)
