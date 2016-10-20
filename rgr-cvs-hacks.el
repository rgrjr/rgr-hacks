;;;; Hacks for version control.
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
  (require 'ewoc)
  (require 'vc))

;; Quiet the compiler.
(defvar vc-ewoc)

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

;;;; Log stuff.

(defvar rgr-vc-backend-to-log-command
  (let ((script
	 (if (executable-find "perl") "vc-chrono-log.pl" "vc-chrono-log.rb")))
    `((CVS ,(concat "cvs -q log -d '>%s' | " script))
      (Git ,(concat "(git show-ref --head && git log --since '%s') | " script))
      (SVN ,(concat "svn log --xml --verbose --revision '{%s}:HEAD' | "
		    script))))
  "Alist mapping backend names to log summary commands for handled
version control back ends.")

(defvar vc-recent-changes-number-of-days 7
  "Default number of history days to show.")

(defun rgr-vc-number-of-days (number-of-days)
  ;; Interpret a raw numeric argument for rgr-vc-recent-changes.
  (cond ((not number-of-days) vc-recent-changes-number-of-days)
	((integerp number-of-days) number-of-days)
	((equal number-of-days '(4))
	  ;; C-u
	  14)
	((equal number-of-days '(16))
	  ;; C-u C-u
	  30)
	((equal number-of-days '(64))
	  ;; C-u C-u C-u
	  90)
	((consp number-of-days) (car number-of-days))
	(t (error "got number-of-days %S" number-of-days))))

(defun rgr-vc-recent-changes-buffer-name (&optional directory)
  (or directory
      (setq directory default-directory))
  (let* ((base-name (file-name-nondirectory (directory-file-name directory)))
	 (buf-name (concat "*" base-name "-recent-changes*"))
	 (buffer nil)
	 (idx 1))
    (while (let ((buffer (get-buffer buf-name)))
	     (and buffer
		  (not (equal (with-current-buffer buffer
				default-directory)
			      directory))))
      (setq idx (1+ idx))
      (setq buf-name (format "*%s-%d-recent-changes*" base-name idx)))
    buf-name))

;;;###autoload
(defun rgr-vc-recent-changes (directory &optional number-of-days)
  "Show a summary of VCS log output using vc-chrono-log.pl.
The summary is in reverse chronological order, with changed files
shown where possible, and is put in a buffer named '*dir-recent-changes*',
where 'dir' is the working copy directory name.  By default, it covers
the last week; the default number of days comes from the
vc-recent-changes-number-of-days variable.

A numeric argument shows the log for that many days, and also prompts
for a directory.  If you give a C-u, it shows the last two weeks' worth;
if C-u C-u, then the last 30 days; if C-u C-u C-u, then 90 days."
  (interactive
   (list (if current-prefix-arg
	     (file-truename
	       (read-file-name "VC recent changes for directory: "
			       default-directory default-directory t
			       nil #'file-directory-p))
	     default-directory)
	 current-prefix-arg))
  (require 'time-date)		;; part of gnus
  (let* ((default-directory directory)
	 (backend (vc-responsible-backend directory))
	 (command-format
	   (cond ((null backend)
		   (error "The directory %S is not under version control."
			  directory))
		 ((car (cdr (assoc backend rgr-vc-backend-to-log-command))))
		 (t
		   (error "Don't know how to deal with backend '%S'."
			  backend))))
	 (number-of-days (rgr-vc-number-of-days number-of-days))
	 (n-days-ago (subtract-time (current-time)
				    (days-to-time number-of-days)))
	 (n-days-ago-string
	   ;; this is an easy-to-parse format that is understood by all the VC
	   ;; backends I use.  -- rgr, 26-Nov-05.
	   (format-time-string "%Y-%m-%d %H:%M" n-days-ago))
	 (buf-name (rgr-vc-recent-changes-buffer-name default-directory))
	 (old-point (and (equal buf-name (buffer-name))
			 (eq major-mode 'vc-history-mode)
			 ;; We're in the output buffer already, so we must be
			 ;; reverting; attempt to preserve point.
			 (let ((point (point)))
			   (save-excursion
			     (or (looking-at "^[0-9-]+ [0-9:]+:$")
				 (log-view-msg-prev))
			     (list (buffer-substring-no-properties
				     (point)
				     (save-excursion (end-of-line) (point)))
				   (- point (point))))))))
    ;; (error "Date '%s'." n-days-ago-string)
    (let ((output (get-buffer-create buf-name)))
      (shell-command (format command-format n-days-ago-string) output)
      (with-current-buffer output
	;; must preserve the default directory so that vc-history-diff knows
	;; where to operate.
	(setq default-directory directory)
	(vc-history-mode)
	;; Make this revert-able.
	(set (make-local-variable 'vc-recent-changes-number-of-days)
	     number-of-days)
	(set (make-local-variable 'revert-buffer-function)
	     #'(lambda (ignore-auto noconfirm)
		 (rgr-vc-recent-changes default-directory
					vc-recent-changes-number-of-days)))
	(when (and old-point
		   (search-forward (car old-point) nil t))
	  ;; Restore our place in the buffer.
	  (beginning-of-line)
	  (forward-char (cadr old-point)))
	(set-buffer-modified-p nil)))))

(defun vc-history-set-days (number-of-days)
  "In a vc-history buffer, set the days shown with a numeric arg, and revert.
The numeric arg (e.g. for C-u) is interpreted the same way as for
\\[rgr-vc-recent-changes].  Without a numeric arg, reset to the default."
  (interactive (list current-prefix-arg))
  (rgr-vc-recent-changes default-directory
			 (rgr-vc-number-of-days number-of-days)))

(defun rgr-vc-find-recent-changes (directory)
  "Find summary of VCS log output for directory without updating it."
  (interactive
   (list (if current-prefix-arg
	     (file-truename
	       (read-file-name "Find VC recent changes for directory: "
			       default-directory default-directory t
			       nil #'file-directory-p))
	     default-directory)))
  (let* ((buf-name (rgr-vc-recent-changes-buffer-name directory))
	 (output (get-buffer buf-name)))
    (if output
	(pop-to-buffer output)
      (rgr-vc-recent-changes directory vc-recent-changes-number-of-days))))

;;;###autoload
(defun rgr-vc-project-diff ()
  "Diff for the 'project' rooted at the current directory non-interactively.
This would be just a shorthand for the vc-diff command (\\[vc-diff])
when asked to compare a working directory to the original CVS version
\(e.g. 'C-u \\[vc-diff] \".\" RET RET RET'), but it also renames the
output buffer from '*vc-diff*' to '*vc-project-diff*'."
  (interactive)
  (if (>= emacs-major-version 23)
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
    (rename-buffer "*vc-project-diff*")
    ;; Unfortunately, this still trashes the "*vc-diff*" buffer.
    (set (make-local-variable 'revert-buffer-function)
	 '(lambda (ignore-auto noconfirm) (rgr-vc-project-diff)))))

;;;; vc-dir hacks.

;;;###autoload
(defun rgr-find-file-vc-dir-node (current-file)
  "Find current-file in a vc-dir buffer, returning (node buffer)."
  (interactive)
  (let ((vc-dir-buffer nil) (vc-dir-default-directory-len 0)
	(vc-dir-node nil))
    ;; Look for a vc-dir buffer that includes current-file, or a buffer that
    ;; might contain current-file if it were visible.
    (let ((tail (buffer-list)))
      (while (and tail (not vc-dir-node))
	(let ((buffer (car tail)))
	  (with-current-buffer buffer
	    (if (and (eq major-mode 'vc-dir-mode))
		;; Search for a node for current-file.
		(let ((node (ewoc-nth vc-ewoc 0))
		      (dd-len (length default-directory)))
		  (while node
		    (let ((nodefile (vc-dir-fileinfo->name (ewoc-data node))))
		      ;; (message "[checking %S]" nodefile)
		      (if (string-equal (expand-file-name nodefile)
					current-file)
			  ;; Success:  Don't set vc-dir-default-directory-len,
			  ;; which is only used for finding a fallback.
			  (setq vc-dir-buffer buffer
				vc-dir-node node
				tail nil node nil)))
		    (setq node (ewoc-next vc-ewoc node)))
		  ;; If not found, but the directory is a prefix of
		  ;; current-file, then remember the buffer as a fallback.
		  (if (and (null vc-dir-node)
			   (> (length current-file) dd-len)
			   (string-equal (substring current-file 0 dd-len)
					 default-directory)
			   ;; When we have multiple candidates, pick the one
			   ;; deeper in the directory hierarchy.
			   (or (null vc-dir-buffer)
			       (> dd-len vc-dir-default-directory-len)))
		      (setq vc-dir-buffer buffer
			    vc-dir-default-directory-len dd-len))))))
	(setq tail (cdr tail))))
    (list vc-dir-node vc-dir-buffer)))

;;;###autoload
(defun rgr-vc-dir-goto-node-or-buffer (vc-dir-node vc-dir-buffer)
  '(message "got %S in %S" vc-dir-node vc-dir-buffer)
  (cond ((not vc-dir-buffer)
	  ;; Totally failed, so offer to start vc-dir on the backend root
	  ;; directory.
	  (let* ((backend (vc-deduce-backend))
		 (root (or (and backend
				(vc-call-backend backend 'root
						 default-directory))
			   default-directory)))
	    (vc-dir (read-directory-name "VC status for directory: "
					 root root t nil))))
	((not vc-dir-node)
	  (switch-to-buffer-other-window vc-dir-buffer)
	  (ewoc-goto-node vc-ewoc (ewoc-nth vc-ewoc 0))
	  (vc-dir-move-to-goal-column)
	  (message "File is under this buffer, but not visible."))
	(t
	  (switch-to-buffer-other-window vc-dir-buffer)
	  (ewoc-goto-node vc-ewoc vc-dir-node)
	  (vc-dir-move-to-goal-column))))

;;;###autoload
(defun rgr-goto-file-in-vc-dir ()
  "Go to the current file or directory in a vc-dir buffer."
  (interactive)
  (apply #'rgr-vc-dir-goto-node-or-buffer
	 (rgr-find-file-vc-dir-node
	   (or buffer-file-name
	       default-directory
	       (error "Buffer has no file name.")))))

;;;; vc-history mode.

;; This used for navigating and operating on the output of the
;; rgr-vc-recent-changes command (q.v.).  Conceptually, it works like
;; log-view-mode, except that each entry describes a set of changes that were
;; committed at the same time, where as log-view-mode only works on a file at a
;; time.

(require 'log-view)

(defvar vc-history-message-re "^\\([0-9]+-[0-9]+-[0-9]+ [0-9:]+\\):"
  "Regular expression that matches a date.
Really, this is only useful for CVS histories.")

(defgroup vc-history nil
  "Major mode for browsing M-x rgr-vc-recent-changes summaries of VC history."
  :group 'pcl-cvs
  :prefix "vc-history-")

(easy-mmode-defmap vc-history-mode-map
  '(("q" . quit-window)
    ("z" . kill-this-buffer)
    ("m" . set-mark-command)
    ("d" . vc-history-diff)
    ("G" . vc-history-set-days)
    ("=" . vc-history-diff)
    ;; ("f" . log-view-find-version)
    ("n" . log-view-msg-next)
    ("p" . log-view-msg-prev))
  "VC-History's keymap."
  :group 'vc-history)

(defvar vc-log-fileset)

;;;###autoload
(define-derived-mode vc-history-mode log-view-mode "VC-History"
  "Major mode for browsing VC summary log output."
  (set (make-local-variable 'log-view-message-re) vc-history-message-re)
  ;; This makes "d" work in history buffers.
  (set (make-local-variable 'vc-log-fileset) (list default-directory)))

(defun vc-history-current-tag (&optional where)
  "Return a tag that describes the revision at point, or at WHERE if supplied.
For Subversion, this will be the revision number; for Git, the revision hash;
otherwise, it is the date."
  (save-excursion
    (when where
      (goto-char where))
    (cond ((re-search-forward "^ *revision: *\\([0-9a-f]+\\)")
	    (match-string 1))
	  ((save-excursion
	     (forward-line 1)
	     (re-search-backward vc-history-message-re nil t))
	    (match-string 1)))))

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
  (require 'rgr-new-vc-hacks)	;; for the updated vc-deduce-fileset
  (let ((fr (vc-history-current-tag beg))
        (to (vc-history-current-tag end)))
    (when (string-equal fr to)
      (save-excursion
        (goto-char end)
        (log-view-msg-next)
        (setq to (vc-history-current-tag))))
    '(message "[default dir %S backend %S]"
	     default-directory (vc-responsible-backend default-directory))
    (let ((vc-log-fileset (list default-directory)))
      ;; [kludge:  bind vc-log-fileset here so that the vc-deduce-fileset hack
      ;; in rgr-new-vc-hacks.el for Emacs 23 can use it.  -- rgr, 2-Jul-09.]
      (vc-version-diff default-directory to fr))))

;;;; Finding definition names and adding definition comments.

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

(defun rgr-vc-all-comment-files (&optional start end)
  ;; Get the name of all files commented in this buffer.
  (save-excursion
    (goto-char (or start (point-min)))
    (let ((result nil))
      (while (re-search-forward "^\\* +" end t)
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

(defun rgr-add-definition-comment-internal (name &optional source-buffer)
  ;; Given a name in source-buffer (which defaults to the current buffer),
  ;; insert "   + (def-name): " into to the current comment buffer.
  (let* ((source-buffer (or source-buffer (current-buffer)))
	 (changed-file (buffer-file-name source-buffer)))
    (switch-to-buffer-other-window
      (or (and vc-parent-buffer
	       ;; Take the parent buffer if it's a log buffer.
	       (with-current-buffer vc-parent-buffer
		 (derived-mode-p 'log-edit-mode))
	       vc-parent-buffer)
	  ;; If coming from a file buffer, look for the corresponding log.
	  (and (fboundp 'vc-log-buffer-for-file)
	       (with-current-buffer source-buffer
		 (and buffer-file-name
		      (vc-log-buffer-for-file buffer-file-name))))
	  (error "No associated *VC-log* buffer.")))
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
	 (name (with-current-buffer source-buffer
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
	(t
	  ;; somewhere on a non-empty line
	  (or (looking-at paragraph-start)
	      (forward-paragraph 1))
	  (insert "\n")
	  (or (eobp)
	      (forward-char -1))))
  (insert "   + ")
  (cond ((eobp)
	  ;; end of buffer; make this a complete line.
	  (insert "\n")
	  (forward-char -1))))

(defvar rgr-vc-log-join-respect-fill-column t
  "Whether to skip joining if it would cross over the fill-column.")

(defun rgr-vc-log-join-consecutive-file-headings ()
  "Join consecutive '* foo:' lines with a comma, from here to EOB."
  (interactive)
  (save-excursion
    (while (search-forward ":\n*" nil t)
      (goto-char (match-end 0))
      (cond ((not auto-fill-function)
	      (replace-match "," t t))
	    (rgr-vc-log-join-respect-fill-column
	      (let ((new-end-col
		     (+ (save-excursion
			  (goto-char (match-beginning 0))
			  (current-column))
			;; We match three chars, and replace them with one.
			-2
			(save-excursion
			  (goto-char (match-end 0))
			  (end-of-line)
			  (current-column)))))
		(if (<= new-end-col fill-column)
		    (replace-match "," t t))))
	    (t
	      (replace-match "," t t)
	      (funcall auto-fill-function))))
    ;; Move back before the colon, so we can join this with the next line.
    (forward-char -1)))

(defun rgr-vc-log-join-previous-file-heading ()
  "Join this line with the '* foo:' on the previous line."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (skip-chars-backward " \t\n:")
    (or (looking-at ":[ \t]*\n[ \t\n]*\\*")
	(error "Not in the second of two file heading lines.")))
  (replace-match "," t t))

(defun rgr-vc-log-diff ()
  "Do vc-diff, then go back to the original window."
  (interactive)
  (vc-diff)
  (other-window 1))

;;;###autoload
(defun rgr-vc-log-edit-hook ()
  ;; This makes fill-paragraph operate on each comment individually.
  (set (make-local-variable 'paragraph-start) "^[ \t]*\\($\\|[*+]\\)")
  (define-key log-edit-mode-map "\C-cJ"
    'rgr-vc-log-join-consecutive-file-headings)
  (define-key log-edit-mode-map "\C-cj" 'rgr-vc-log-join-previous-file-heading)
  (define-key log-edit-mode-map "\C-c+" 'rgr-vc-log-plus)
  (if (rgr-emacs-version-p 24 4)
      (setq log-edit-hook '(vc-log-insert-fileset-skeleton
			    ;; vc-diff must come last because it changes the
			    ;; current window.
			    vc-diff)))
  (if (rgr-emacs-version-p 23)
      (new-vc-install-log-edit-mode-keys))
  ;; vc-diff must come last because it changes the current window.
  (save-excursion
    (vc-diff)))

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
