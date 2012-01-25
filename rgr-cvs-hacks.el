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

;;;; Log stuff.

(defvar rgr-vc-backend-to-log-command
  (let ((script
	  ;; Prefer the Ruby version if Ruby is available, else fall back to the
	  ;; Perl script.
	  (if (executable-find "ruby") "vc-chrono-log.rb" "vc-chrono-log.pl")))
    `((CVS ,(concat "cvs -q log -d '>%s' | " script))
      (SVN ,(concat "svn log --xml --verbose --revision '{%s}:HEAD' | "
		    script))))
  "Alist mapping backend names to log summary commands for handled
version control back ends.")

(defvar vc-recent-changes-number-of-days 3
  "Default number of history days to show.")

;;;###autoload
(defun rgr-vc-recent-changes (directory &optional number-of-days)
  "Show a summary of 'cvs log' or 'svn log' output.
The summary is in reverse chronological order, with changed files
shown where possible.  By default it covers the last three days
for the current directory.  A numeric argument shows the log for
that many days, and also prompts for a directory.  If you give a
C-u, it shows the last week's worth; if C-u C-u, then the last
month (30 days, actually)."
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
	 (number-of-days
	   (cond ((integerp number-of-days) number-of-days)
		 ((not number-of-days) vc-recent-changes-number-of-days)
		 ((equal number-of-days '(4))
		   ;; control-U
		   7)
		 ((equal number-of-days '(16))
		   ;; control-U control-U
		   30)
		 ((consp number-of-days) (car number-of-days))
		 (t (error "got number-of-days %S" number-of-days))))
	 (n-days-ago (subtract-time (current-time)
				    (days-to-time number-of-days)))
	 (n-days-ago-string
	   ;; this is an easy-to-parse format that is understood by all the VC
	   ;; backends I use.  -- rgr, 26-Nov-05.
	   (format-time-string "%Y-%m-%d %H:%M" n-days-ago)))
    ;; (error "Date '%s'." n-days-ago-string)
    (let ((output (get-buffer-create "*vc-recent-changes*")))
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
					vc-recent-changes-number-of-days)))))))

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
			  (end-of-line)
			  (current-column))
			(1- (- (match-end 0) (match-beginning 0)))
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

;;;###autoload
(defun rgr-vc-log-edit-hook ()
  ;; This makes fill-paragraph operate on each comment individually.
  (set (make-local-variable 'paragraph-start) "^[ \t]*\\($\\|[*+]\\)")
  (define-key log-edit-mode-map "\C-cJ"
    'rgr-vc-log-join-consecutive-file-headings)
  (define-key log-edit-mode-map "\C-cj" 'rgr-vc-log-join-previous-file-heading)
  (define-key log-edit-mode-map "\C-c+" 'rgr-vc-log-plus)
  (if (rgr-emacs-version-p 23)
      (new-vc-install-log-edit-mode-keys)))

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
