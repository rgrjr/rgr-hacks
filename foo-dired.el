;;;; rgr-dired stuff
;;;
;;;    Useful stuff in here is rgr-dired-rename-file-and-versions and
;;; rgr-dired-sort-by-size (but the latter is buggy).
;;;
;;;    To compile this (almost) without error, do the following:
;;;
;;;	(mapcar 'require '(dired dired-aux))
;;;
;;;    Modification history:
;;;
;;; rgr-dired-sort-by-size: new, but semi-kludgey, fix
;;;	rgr-exchange-point-and-mark C-u bug.  -- rgr, 21-Feb-98.
;;; rgr-dired-sort-by-size: made sticky.  -- rgr, 26-Feb-98.
;;; rgr-dired-rename-file-and-versions: new hack.  -- rgr, 2-Feb-99.
;;; split out of the rgr-hacks.el file.  -- rgr, 10-Sep-99.
;;; rgr-dired-rename-file-and-versions: require dired-aux.  -- rgr, 16-Jan-01.
;;; rgr-dired-do-size-sort: fix sort arg for Linux version.  -- rgr, 4-Aug-01.
;;;

;; [another gratuitous change.  -- rgr, 7-Apr-06.]

(defvar rgr-dired-desired-sort nil)
(defvar rgr-dired-last-switches nil)

(defun rgr-dired-do-size-sort ()
  ;; [debugging code.  -- rgr, 26-Feb-98.]
  '(call-process-region
    (point) (point-max) "cut" nil
    (generate-new-buffer "*dir sort debug*") nil "-c33-42,56-")
  (call-process-region
    ;; "start end program delete buffer display &rest args"
    (point) (point-max) "sort" t t nil
    ;; program args.  [***kludge***: file name is traditionally just before the
    ;; date, so we'd be much better off looking for the date & working
    ;; backwards, than hardwiring numbers that depend on the dired switches.  --
    ;; rgr, 21-Feb-98.]  [and sort version.  -- rgr, 4-Aug-01.]
    "-nr" "+4"))

(defun rgr-dired-sort-internal ()
  ;; Callback for dired sorting.
  ;; [foo bar.]
  (goto-char (point-min))
  (while (not (dired-get-filename 'no-dir t))
    (forward-line))
  (cond ((and rgr-dired-last-switches
	      (not (equal rgr-dired-last-switches dired-actual-switches)))
	  ;; The user changed the sort some other way; forget about resorting.
	  (setq rgr-dired-desired-sort nil))
	((eobp))
	((eq rgr-dired-desired-sort 'size)
	  (beginning-of-line)
	  (rgr-dired-do-size-sort))
	(t
	  (message "Unknown sort type %S ?" rgr-dired-desired-sort)))
  (setq rgr-dired-last-switches dired-actual-switches))

;;;###autoload
(defun rgr-dired-sort-by-size ()
  "In dired mode, sort files downward by file size.
Reverts the dired buffer."
  (interactive)
  (add-hook 'dired-after-readin-hook 'rgr-dired-sort-internal)
  (setq rgr-dired-last-switches dired-actual-switches)
  (setq rgr-dired-desired-sort 'size)
  (dired-revert))

;;;###autoload
(defun rgr-dired-rename-file-and-versions ()
  "Renames the current file and all versions consistently."
  (interactive)
  (require 'dired-aux)	;; for dired-collect-file-versions
  (let* ((files (dired-get-marked-files))
	 (file (car files)))
    (or (= (length files) 1)
	(error "This only works for one file at a time."))
    (let* ((versions (let ((dired-file-version-alist nil))
		       ;; [bad modularity.  -- rgr, 2-Feb-99.]
		       (dired-collect-file-versions file)
		       (cdr (car dired-file-version-alist))))
	   (version-files
	     (cons file
		   (mapcar (function (lambda (version)
			     (format "%s.~%d~" file version)))
			   versions)))
	   (new-name (dired-mark-read-file-name
		       "Rename %s and its versions to: "
		       (dired-dwim-target-directory)
		       'move nil
		       (mapcar (function dired-make-relative) files)))
	   (into-dir (file-directory-p new-name)))
      (message "Got %S versions %S, to %S" file versions new-name)
      (sit-for 2)
      (dired-create-files
        (function dired-rename-file) "Rename" version-files
	(if into-dir
	    ;; Normal rename of files into a new directory
	    (function (lambda (from)
	      (expand-file-name (file-name-nondirectory from) new-name)))
	    ;; Change name and/or type, keeping version.
	    (function (lambda (from)
	      (if (string-match "\\.~\\([0-9]+~\\)$" from)
		  (concat new-name (match-string 0 from))
		  ;; this is for the "newest" version.
		  new-name))))))))

;;; Newer stuff.

(defun dired-do-flush-spaces-internal (op-symbol file-creator operation arg
				       &optional marker-char op1
				       how-to)
  ;; based on dired-do-create-files
  (let ((fn-list (dired-get-marked-files nil arg)))
    (dired-create-files
      file-creator operation fn-list
      (function (lambda (from)
	(let ((directory (file-name-directory from))
	      (file-name (file-name-nondirectory from)))
	  (while (string-match " " file-name)
	    (setq file-name (replace-match "-" t t file-name)))
	  (expand-file-name file-name directory))))
      marker-char)))

;;;###autoload
(defun dired-do-flush-spaces (&optional arg)
  "Rename current file or all marked (or next ARG) files changing SPC to '-'."
  (interactive "P")
  (dired-do-flush-spaces-internal
    'flush-spaces (function dired-rename-file)
    "Remove spaces from" arg dired-keep-marker-rename))

;;; Tail ends.

;;;###autoload
(defun rgr-dired-load-hook ()
  ;; new hack  -- rgr, 2-Feb-99.
  (define-key dired-mode-map "\C-cr" 'rgr-dired-rename-file-and-versions)
  ;; some still newer hacks.  -- rgr, 13-Sep-04.
  (define-key dired-mode-map "\C-c " 'dired-do-flush-spaces)
  (define-key dired-mode-map "\C-ca" 'vm-dired-attach-file)
  (define-key dired-mode-map "U" 'browse-url-of-dired-file))

(provide 'rgr-dired)
