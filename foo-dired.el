;;;; rgr-dired stuff
;;;
;;;    Useful stuff in here is rgr-dired-rename-file-and-versions and
;;; rgr-dired-sort-by-size (but the latter is buggy).
;;;

;; [another gratuitous change.  -- rgr, 7-Apr-06.]

(defvar rgr-dired-desired-sort nil)
(defvar rgr-dired-last-switches nil)

;; sigh.  -- rgr, 10-Apr-06.

;; ackety, yackety.  -- rgr, 27-May-06.

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

;; barf.  barf, barf.

;; (setq file (directory-file-name default-directory))
;; (vc-cvs-registered file)
;; (vc-file-getprop (directory-file-name default-directory) 'vc-backend)
