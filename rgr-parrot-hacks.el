;;;; Tools for hacking Parrot.
;;;
;;; [created.  -- rgr, 24-May-08.]
;;;
;;; $Id$

;;;###autoload
(defun rgr-write-parrot-patch-file (&optional file-name)
  "In a *vc-diff* buffer or patch file, add a notation to patches.tbl
for a Parrot patch.  If the patch has already been committed (as determined
by looking for a revision comment), adds the \"ci\" and revision notation
to the patches.tbl buffer."
  (interactive
    (list (or buffer-file-name
	      (let ((default-directory
		      (expand-file-name "~/projects/meta-parrot/")))
		(read-file-name "Write patch file: " default-directory
				(expand-file-name
				  (file-name-nondirectory (buffer-name))
				  default-directory)
				nil nil)))))
  (if (not (and buffer-file-name
		(equal file-name buffer-file-name)))
      (write-file file-name nil))
  ;; Extract revision info.
  (let ((rev-string (save-excursion
		      (goto-char (point-min))
		      (and (re-search-forward "revision: \\([0-9]+\\);" nil t)
			   (match-string-no-properties 1))))
	(file-name (and (string-match "[^/]+$" buffer-file-name)
			(match-string 0 buffer-file-name)
			(buffer-name)))
	;; format is "dd-Mon-yy", with zero padding.
	(date-string (format-time-string "%d-%b-%y"))
	(patches-buf (find-file-noselect "~/projects/meta-parrot/patches.tbl")))
    ;; (message "got rev %S" rev-string)
    (with-current-buffer patches-buf
      (goto-char (point-max))
      (insert (if rev-string "ci" "")
	      "\t"
	      date-string
	      "\t"
	      file-name
	      "\t"
	      (or rev-string "")
	      "\n")
      (save-buffer))))

(provide 'rgr-parrot-hacks)
