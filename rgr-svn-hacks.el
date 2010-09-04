;;;****************************************************************************
;;;
;;;;   Subversion hackery.
;;;
;;; [created.  -- rgr, 16-Aug-10.]
;;;
;;; $Id: emacs-init.el 539 2010-07-16 17:02:57Z rgr $

(defun vc-svn-find-root-internal ()
  ;; Recursive grinder for vc-svn-root; expects to be in a scratch buffer, with
  ;; default-directory as the directory to test.  Returns nil on failure
  ;; (i.e. we've gone too far), or the top-level working-copy directory,
  ;; according to the vc-svn-root heuristic.
  (erase-buffer)
  (call-process "svn" nil t nil "info")
  (goto-char (point-min))
  (cond ((re-search-forward "not a working copy" nil t)
	  nil)
	((re-search-forward "^URL: *.*/\\([^/]+\\)$")
	  ;; We are in an SVN working copy; compare the dir name to the URL.
	  ;; Don't compare the full URLs because it is sometimes useful to
	  ;; "graft" a subdirectory from a branch.
	  (let* ((url-dir-name (match-string 1))
		 (working-dir-name
		   (if (string-match "\\([^/]+\\)/$" default-directory)
		       (match-string 1 default-directory)
		       ;; Must be at the root.
		       "")))
	    (if (equal url-dir-name working-dir-name)
		;; Directory repo name matches WC name, so this may not be it.
		(or (let ((default-directory
			    ;; Move up to the parent directory.
			    (file-name-directory
			      (directory-file-name default-directory))))
		      (vc-svn-find-root-internal))
		    ;; Parent directory is not under SVN.
		    default-directory)
		;; Parent directory belongs to another repo tree.
		default-directory)))
	(t
	  ;; Error detected by "svn info"?
	  nil)))

;;;###autoload
(defun vc-svn-root (&optional from-file)
  ;; Returns a plausible root directory for SVN.  Since SVN does not have a
  ;; formal notion of "root directory", we use the heuristic that a directory is
  ;; the root if (a) its parent is not under SVN control, or (b) the directory
  ;; name does not match the repo directory name.  "svn info" is called multiple
  ;; times to check this.
  (let ((default-directory (if from-file
			       (file-name-directory from-file)
			       default-directory)))
    (with-temp-buffer
      (vc-svn-find-root-internal))))

;; decache.
(put 'SVN 'vc-functions nil)

(provide 'rgr-svn-hacks)
