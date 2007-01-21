;;;; Diff hacks.
;;;
;;; This file is based partly on Discus (DIstributed Source Code Update System),
;;; a tool for maintaining symmetrical directories on different systems, and
;;; partly on the GNU emacs diff command.
;;;
;;; This program is free software; you can customize it under the terms of the
;;; GNU General Public License as published by the Free Software Foundation;
;;; either version 1, or (at your option) any later version.  But, as this is a
;;; test version (still), **do not redistribute it** yourself.  Ask the author
;;; for the latest version.
;;;
;;; This program is distributed in the hope that it will be useful, but WITHOUT
;;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;;; more details.
;;;
;;; You should have received a copy of the GNU General Public License along with
;;; GNU emacs; if not, write to the Free Software Foundation, Inc., 675 Mass
;;; Ave, Cambridge, MA 02139, USA.
;;;
;;; $Id$

(require 'diff)

(defun rgr-diff-internal (old new &optional switches new-file-is-temp-p)
  ;; based on the discus-diff function, with a few tricks from the diff
  ;; function.
  (let* ((old-alt (file-local-copy old))
	 ;; (compilation-process-setup-function 'diff-process-setup)
	 ;; Prefer explicitly specified switches.
	 (args (append (let ((switches (or switches diff-switches)))
			 (if (listp switches)
			     switches
			     (list switches)))
		       (list (or old-alt old) new))))
    (save-excursion
      (set-buffer (get-buffer-create "*diff*"))
      (toggle-read-only -1)
      (erase-buffer)
      (display-buffer (current-buffer) t)
      (apply (function call-process) diff-command nil (current-buffer) t args)
      (if old-alt
	  (delete-file old-alt))
      (if new-file-is-temp-p
	  ;; Replace the "new" file name with the old file name in the output.
	  ;; Since the new file is a temporary file, this is necessary to get
	  ;; diff-mode commands to operate on the correct file.  [bug:  this
	  ;; regexp only works for "diff -u".  -- rgr, 24-Aug-05.]
	  (save-excursion
	    (goto-char (point-min))
	    (while (re-search-forward (concat "^\\+\\+\\+ "
					      (regexp-quote new) "\t")
				      nil t)
	      (replace-match (concat "+++ " old "\t") t t))))
      (diff-mode)
      (current-buffer))))

;;;###autoload
(defun rgr-quick-source-compare ()
  "Run diff on a buffer and the disk version of the file."
  (interactive)
  (let* ((file (or (buffer-file-name)
		   (error "Not a file buffer.")))
	 (tmp-dir (or temporary-file-directory (getenv "TMPDIR")
		      (getenv "TMP") "/tmp"))
	 (tmp (make-temp-name (expand-file-name "rgr-diff-" tmp-dir))))
    (unwind-protect
	(progn (save-restriction
		 (widen)
		 (write-region (point-min) (point-max) tmp))
	       (rgr-diff-internal file tmp nil t))
      (condition-case ignore
	  (delete-file tmp)
	(error nil)))))

;;;###autoload
(defun rgr-diff-patch-directory (dir)
  "In a patch-mode buffer, apply it to a specified directory."
  (interactive "DDirectory to patch: ")
  (let ((patch-file (expand-file-name (format "foo-%d.patch" (emacs-pid)) dir)))
    (write-region (point-min) (point-max) patch-file nil nil nil 'excl)
    (save-excursion
      (set-buffer (get-buffer-create "*patch*"))
      (setq default-directory dir)
      ;; (message "in %S" default-directory)
      (toggle-read-only -1)
      (erase-buffer)
      (display-buffer (current-buffer) t)
      (call-process "/bin/bash" patch-file t t "-c" "patch -p0")
      (toggle-read-only 1))
    (delete-file patch-file)))

;;;###autoload
(defun rgr-diff-mode-hook ()
  (define-key diff-mode-map "\C-cp" 'rgr-diff-patch-directory)
  (define-key diff-mode-map "\C-c!" 'rgr-diff-add-definition-comment))

;;;###autoload
(defun rgr-install-diff-hacks ()
  (global-set-key "\C-c=" 'rgr-quick-source-compare)
  (global-set-key "\C-c!" 'rgr-add-definition-comment)
  (add-hook 'diff-mode-hook 'rgr-diff-mode-hook))

(provide 'rgr-diff-hacks)
