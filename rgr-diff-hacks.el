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

(defun rgr-diff-internal (old new &optional switches)
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
      (diff-mode)
      (current-buffer))))

;;;###autoload
(defun rgr-quick-source-compare ()
  "Run diff on a buffer and the disk version of the file."
  (interactive)
  (let ((file (or (buffer-file-name)
		  (error "Not a file buffer.")))
	(tmp (make-temp-name "/tmp/rgr-diff-"))
	(result nil) (finished-p nil))
    (unwind-protect
	(progn (save-restriction
		 (widen)
		 (write-region (point-min) (point-max) tmp))
	       (setq result (rgr-diff-internal file tmp))
	       (setq finished-p t))
      ;; If rgr-diff-internal returned, then we can (and must) rely on the
      ;; compilation-finish-function to clean up the temp file.
      (if (not finished-p)
	  (condition-case ignore
	      (delete-file tmp)
	    (error nil))))
    result))

;;;###autoload
(defun rgr-diff-mode-hook ()
  (define-key diff-mode-map "\C-c!" 'rgr-diff-add-definition-comment))

;;;###autoload
(defun rgr-install-diff-hacks ()
  (global-set-key "\C-c=" 'rgr-quick-source-compare)
  (add-hook 'diff-mode-hook 'rgr-diff-mode-hook))

(provide 'rgr-diff-hacks)
