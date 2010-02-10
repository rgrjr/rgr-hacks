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

(eval-when-compile
  (require 'diff)
  (require 'diff-mode))

;;;###autoload
(defun rgr-diff-patch-directory (dir)
  "In a patch-mode buffer, apply it to a specified directory."
  (interactive "DDirectory to patch: ")
  (if (not (file-directory-p dir))
      (error "Must have a directory."))
  (let* ((dir (file-name-as-directory dir))
	 ;; [bug: predictable file name.  -- rgr, 15-Feb-07.]
	 (patch-file (expand-file-name (format "foo-%d.patch" (emacs-pid))
				       dir)))
    (write-region (point-min) (point-max) patch-file nil nil nil 'excl)
    (with-current-buffer (get-buffer-create "*patch*")
      (setq default-directory dir)
      ;; (message "in %S" default-directory)
      (toggle-read-only -1)
      (erase-buffer)
      (display-buffer (current-buffer) t)
      (call-process "/bin/bash" patch-file t t "-c" "patch -p0")
      (toggle-read-only 1))
    (delete-file patch-file)))

(defun rgr-diff-run-diffstat ()
  "Run `diffstat' on the current buffer."
  (interactive)
  (shell-command-on-region (point-min) (point-max) "diffstat"))

(defun rgr-diff-hunk-next (&optional count)
  "Move to the next hunk, and show it at the top of the window."
  (interactive "P")
  (diff-hunk-next count)
  (recenter 0))

(defun rgr-diff-hunk-prev (&optional count)
  "Move to the previous hunk, and show it at the top of the window."
  (interactive "P")
  (diff-hunk-prev count)
  (recenter 0))

(defun rgr-diff-goto-vc-file ()
  "Go to the current file in a vc-dir buffer."
  (interactive)
  (let ((current-file
	  (or (expand-file-name (diff-find-file-name))
	      (error "No current file.")))
	(vc-dir-buffer nil) (vc-dir-default-directory-len 0)
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
    ;; Go to it.
    '(message "got %S in %S" vc-dir-node vc-dir-buffer)
    (cond ((not vc-dir-buffer)
	    ;; Totally failed, so offer to start vc-dir.
	    (call-interactively 'vc-dir))
	  ((not vc-dir-node)
	    (switch-to-buffer-other-window vc-dir-buffer)
	    (ewoc-goto-node vc-ewoc (ewoc-nth vc-ewoc 0))
	    (vc-dir-move-to-goal-column)
	    (message "File is under this buffer, but not visible."))
	  (t
	    (switch-to-buffer-other-window vc-dir-buffer)
	    (ewoc-goto-node vc-ewoc vc-dir-node)
	    (vc-dir-move-to-goal-column)))))

;;;###autoload
(defun rgr-diff-mode-hook ()
  (define-key diff-mode-map "\C-cp" 'rgr-diff-patch-directory)
  (define-key diff-mode-map "\C-c!" 'rgr-diff-add-definition-comment)
  (define-key diff-mode-map "\C-cd" 'rgr-diff-run-diffstat)
  (define-key diff-mode-map "\C-cf" 'rgr-diff-goto-vc-file)
  (define-key diff-mode-map "\M-P" 'rgr-diff-hunk-prev)
  (define-key diff-mode-map "\M-N" 'rgr-diff-hunk-next))

;;;###autoload
(defun rgr-install-diff-hacks ()
  (global-set-key "\C-c=" 'diff-buffer-with-file)
  (global-set-key "\C-c!" 'rgr-add-definition-comment)
  (add-hook 'diff-mode-hook 'rgr-diff-mode-hook))

(provide 'rgr-diff-hacks)
