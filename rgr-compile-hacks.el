;;;****************************************************************************
;;;
;;;    GNU Emacs compile-mode hackery.
;;;
;;;    compile mode consists of support for M-x grep and M-x compile commands.
;;; We add a few extra tricks.
;;;
;;;    To compile this without error, do the following:
;;;
;;;	  (mapcar 'require '(compile))
;;;
;;;    Modification history:
;;;
;;; [created, with the cdgrep command.  -- rgr, 4-Apr-94.]
;;;
;;; $Id$

(eval-when-compile
  (require 'rgr-lisp-hacks)
  (require 'compile))

(require 'compile)

(defvar compilation-buffer-compile-command nil)

;;;; rgr-grep-summarize-hits

(defvar rgr-grep-summarize-hits-command-string
	"perl -ne 'print \"$1\\n\" if /^([^ :]+):/;' | uniq -c"
  "Shell command to summarize the hits in a grep (compilation) buffer.")

;;;###autoload
(defun rgr-grep-summarize-hits ()
  "Display a summary of hits from the last \\[grep] command."
  (interactive)
  ;; this is based on shell-command-on-region
  (let ((buffer nil))
    (save-excursion
      (set-buffer (compilation-find-buffer))
      (setq buffer (get-buffer-create (concat (buffer-name) " summary")))
      ;; Clear the output buffer, then run the command with output there.
      (save-excursion
	(set-buffer buffer)
	(setq buffer-read-only nil)
	(erase-buffer))
      (call-process-region (point-min) (point-max) shell-file-name
			   nil buffer nil
			   shell-command-switch
			   rgr-grep-summarize-hits-command-string))
    (set-window-start (display-buffer buffer) 1)))

;;;; rgr-recompile

(defun rgr-remember-compile-command ()
  (set (make-local-variable 'compilation-buffer-compile-command)
       compile-command))

;; This is sufficiently benign that we can do it unconditionally.  -- rgr,
;; 26-Nov-96.
(add-hook 'compilation-mode-hook 'rgr-remember-compile-command)

(defun rgr-find-last-compilation-buffer (dir)
  "Find the last compilation-mode buffer associated with dir.
Should be consolidated with rgr-recompile code."
  ;; [***bug***: this doesn't work; buffer-list isn't ordered right.  -- rgr,
  ;; 5-Sep-96.]
  (let ((tail (buffer-list)) (compilation-buffer nil))
    (save-excursion
      (while tail
	(let ((buffer (car tail)))
	  (set-buffer buffer)
	  (if (and (eq major-mode 'compilation-mode)
		   (not (string-match "grep" (buffer-name)))
		   (equal dir default-directory))
	      (setq compilation-buffer buffer
		    tail nil)))
	(setq tail (cdr tail))))
    compilation-buffer))

;;;###autoload
(defun rgr-recompile (&optional reprompt-p)
  "Redo the last compile command involving the current directory,
without asking any questions."
  (interactive "P")
  (let* ((dir default-directory)
	 (compilation-buffer (rgr-find-last-compilation-buffer dir)))
    (if compilation-buffer
	(let ((command nil))
	  (save-excursion
	    ;; Get the command to redo.
	    (set-buffer compilation-buffer)
	    (setq command (or compilation-buffer-compile-command
			      ;; hack.
			      compile-command))
	    (if reprompt-p
		;; this is taken from the compile command's interactive spec.
		(setq command (read-from-minibuffer "Compile command: "
						    command nil nil
						    '(compile-history . 1))))
	    ;; Save buffers in this directory.
	    (let ((tail (buffer-list))
		  (pattern (concat "^" (regexp-quote (expand-file-name dir)))))
	      (while tail
		(set-buffer (car tail))
		(if (and (buffer-modified-p)
			 (not (buffer-base-buffer))
			 buffer-file-name
			 (string-match pattern buffer-file-name))
		    (save-buffer))
		(setq tail (cdr tail)))))
	  ;; Recompile (copied from the body of the compile command).
	  (let ((default-directory dir))
	    (save-excursion
	      (setq compile-command command)
	      (compilation-start compile-command))))
	;; Invoke compile afresh.
	(compile (if (or compilation-read-command current-prefix-arg)
		     (read-from-minibuffer "Compile command: "
					   compile-command nil nil
					   '(compile-history . 1))
		     compile-command)))))

;;;; Tag ends.

;;;###autoload
(defun rgr-compilation-mode-hook ()
  ;; Customize the window height.  This is 1/4 of the frame, but never less than
  ;; six lines.  [stolen from the rgr-rmail-mode-hook version.  -- rgr,
  ;; 10-Oct-00.]
  (setq compilation-window-height (max (/ (frame-height) 4) 6)))

(provide 'rgr-compile-hacks)
