;;;; Playing with frames under X11.
;;;
;;; [created.  -- rgr, 20-Aug-01.]
;;;
;;; $Id$

(defun rgr-install-frame-properties ()
  ;; Get a decent label and a more visible mouse.  The mouse cursor is red if
  ;; you are running "su", and blue otherwise.  [shouldn't this go on a frame
  ;; creation hook?  -- rgr, 8-Nov-95.]
  ;; [this was bmerc-install-frame-properties (in bmerc-hacks.el), but that made
  ;; it unavailable to xemacs, and the rest of the bmerc-hacks.el file is no
  ;; longer useful anyway.  -- rgr, 26-Jul-01.]
  (let* ((version
	  ;; In the version (e.g.) "19.34.1", the last component is not
	  ;; significant (it's the number of times I recompiled it before I got
	  ;; it right).  -- rgr, 19-Oct-98.
	  (format "%s.%s" rgr-emacs-major-version rgr-emacs-minor-version))
	 (real-login-name (user-real-login-name))
	 (su-p (not (equal real-login-name (user-login-name))))
	 (ssh-p (getenv "SSH_CONNECTION"))
	 (label (concat (if (eq rgr-emacs-flavor 'fsf)
			    "emacs"
			    (symbol-name rgr-emacs-flavor))
			(if (string-match "^22\\.2" version)
			    ;; don't show the standard version(s).
			    ""
			    ;; add spacing.
			    (concat " " version))
			(if (or su-p ssh-p)
			    (let ((name (system-name)))
			      (format " %s@%s"
				      real-login-name
				      (if (string-match "\\." name)
					  (substring name 0 (match-beginning 0))
					  name)))
			    ""))))
    (if (eq rgr-emacs-flavor 'xemacs)
	(setq frame-title-format label)
	(modify-frame-parameters (selected-frame)
				 (list (cons 'name label)
				       ;; [this doesn't seem to work in 20.3.
				       ;; -- rgr, 21-Mar-04.]
				       (cons 'mouse-color
					     (if su-p "blue" "red")))))))

(defun rgr-x11-kill-ring-save (beg end)
  "Force X11 cut buffer save, even if interprogram-cut-function is disabled."
  (interactive "r")
  (let ((interprogram-cut-function 'x-select-text))
    (copy-region-as-kill beg end)
    (message "%d bytes saved" (1+ (- end beg)))))

(defun rgr-x11-kill-ring-yank (&optional arg)
  "Force X11 cut buffer yank, even if interprogram-paste-function is disabled."
  (interactive "*P")
  (let ((interprogram-paste-function 'x-cut-buffer-or-selection-value))
    (yank arg)))

;;;###autoload
(defun rgr-install-x11-hacks ()
  (require 'rgr-mouse)
  (rgr-install-mouse-commands)
  (rgr-install-frame-properties)
  (global-set-key [?\C-\.] 'ilisp-next-possibility)
  ;; [not sure if this autoloads . . .  -- rgr, 27-Nov-95.]  [it didn't, but I
  ;; seem to have taken care of that in the mean time.  -- rgr, 4-Apr-96.]
  (global-set-key [?\C-x ?\C-\;] 'rgr-comment-region-lisp))

(provide 'rgr-x11-hacks)
