
;;;; Playing with frames under X11.
;;;
;;; [created.  -- rgr, 20-Aug-01.]
;;;
;;; $Id$

(defun rgr-install-frame-properties ()
  ;; Get a decent label and a more visible mouse.  The mouse cursor is red if
  ;; you are running "su", and blue otherwise.
  (let* ((version
	  (cond ((or (and (= emacs-major-version 24)
			  (or (= emacs-minor-version 2)
			      (= emacs-minor-version 3))))
		  ;; don't show the standard version(s).
		  "")
		;; In the versions "19.34.1" and "24.0.50.1", the last component
		;; is not significant (it's the number of times I recompiled it
		;; before I got it right).  -- rgr, 19-Oct-98.
		((string-match "^\\([0-9.]+\\)\\.[0-9]+$" emacs-version)
		  (concat " " (match-string 1 emacs-version)))
		;; Fallback.
		(t (concat " " emacs-version))))
	 (real-login-name (user-real-login-name))
	 (su-p (not (equal real-login-name
			   ;; SUDO_USER is also defined by kdesu (e.g.).
			   (or (getenv "SUDO_USER") (user-login-name)))))
	 (ssh-p (getenv "SSH_CONNECTION"))
	 (label (concat "emacs" version
			(if (or su-p ssh-p)
			    (let ((name (system-name)))
			      (format " %s@%s"
				      real-login-name
				      (if (string-match "\\." name)
					  (substring name 0 (match-beginning 0))
					  name)))
			    "")))
	 (background
	   ;; fix lame color scheme under KDE on SuSE 9.0.  -- rgr, 13-Mar-04.
	   ;; [actually, let's make this the default.  -- rgr, 20-Mar-04.]
	   (cond ((zerop (user-real-uid))
		   ;; use something distinctive for root.  -- rgr, 13-Mar-04.
		   "light cyan")
		 ;; use something more bland for normal users.
		 ((eq rgr-site 'mgi)
		   ;; this is a tad lighter than "DarkSeaGreen1".
		   "#e8ffe8")
		 (t
		   ;; home site.  [this was "linen", but it needed to be a bit
		   ;; darker.  -- rgr, 10-Nov-12.]
		   "#dad0c6"))))
    (modify-frame-parameters (selected-frame)
			     (list (cons 'name label)
				   (cons 'background-color background)
				   (cons 'foreground-color "black")))))

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

(defun rgr-x11-install-nondefault-fontset ()
  (let ((font nil))
    (dolist (fontset (fontset-list))
      (or (string-match "fontset-default$" fontset)
	  (string-match "fontset-auto[0-9]+$" fontset)
	  (setq font fontset)))
    (when font
      ;; [this is all magic from the menu-set-font fn.  -- rgr, 3-Feb-10.]
      (set-face-attribute 'default (selected-frame)
			  :width 'normal
			  :weight 'normal
			  :slant 'normal
			  :font font)
      (let ((font-object (face-attribute 'default :font)))
	(dolist (f (frame-list))
	  (and (not (eq f (selected-frame)))
	       (display-graphic-p f)
	       (set-face-attribute 'default f :font font-object)))
	(set-face-attribute 'default t :font font-object))
      (let ((spec (list (list t (face-attr-construct 'default)))))
	(put 'default 'customized-face spec)
	(custom-push-theme 'theme-face 'default 'user 'set spec))
      (put 'default 'face-modified nil))))

;;;###autoload
(defun rgr-install-x11-hacks ()
  (require 'rgr-mouse)
  (rgr-install-mouse-commands)
  (rgr-install-frame-properties)
  (or (and (= emacs-major-version 24)
	   (>= emacs-minor-version 3)
	   ;; Presumably this is only works for GTK/Freetype, but I don't know
	   ;; how to check for that.  -- rgr, 9-Mar-14.
	   (condition-case ()
	       (progn (set-frame-font "DejaVu Sans Mono 9")
		      t)
	     (error nil)))
      (and (fboundp 'custom-push-theme)
	   ;; [this only seems to be a problem on openSUSE 13.1.  -- rgr,
	   ;; 11-Dec-13.]
	   (condition-case ()
	       (progn (rgr-x11-install-nondefault-fontset)
		      t)
	     (error nil)))
      (message "Failed to change the font"))
  (global-set-key [?\C-\.] 'ilisp-next-possibility)
  ;; Bind comment-region globally.  (This is an X11 hack because it is too hard
  ;; to type otherwise.)
  (global-set-key [?\C-x ?\C-\;] 'comment-region))

(provide 'rgr-x11-hacks)
