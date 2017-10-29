;;;; Playing with frames under X11.
;;;
;;; [created.  -- rgr, 20-Aug-01.]
;;;

(defun rgr-install-frame-properties ()
  ;; Get a decent label and background color.
  (let* ((version
	  (cond ((or (and (= emacs-major-version 25)
			  (or (= emacs-minor-version 2)
			      (= emacs-minor-version 3)))
		     (and (= emacs-major-version 26))
		     (and (= emacs-major-version 27)))
		  ;; don't show the standard version(s).
		  nil)
		;; In the versions "19.34.1" and "24.0.50.1", the last component
		;; is not significant (it's the number of times I recompiled it
		;; before I got it right).  -- rgr, 19-Oct-98.
		((string-match "^\\([0-9.]+\\)\\.[0-9]+$" emacs-version)
		  (match-string 1 emacs-version))
		;; Fallback.
		(t emacs-version)))
	 (real-login-name (user-real-login-name))
	 (su-p (not (equal real-login-name
			   ;; SUDO_USER is also defined by kdesu (e.g.).
			   (or (getenv "SUDO_USER") (user-login-name)))))
	 (ssh-p (getenv "SSH_CONNECTION"))
	 (user-at-host
	   (and (or su-p ssh-p)
		(let ((name (system-name)))
		  (format "%s@%s"
			  real-login-name
			  (if (string-match "\\." name)
			      (substring name 0 (match-beginning 0))
			    name)))))
	 (label (if (and version user-at-host)
		    (concat version " " user-at-host)
		    (or version user-at-host "emacs")))
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

;;;###autoload
(defun rgr-install-x11-hacks ()
  (require 'rgr-mouse)
  (rgr-install-mouse-commands)
  (rgr-install-frame-properties)
  (or (when (and (= emacs-major-version 24)
		 (>= emacs-minor-version 3))
	;; Presumably this is only works for GTK/Freetype, but I don't know how
	;; to check for that.  -- rgr, 9-Mar-14.
	(condition-case ()
	    (progn (set-frame-font "DejaVu Sans Mono 9")
		   t)
	  (error nil)))
      (when (>= emacs-major-version 25)
	(set-frame-font "Efont Fixed 10" nil t)
	t)
      (message "Failed to change the font"))
  (global-set-key [?\C-\.] 'ilisp-next-possibility)
  ;; Bind comment-region globally.  (This is an X11 hack because it is too hard
  ;; to type otherwise.)
  (global-set-key [?\C-x ?\C-\;] 'comment-region))

(provide 'rgr-x11-hacks)
