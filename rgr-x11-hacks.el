;;;; Playing with frames under X11.
;;;
;;;    Modification history:
;;;
;;; created.  -- rgr, 20-Aug-01.
;;;

(defvar rgr-interesting-frame-heights '(58 43 28))
(defvar rgr-x11-preferred-font-name
	"-adobe-courier-medium-r-normal--*-180-*-m-*-iso8859-15"
  "*Font name pattern that matches what we want to install by default.")

(defun rgr-window-frame (&optional window)
  ;; If given no args, this effectively returns the "current" frame, i.e. the
  ;; frame of the current window.
  (window-frame (or window (next-window))))

;;;###autoload
(defun rgr-toggle-frame-height ()
  (interactive)
  (let ((new-height
	 (or (car (cdr (member (frame-height) rgr-interesting-frame-heights)))
	     (car rgr-interesting-frame-heights))))
    (message "Changing frame height to %d." new-height)
    (set-frame-height (rgr-window-frame) new-height)))

;; (global-set-key [f5] 'rgr-toggle-frame-height)

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
	 (system (let ((name (system-name)))
		   (if (string-match "\\." name)
		       (substring name 0 (match-beginning 0))
		       name)))
	 (label (format "%s %s%s%s@%s"
			(if (eq rgr-emacs-flavor 'fsf)
			    'emacs
			    rgr-emacs-flavor)
			(if (equal version "21.2")
			    ;; don't show the standard version.
			    ""
			    ;; add spacing.
			    (concat version " "))
			(if su-p "[su] " "")
			real-login-name system)))
      (if (eq rgr-emacs-flavor 'xemacs)
	  (setq frame-title-format label)
	  (modify-frame-parameters (selected-frame)
				   (list (cons 'name label)
					 (cons 'mouse-color
					       (if su-p "blue" "red")))))))

(defun rgr-install-x11-font ()
  ;; [this is a real crock; i have no idea why .xresources doesn't work.  --
  ;; rgr, 26-May-03.]
  (save-excursion
    (set-buffer (get-buffer-create " *font work*"))
    (erase-buffer)
    (call-process "xlsfonts" nil t nil
		  "-fn" rgr-x11-preferred-font-name)
    (goto-char (point-min))
    (if (looking-at "^-")
	;; must be a real font spec.
	(set-frame-font (buffer-substring (point)
					  (progn (end-of-line)
						 (point))))
	(message "Can't find desired font; ignoring."))))

;;;###autoload
(defun rgr-install-x11-hacks ()
  (require 'rgr-mouse)
  (rgr-install-mouse-commands)
  (rgr-install-frame-properties)
  (rgr-install-x11-font)
  ;; [this puts the props on the old frame.  -- rgr, 19-Oct-98.]
  ;; (add-hook 'after-make-frame-hook 'bmerc-install-frame-properties)
  (cond ((rgr-emacs-version-p 19 30)
	  (global-set-key [?\C-\.] 'ilisp-next-possibility)
	  ;; Do this only for window systems; it disables function keys on ASCII
	  ;; terminals.  -- rgr, 5-Apr-96.  [actually, it doesn't if the
	  ;; function-key-map is set correctly.  -- rgr, 6-Feb-98.]  [never
	  ;; mind; I'm now used to typing M-{ and M-} instead.  -- rgr,
	  ;; 19-Oct-98.]
	  ;; (global-set-key "\M-[" 'backward-paragraph)
	  ;; (global-set-key "\M-]" 'forward-paragraph)
	  ;; [not sure if this autoloads . . .  -- rgr, 27-Nov-95.]  [it didn't,
	  ;; but I seem to have taken care of that in the mean time.  -- rgr,
	  ;; 4-Apr-96.]
	  (global-set-key [?\C-x ?\C-\;] 'rgr-comment-region-lisp))))

(provide 'rgr-x11-hacks)
