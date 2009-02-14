;;;*****************************************************************************
;;;
;;;;   Terminal setups.
;;;
;;;    See the rgr-term-setup function below; this is the only entry point.
;;; Sets up terminal type & installs goodies based on the value of the TERM
;;; environment variable.
;;;
;;; $Id$

(defun rgr-dialup-term-setup ()
  "Common terminal setup tasks."
  ;; Economize redraw time.  -- rgr, 14-Feb-96.
  (setq split-window-keep-point nil)
  ;; Likewise, don't waste serial line bandwidth on line number updates.  This
  ;; is important mostly in *cmulisp* and *shell* buffers, but it's not worth
  ;; making buffer-local bindings to shut them off.  -- rgr, 15-Mar-97.
  (setq line-number-mode nil)
  ;; Override mark-sexp command on C-M-Space (which is sent as M-C-@ by most
  ;; terminal emulators).
  (global-set-key "\e\C-@" 'rgr-exchange-point-and-mark))

(defun rgr-vt220-term-setup ()
  ;;  QVT/Term under Win9x.  unfortunately, you can't reprogram the space bar to
  ;;  teach it to do C-space, etc.  may need to put these on function keys.  --
  ;;  rgr, 8-Jul-99.
  (global-set-key "\C-c " 'rgr-set-mark-command)
  ;; [also use C-x SPC, since C-c SPC is shadowed in html-helper-mode.  -- rgr,
  ;; 26-Nov-99.]
  (global-set-key "\C-x " 'rgr-set-mark-command)
  (global-set-key "\C-c\e " 'rgr-exchange-point-and-mark))

(defun rgr-xterm-term-setup ()
  ;; nothing useful yet, but it suppresses the warning message.  -- rgr,
  ;; 10-Sep-99.
  )

(defun rgr-setfont ()
  "Run 'setfont' without any args in order to re-establish the console font.
In openSUSE 11.0 on rgr, switching to an X11 session garbles the
font, and this seems to be the only way to put it back.  Note
that the 'setfont' command will only work if you are root."
  (interactive)
  (shell-command-on-region (point) (point) "setfont" "*setfont output*"))

(defun rgr-linux-term-setup ()
  ;; Set up 
  (global-set-key [f9] 'rgr-setfont))

;;;###autoload
(defun rgr-term-setup ()
  ;; This isn't really a terminal setup as used by term-setup-hook, since it
  ;; mostly defines key bindings based on what's easy to type on a given
  ;; keyboard.  See also the rgr-install-x11-hacks function.  -- rgr, 11-Dec-95.
  (let* ((term (getenv "TERM"))
	 (setup-function
	   (intern (concat "rgr-" (or term "dumb") "-term-setup"))))
    ;; Do this first, so that terminal-specific functions can override.
    (rgr-dialup-term-setup)
    ;; Call the terminal-specific setup function (if defined).
    (cond ((fboundp setup-function)
	    (funcall setup-function))
	  (t
	    (message "No %s function defined for terminal type %s"
		     setup-function term)
	    (sit-for 2)))))

(provide 'rgr-term-setup)
