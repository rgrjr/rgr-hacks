;;;*****************************************************************************
;;;
;;;;   Terminal setups.
;;;
;;;    See the rgr-term-setup function below; this is the only entry point.
;;; Sets up terminal type & installs goodies based on the value of the TERM
;;; environment variable.
;;;
;;;    Modification history:
;;;
;;; . . .
;;; rgr-term-setup: ncsa telnet support.  -- rgr, 28-Mar-96.
;;; rgr-term-setup: no mail peeker when su.  -- rgr, 30-Mar-96.
;;; rgr-term-setup: telnet function key hacking.  -- rgr, 31-Mar-96.
;;; rgr-term-setup: more telnet function key bindings.  -- rgr, 1-Apr-96.
;;; remodularized rgr-install-window-system-hacks and rgr-term-setup; now
;;;	data-driven.  -- rgr, 4-Apr-96.
;;; rgr-vt200-term-setup: update for emacs 19.30.  -- rgr, 5-Apr-96.
;;; rgr-vt200-term-setup: IT 19.31 kludge.  -- rgr, 1-Jun-96.
;;; rgr-term-setup: kill line-number-mode.  -- rgr, 15-Mar-97.
;;; rgr-paper-over-broken-ncsa-telnet-function-keys: new.  -- rgr, 6-Feb-98.
;;; rgr-vt200-term-setup: update for 19.34.  -- rgr, 22-Oct-98.
;;; split out of rgr-hacks.el, move ffap stuff here.  -- rgr, 28-Oct-98.
;;; rgr-vt100-term-setup: kludge for mcclintock.  -- rgr, 18-Dec-98.
;;; don't put find-file-at-point on C-x C-f.  -- rgr, 23-Jan-99.
;;; rgr-dialup-term-setup: start after 1 minute.  -- rgr, 13-Feb-99.
;;; rgr-vt200-term-setup: fn keys still broke on SunOS.  -- rgr, 17-Feb-99.
;;; rgr-dialup-term-setup: reorganize; call this always.  -- rgr, 18-Feb-99.
;;; rgr-vt100-hack-ie: new.  -- rgr, 7-Jul-99.
;;; rgr-vt100-hack-ie: fix typos.  -- rgr, 8-Jul-99.
;;; rgr-vt220-term-setup: QVT/Term under Win9x.  -- rgr, 8-Jul-99.
;;; rgr-xterm-term-setup stub.  -- rgr, 10-Sep-99.
;;; rgr-vt220-term-setup: C-x SPC for rgr-set-mark-command.  -- rgr, 26-Nov-99.
;;; rgr-vt102-term-setup: NiftyTelnet 1.3 SSH 1.1 patch.  -- rgr, 29-Dec-00.
;;; flush obsolete rgr-supdup-term-setup fn.  -- rgr, 13-Apr-03.
;;;

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
  (global-set-key "\e\C-@" 'rgr-exchange-point-and-mark)
  ;; Use find-file-at-point instead of find-file.  [may want to conditionalize
  ;; this on having no mouse.  -- rgr, 11-Jul-95.]  [actually, it doesn't even
  ;; load under emacs 18.  -- rgr, 28-Jul-95.]  [more pain than it's worth,
  ;; except when in a terminal.  -- rgr, 15-Nov-95.]  [config problem; not
  ;; available in versions prior to 19.31.  -- rgr, 23-Mar-95.]  [fixed for
  ;; 19.30.  -- rgr, 17-Jun-98.]  [actually, i like it better with ffap on C-x
  ;; C-M-f, which is consistent between X11 and terminals.  -- rgr, 23-Jan-99.]
  '(cond ((rgr-emacs-version-p 19 30)
	  (global-set-key "\C-x\C-f" 'find-file-at-point)
	  (global-set-key "\C-x4\C-f" 'ffap-other-window)))
  ;; Now set up a mail peeker, since I don't have xbiff at home.  --
  ;; rgr, 7-Feb-96.  [but don't do this when su.  -- rgr, 30-Mar-96.]
  ;; [don't do this at all until we can teach it about maildirs.  -- rgr,
  ;; 3-Oct-04.]
  '(if (equal (user-login-name) (user-real-login-name))
      (run-at-time "60 sec" 120 'rgr-check-mail)))

(defun rgr-vt200-term-setup ()
  ;; vt200 emulation from NCSA Telnet.
  (cond ((rgr-emacs-version-p 19 34)
	  ;; [this shouldn't be necessary, but it seems to work . . .  -- rgr,
	  ;; 22-Oct-98.]
	  (define-key function-key-map "\e[A" [up])
	  (define-key function-key-map "\e[B" [down])
	  (define-key function-key-map "\e[C" [right])
	  (define-key function-key-map "\e[D" [left])))
  (cond ((and (rgr-emacs-version-p 19 34)
	      (string-match "sunos" system-configuration))
	  ;; [damn; I thought this worked in 19.34.  but not on SunOS machines,
	  ;; it appears.  -- rgr, 17-Feb-99.]
	  (rgr-paper-over-broken-ncsa-telnet-function-keys))
	((or (string-match "^19\\.30" emacs-version)
	     (rgr-emacs-version-p 19 34))
	  ;; Function key mapping seems to work here.  [Strange; works in our
	  ;; 19.30 on Alphas, but not IT 19.31 on Sparcs.  Kludge for now.  --
	  ;; rgr, 1-Jun-96.]  [Also works on 19.34 now.  -- rgr, 22-Oct-98.]
	  ;;
	  ;; Keys labelled on the Mac keyboard as function keys are mapped to
	  ;; the function key symbol that is 5 higher than what is on the label
	  ;; (i.e. pressing f2 sends f7).  Since f6 sends f11, that acts as a
	  ;; meta prefix (escape) by convention.  The top row of the keypad is
	  ;; mapped to kp-f1 through kp-f4 (but kp-f4 is perilously close to the
	  ;; power switch!).
	  ;;
	  ;; Note that C-h l shows the actual characters sent, and not their
	  ;; remappings by the read-key-sequence function.  -- rgr, 5-Apr-96.
	  ;;
	  ;; Here's what the keys between the main keyboard and the keypad are
	  ;; mapped to:
	  ;;
	  ;; [ help (find) ]  [ home (insert) ] [ page-up (delete) ]
	  ;; [ |X> (select) ] [ end (prior) ]   [ page-down (next) ]
	  ;;
	  ;; [ the arrow keys are screwed up.]  [not any more; see new mapping
	  ;; installed above.  -- rgr, 22-Oct-98.]
	  ;;
	  ;; (global-set-key [help] 'apropos) ;; help on Mac [but called 'find']
	  (global-set-key [f6] 'rgr-invoke-rmail) ;; F1
	  (global-set-key [f9] 'rgr-insert-symbol-abbreviation) ;; F4 on mac
	  ;; Use F5 to select the default (other-buffer) in the other
	  ;; window, creating one if in one-window configuration, and leave
	  ;; the cursor in the current window.
	  (global-set-key [f10] "\C-x4b\n\C-xo") ;; labelled F5 on Mac
	  (global-set-key [f13] 'rgr-find-shell) ;; F8
	  ;; F9, finally.  -- rgr, 1-Apr-96.
	  (global-set-key [f14] 'rgr-recompile))
	(t
	  ;; Fix broken key mapping in emacs 19.27.
	  (rgr-paper-over-broken-ncsa-telnet-function-keys))))

(defun rgr-vt220-term-setup ()
  ;;  QVT/Term under Win9x.  unfortunately, you can't reprogram the space bar to
  ;;  teach it to do C-space, etc.  may need to put these on function keys.  --
  ;;  rgr, 8-Jul-99.
  (global-set-key "\C-c " 'rgr-set-mark-command)
  ;; [also use C-x SPC, since C-c SPC is shadowed in html-helper-mode.  -- rgr,
  ;; 26-Nov-99.]
  (global-set-key "\C-x " 'rgr-set-mark-command)
  (global-set-key "\C-c\e " 'rgr-exchange-point-and-mark))

(defun rgr-paper-over-broken-ncsa-telnet-function-keys ()
  ;; Applies to NCSA/BYU Telnet version 2.5 of June 1992.  -- rgr, 6-Feb-98.
  ;; [function keys are screwed; the Telnet doc (not the RFC!) claims to send
  ;; strings matching "\e\\[[0-9]+~" (using the emacs string quoting convention)
  ;; for the function keys, which is what emacs is expecting, but the actual
  ;; strings sent are as shown below.  See the
  ;; /usr/local/share/emacs/19.34/lisp/term/lk201.el file for how emacs sets
  ;; things up for standard DEC terminals.  -- rgr, 4-Apr-96.]
  (define-key function-key-map "\eO4" [help])
  (define-key function-key-map "\eO." [f1])
  (define-key function-key-map "\eO," [f2])
  ;; f3 sends something that turns into "help"
  (define-key function-key-map "\eO8" [f4])
  (define-key function-key-map "\eO1" [f5])
  ;; f6 and f8 send the same thing, call it f8 because i use that to summon a
  ;; shell.
  (define-key function-key-map "\eO3" [f7])
  (define-key function-key-map "\eO2" [f8]);; and f6, dammit
  (define-key function-key-map "\eO5" [f9])
  ;; f10 sends something that turns into "left".
  (define-key function-key-map "\eO6" [f11])
  ;; f12 sends the same thing as f5.
  )

(defun rgr-vt102-term-setup ()
  ;; vt102 emulation from Claris Works (which does work, but not very well), or
  ;; vt100 emulation from NCSA Telnet (which may work better).  Most
  ;; irritatingly, "delete" sends backspace!  The recipe below was cribbed from
  ;; the "Keyboard translation" entry in the Emacs info file.  -- rgr, 8-Dec-95.
  ;; [CW 4.0 supports this retranslation; better to do it on the Mac.  -- rgr,
  ;; 14-Feb-96.]
  ;; Translate `C-h' to DEL.
  '(keyboard-translate ?\C-h ?\C-?)
  ;; Translate DEL to `C-h'.
  '(keyboard-translate ?\C-? ?\C-h)
  ;;
  (cond ((equal (getenv "REMOTEHOST") "jan.rgrjr.com")
	  ;; Assume NiftyTelnet 1.3 SSH 1.1.  This sends the escape keys off by
	  ;; 6.  (Or maybe /usr/share/emacs/20.7/lisp/term/lk201.el is broken
	  ;; instead . . .  -- rgr, 29-Dec-00.)
	  (message "Resetting function keys for NiftyTelnet 1.3 SSH 1.1.")
	  (define-key function-key-map "\e[17~" [f1])
	  (define-key function-key-map "\e[18~" [f2])
	  (define-key function-key-map "\e[19~" [f3])
	  (define-key function-key-map "\e[20~" [f4])
	  (define-key function-key-map "\e[21~" [f5])
	  (define-key function-key-map "\e[23~" [f6])
	  (define-key function-key-map "\e[24~" [f7])
	  (define-key function-key-map "\e[25~" [f8])
	  (define-key function-key-map "\e[26~" [f9])
	  (define-key function-key-map "\e[28~" [f10])
	  (define-key function-key-map "\e[29~" [f11])
	  (define-key function-key-map "\e[31~" [f12])
	  (define-key function-key-map "\e[32~" [f13])
	  (define-key function-key-map "\e[33~" [f14])
	  (define-key function-key-map "\e[34~" [f15]))))

(defun rgr-vt100-symbolics-term-setup ()
  ;; Fake vt100 mode in a Symbolics TELNET window; not as flexible as SUPDUP.
  ;; [maybe we could fix that?  -- rgr, 29-Jul-95.]  [not much point any more.
  ;; this will probably be obsolete soon.  sigh.  -- rgr, 4-Apr-96.]
  (global-set-key "\C-\M-@" 'rgr-exchange-point-and-mark))

(defun rgr-vt100-term-setup ()
  ;; attempting to kludge around mcclintock vt200 woes.
  (rgr-paper-over-broken-ncsa-telnet-function-keys))

(defun rgr-xterm-term-setup ()
  ;; nothing useful yet, but it suppresses the warning message.  -- rgr,
  ;; 10-Sep-99.
  )

;;;###autoload
(defun rgr-vt100-hack-ie ()
  ;; internet explorer terminal emulation sucks.  can't do C-_, can't do C-spc,
  ;; and the function keys are broken; most of them don't send.  -- rgr,
  ;; 7-Jul-99.
  (interactive)
  ;; The recipe below was cribbed from the "Keyboard translation" entry in the
  ;; Emacs info file.  -- rgr, 8-Dec-95.  [re-cribbed from the vt102 setup.  --
  ;; rgr, 7-Jul-99.]
  ;; Translate `C-h' to DEL.
  (keyboard-translate ?\C-h ?\C-?)
  ;; Translate DEL to `C-h'.
  (keyboard-translate ?\C-? ?\C-h)
  ;; Function key mappings (only the first four are sent!)
  (define-key function-key-map "\eOP" [f1])
  (define-key function-key-map "\eOQ" [f2])
  (define-key function-key-map "\eOR" [f3])
  (define-key function-key-map "\eOS" [f4]))

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
