;;;****************************************************************************
;;;
;;;    GNU emacs mouse hackery (Lucid emacs version 19 only).
;;;
;;;    This requires rgr-with-lisp-syntax (in the ./rgr-hacks.el file).  See
;;; also the ./rgr-mouse-18.el and ./rgr-mouse-19.el files.  To debug this
;;; stuff, do (setq debug-on-error t).
;;;
;;;    Modification history:
;;;
;;; split out of the ./lemacs-stuff.el file; untested.  -- rgr, 25-Jan-95.
;;;

;; Porking with mouse buttons.

(defun rgr-dump-button-bindings ()
  (dolist (shifts '(() (control) (meta) (control meta)))
    (dolist (button '(button1 button2 button3))
      (let* ((key (append shifts (list button)))
	     (command (lookup-key (current-global-map) (vector key))))
	(if command
	    (insert (format "\n%s %s" key command)))))))

(defun rgr-new-mouse-edit-thing (event)
  ;; based on rgr-mouse-edit-thing but works in lemacs-19.  [***bug***: the
  ;; save-window-excursion doesn't seem to work.  -- rgr, 10-Nov-94.]
  "Find the source files for the thing under the mouse.
If it looks like a pathname, then do find-file on it.
If it looks like a Common Lisp or elisp symbol, then do M-.
Whatever it is, it is found in the current window, regardless of where
you click.  'Looks like a pathname' means it starts with '.', '/', or
'~', or (as in a Lispm pathname) has a host: prefix followed by one of
these three characters.  Lispm pathnames are converted to ange-ftp
pathnames, which generally works, though only for Unix syntax."
  (interactive "@e")
  (let ((thing (save-excursion
		 (save-window-excursion
		   (if (mouse-set-point event)
		       (rgr-thing-around-point))))))
    (message "Got %s." thing)
    (if thing
	(rgr-ed thing))))

(defun rgr-install-mouse-commands ()
  "Install some Symbolics-like mouse commands (Lucid emacs 19 only)."
  ;; Meant to be used from a .emacs file, but doesn't hurt to have it available
  ;; as an interactive command, too.
  (interactive)
  (global-set-key [(control button2)] 'mouse-track-insert)
  (global-set-key [(meta button1)] 'rgr-new-mouse-edit-thing)
  (global-set-key [(control meta button1)] 'rgr-new-mouse-edit-thing))

; (rgr-dump-button-bindings)

