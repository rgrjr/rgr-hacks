;;;****************************************************************************
;;;
;;;    GNU emacs mouse hackery (version 18 only).
;;;
;;;    This requires rgr-with-lisp-syntax (still in the ./rgr-hacks.el file).
;;; Do (setq debug-on-error t) to debug this stuff.
;;;
;;;    Modification history:
;;;
;;; rgr-mouse-edit-thing, other mouse hackery.  -- rgr, 30-Mar-94.
;;; rgr-mouse-insert-thing, better -edit-thing.  -- rgr, 1-Apr-94.
;;; make rgr-thing-around-point deal with pathnames better.  -- rgr, 4-Apr-94.
;;; rgr-mouse-insert-thing of current line.  -- rgr, 6-Apr-94.
;;; . . . split rgr-ed out of rgr-mouse-edit-thing .  -- rgr, 7-Apr-94.
;;; rgr-buffer-menu-execute-and-select-current command, rgr-with-lisp-syntax
;;;	and related mouse command fixing.  -- rgr, 12-Apr-94.
;;; fix rgr-ed for lw:eval-region.  -- rgr, 12-Aug-94.
;;; . . . use TeX-mode syntax for c-Middle.  -- rgr, 27-Sep-94.
;;; split out version 18 mouse hackery.  -- rgr, 17-Jan-95.
;;; rgr-mouse-insert-thing: word smarts (untested).  -- rgr, 29-Jan-98.
;;;

(defun rgr-mouse-gesture-name (gesture)
  ;; Returns a string that names the mouse gesture ecoded by gesture.  This is
  ;; the same as that of the figurative constants defined in the
  ;; /usr/local/utils/Gnu/emacs-18.59/lisp/x-mouse.el file, but without the
  ;; "x-button-" prefix.
  (let ((bits (/ gesture 8))
	(names '("?-" "s-" "m-" "c-"))
	(shifts nil))
    (while (> bits 0)
      (if (= (% bits 2) 1)
	  (setq shifts (cons (car names) shifts)))
      (setq bits (/ bits 2))
      (setq names (cdr names)))
    (concat (if shifts
		(apply (function concat) shifts))
	    (nth (% gesture 4) '("right" "middle" "left" "???"))
	    (if (= (% (/ gesture 4) 2) 1)
		"-up"))))

(defun rgr-document-mouse-commands ()
  "Print the list of all currently-bound mouse commands and their gestures."
  (interactive)
  (let ((map mouse-map) (i 0)
	(cmds (list '("Gesture" "Command name" "Documentation"))))
    (while (< i (length map))
      (let ((cmd (aref map i)))
	(if (and cmd (not (eq cmd 'x-mouse-ignore)))
	    (setq cmds
		  (cons (list (concat (rgr-mouse-gesture-name i) ":")
			      (symbol-name cmd)
			      (let ((doc (documentation cmd)))
				(and doc
				     (if (string-match "\n" doc)
					 (substring doc 0 (match-beginning 0))
					 doc))))
			cmds)))
	(setq i (1+ i))))
    (setq cmds (nreverse cmds))
    (with-output-to-temp-buffer "*Help*"
      (save-excursion
	(set-buffer "*Help*")
	(make-local-variable 'truncate-lines)
	(setq truncate-lines t))
      (princ "Mouse commands:\n\n")
      (rgr-print-table cmds))))

(defun rgr-mouse-insert-thing (arg)
  "Insert the thing under the mouse at point.
Sets the mark to the beginning of the insertion, and leaves point at
the end.  This works between windows/buffers, including the minibuffer,
and does not change the current buffer."
  (let ((from-buffer nil) start end (inserting-word-p nil))
    (save-excursion
      (save-window-excursion
	(if (x-mouse-set-point arg)
	    (progn
	      (setq from-buffer (current-buffer))
	      (if (eolp)
		  ;; Take the current line.
		  (progn (setq end (point))
			 (beginning-of-line)
			 (setq start (point)))
		  ;; Find the current s-exp.
		  (rgr-with-lisp-syntax
		    (cond ((looking-at "[ \t\n]")
			    (skip-chars-forward " \t\n"))
			  ((looking-at "\\sw\\|\\s_")
			    ;; doesn't work; wrong interpretation of "word."
			    ;; (the other is bogus, though).  -- rgr, 4-Apr-94.
			    ;; (re-search-backward "\\b")
			    (skip-chars-backward "^([\" \t\n")))
		    (setq start (point))
		    (setq inserting-word-p (looking-at "\\<"))
		    (forward-sexp 1)
		    (setq end (point))))))))
    (cond (from-buffer
	    (rgr-set-mark-command nil)
	    (if (and inserting-word-p
		     (looking-at "\\>"))
		(insert " "))
	    (insert-buffer-substring from-buffer start end)))))

(defun rgr-mouse-edit-thing (arg)
  "Find the source files for the thing under the mouse.
If it looks like a pathname, then do find-file on it.
If it looks like a Common Lisp or elisp symbol, then do M-.
Whatever it is, it is found in the current window, regardless of where
you click.  'Looks like a pathname' means it starts with '.', '/', or
'~', or (as in a Lispm pathname) has a host: prefix followed by one of
these three characters.  Lispm pathnames are converted to ange-ftp
pathnames, which generally works, though only for Unix syntax."
  (let ((thing (save-excursion
		 (save-window-excursion
		   (if (x-mouse-set-point arg)
		       (rgr-thing-around-point))))))
    (if thing
	(rgr-ed thing))))

(defvar rgr-mouse-original-window nil)
(defvar rgr-mouse-original-point nil)

(defun rgr-mouse-mark-text-down (arg)
  "Set mark or copy text to kill ring.
If clicked in a single spot, sets the mark there.
If clicked in one spot & dragged to and released at another, copies the
intervening text to the kill ring.  (Note that rgr-mouse-mark-text-up
must be bound to the corresponding release event for either of these to
work.)  The click and release points may be in either order, and need
not even be in the same window, as long as they are in the same buffer
\(which need not be the same as the current buffer).  Point, current
buffer, and window state are not changed.  For Symbolics hackers, this
is like #\c-mouse-left in a dynamic window."
  ;; Can't use save-excursion because this command happens as two clicks.  At
  ;; least we don't have to remember the current buffer, because this is implied
  ;; by the current window (and the window's buffer can't be changed between the
  ;; clicks).
  (setq rgr-mouse-original-window (selected-window))
  (setq rgr-mouse-original-point (point))
  '(message "Point %d in %s."
	   rgr-mouse-original-point rgr-mouse-original-window)
  (x-mouse-set-point arg))

(defun rgr-mouse-mark-text-up (arg)
  "See rgr-mouse-mark-text-down, without which this will not work."
  (if (not rgr-mouse-original-window)
      (error "You can only invoke %s after %s."
	     'rgr-mouse-mark-text-up 'rgr-mouse-mark-text-down))
  (unwind-protect
       (let ((start-point (point)) (start-buffer (current-buffer)))
	 (cond ((not (x-mouse-set-point arg))
		 nil)
	       ((not (eq start-buffer (current-buffer)))
		 ;; But we don't care about the window, in case the user wants
		 ;; to mark a long stretch of text by putting the start & end in
		 ;; different visible windows.
		 (error "Marked text must lie within a single buffer."))
	       ((eq start-point (point))
		 ;; Zero-length text selection; just set mark.
		 (push-mark))
	       (t
		 ;; Normal c-w behavior (code partly stolen from x-cut-text).
		 (let ((beg (min start-point (point)))
		       (end (max start-point (point))))
		   (x-store-cut-buffer (buffer-substring beg end))
		   (copy-region-as-kill beg end)
		   (message "%d characters copied to kill ring." (- end beg))
		   (sit-for 1)))))
    ;; Now go back to where we were.
    (select-window rgr-mouse-original-window)
    (goto-char rgr-mouse-original-point)
    (setq rgr-mouse-original-window nil)))

(defun rgr-install-mouse-commands ()
  "Install some Symbolics-like mouse commands (emacs 18 only)."
  ;; Mean to be used from a .emacs file, but doesn't hurt to have it available
  ;; as an interactive command, too.
  (interactive)
  (require 'x-mouse)	;; needed to define mouse-map, etc.
  ;; (require 'x-menu)  ;; no such!
  ;; [autoloading doesn't help rgr-mouse-edit-thing because it won't fire up a
  ;; lisp anyway.  -- rgr, 1-Apr-94.]
  '(autoload 'edit-definitions-lisp "ilisp"
	    "Edit definitions through inferior lisp" t nil)
  ;; Fix syntax (required for rgr-thing-around-point).
  (modify-syntax-entry ?. "_" emacs-lisp-mode-syntax-table)
  (if lisp-mode-syntax-table
      (modify-syntax-entry ?. "_" lisp-mode-syntax-table))
  (define-key mouse-map x-button-c-left 'rgr-mouse-mark-text-down)
  (define-key mouse-map x-button-c-left-up 'rgr-mouse-mark-text-up)
  (define-key mouse-map x-button-c-middle 'rgr-mouse-insert-thing)
  (define-key mouse-map x-button-m-left 'rgr-mouse-edit-thing)
  ;; Also bind to c-m, in case we need to use this under a window manager that
  ;; usurps meta-clicks (until we think of something better to bind here).
  (define-key mouse-map x-button-c-m-left 'rgr-mouse-edit-thing))
