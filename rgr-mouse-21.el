;;;****************************************************************************
;;;
;;;    GNU emacs mouse hackery.
;;;
;;;    This requires various support functions in the ./rgr-mouse.el file.  See
;;; also the ./rgr-mouse-18.el file.
;;;
;;;    Modification history:
;;;
;;; started recreating from the old ./rgr-mouse-18.el file.  -- rgr, 24-Jan-95.
;;; now seems to work under fsf emacs 19.  -- rgr, 25-Jan-95.
;;; rgr-mouse-dired-find-file: Meta-Left in dired-mode.  -- rgr, 30-Jan-95.
;;; rgr-install-mouse-commands: emacs 19.30 fix.  -- rgr, 25-Mar-96.
;;; rgr-mouse-insert-thing: word whitespace smarts.  -- rgr, 29-Jan-98.
;;; rgr-mouse-insert-thing: fix bugs in wws, clean up.  -- rgr, 12-May-98.
;;; rgr-mouse-insert-thing: keep-properties-p, e/bobp fixup.  -- rgr, 2-Mar-99.
;;; make this work with xemacs 21.  -- rgr, 26-Jul-01.
;;;

(defvar rgr-mouse-insert-whitespace-between-words-p t
  "*Whether to insert a space between words.
This happens when mouse-inserting a wordlike thing (as determined by \\<) just
after another wordlike thing (as determined by \\>).  This may be a good thing
to make buffer-local depending on the mode.")

(defun rgr-print-map-internal (keymap prefix)
  (let ((tail (cdr keymap)))
    (if (arrayp (car tail))
	(setq tail (cdr tail)))
    (while tail
      (let ((entry (car tail)))
	(if (consp entry)
	    (let* ((key (car entry)) (binding (cdr entry))
		   (sequence (vconcat prefix (vector key)))
		   (key-string
		     ;; (if (numberp key) (make-string 1 key) (format "%s" key))
		     (key-description sequence)))
	      (cond ((atom binding)
		      (insert "\n" key-string
			      (format ": %S" binding)))
		    ((eq (car binding) 'keymap)
		      (rgr-print-map-internal binding sequence))
		    ((and (consp (cdr binding))
			  (eq (car (cdr binding)) 'keymap))
		      ;; menu
		      (insert (format "\n*** begin %S ***" (car binding)))
		      (rgr-print-map-internal (cdr binding) sequence)
		      (insert (format "\n*** end %S ***" (car binding))))
		    (t
		      ;; [don't know what to make of this.  -- rgr, 24-Jan-95.]
		      (insert "\n" key-string 
			      (format ": %S" binding)))))))
      (setq tail (cdr tail)))))

(defun rgr-print-command-table (cmds &optional sort-predicate)
  (if sort-predicate
      (setq cmds (sort cmds sort-predicate)))
  (with-output-to-temp-buffer "*Help*"
    (save-excursion
      (set-buffer "*Help*")
      (make-local-variable 'truncate-lines)
      (setq truncate-lines t))
    (princ "Mouse commands:\n\n")
    (rgr-print-table (cons '("Gesture" "Command name" "Documentation") cmds))))

(defun rgr-find-mouse-commands (map)
  ;; Helper for rgr-document-mouse-commands, below.  Needless to say, this can
  ;; afford to skip the key vector, if (arrayp (car (cdr map))).
  (let ((cmds nil) (tail (cdr map)))
    (if (arrayp (car tail))
	(setq tail (cdr tail)))
    (while tail
      (let ((entry (car tail)))
	(if (consp entry)
	    (let ((key (car entry)) (cmd (cdr entry)))
	      (if (and (symbolp key) cmd (symbolp cmd)
		       (string-match "mouse" (symbol-name key)))
		  (setq cmds
			(cons (list (concat (key-description (vector key)) ":")
				    (symbol-name cmd)
				    (let ((doc (documentation cmd)))
				      (if (and doc (string-match "\n" doc))
					  (substring doc 0 (match-beginning 0))
					  doc)))
			      cmds))))))
      (setq tail (cdr tail)))
    cmds))

(defun rgr-document-mouse-commands ()
  "Print the list of all currently-bound mouse commands and their gestures."
  (interactive)
  (let ((cmds (nconc (rgr-find-mouse-commands (current-global-map))
		     (rgr-find-mouse-commands (current-local-map))))) 
    (rgr-print-command-table cmds
			     (function (lambda (a b)
			       (string< (car a) (car b)))))))

;;;; Real commands.

(defun rgr-mouse-insert-thing (event &optional keep-properties-p)
  "Insert the thing under the mouse at point.
Sets the mark to the beginning of the insertion, and leaves point at
the end.  This works between windows/buffers, including the minibuffer,
and does not change the current buffer."
  (interactive "eP")	;; [***bug***: this doesn't work.  -- rgr, 2-Mar-99.]
  ;; (message "got %S %S" keep-properties-p event)
  (let ((from-buffer nil) start end
	(inserted-string-word-start-p nil)
	(inserted-string-word-end-p nil))
    ;; Identify the event's word/s-exp, which may be in another buffer.
    (save-excursion
      (save-window-excursion
	(mouse-set-point event)
	(setq from-buffer (current-buffer))
	(cond ((eolp)
		;; Take the current line.
		(setq end (point))
		(beginning-of-line)
		(setq start (point)))
	      (t
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
		 (setq inserted-string-word-start-p (looking-at "\\<"))
		 (forward-sexp 1)
		 (setq inserted-string-word-end-p (looking-at "\\>"))
		 (setq end (point)))))))
    ;; If we got something, stick it in the current buffer at point.
    (if from-buffer
	(let ((start-point (point))
	      (need-initial-space-p
		;; Remember this and go back to do it later, so we don't affect
		;; the meaning of start & end.  -- rgr, 12-May-98.
		(and inserted-string-word-start-p
		     (not (bobp))
		     (looking-at "\\>")))
	      (need-trailing-space-p
		(and inserted-string-word-end-p
		     (not (eobp))
		     (looking-at "\\<"))))
	  (push-mark)
	  (if keep-properties-p
	      (insert-buffer-substring from-buffer start end)
	      ;; strip properties
	      (insert (save-excursion
			(set-buffer from-buffer)
			(buffer-substring-no-properties start end))))
	  (cond (rgr-mouse-insert-whitespace-between-words-p
		  (if need-trailing-space-p
		      (insert " "))
		  (if need-initial-space-p
		      (save-excursion
			(goto-char start-point)
			(insert " ")))))))))

(defvar rgr-mouse-original-window nil)
(defvar rgr-mouse-original-point nil)

(defun rgr-mouse-mark-text-down (event)
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
  ;; [It would be nice to soup this up with mouse-drag-secondary borrowings,
  ;; specifically the dynamic mouse-dragging highlighting.  But
  ;; mouse-drag-secondary is long & involved, so some other time.  -- rgr,
  ;; 25-Jan-95.]
  ;; Can't use save-excursion because this command happens as two clicks.  At
  ;; least we don't have to remember the current buffer, because this is implied
  ;; by the current window (and the window's buffer can't be changed between the
  ;; clicks).
  (interactive "e")
  (setq rgr-mouse-original-window (selected-window))
  (setq rgr-mouse-original-point (point))
  (mouse-set-point event))

(defun rgr-mouse-mark-text-up (event)
  "See rgr-mouse-mark-text-down, without which this will not work."
  (interactive "e")
  (if (not rgr-mouse-original-window)
      (error "You can only invoke %s after %s."
	     'rgr-mouse-mark-text-up 'rgr-mouse-mark-text-down))
  (unwind-protect
       (let ((start-point (point)) (start-buffer (current-buffer)))
	 (mouse-set-point event)
	 (cond ((not (eq start-buffer (current-buffer)))
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
		   (x-set-selection nil (buffer-substring beg end))
		   (copy-region-as-kill beg end)
		   (message "%d characters copied to kill ring." (- end beg))
		   (sit-for 1)))))
    ;; Now go back to where we were.
    (select-window rgr-mouse-original-window)
    (goto-char rgr-mouse-original-point)
    (setq rgr-mouse-original-window nil)))

(defun rgr-mouse-dired-find-file (event)
  "In dired, visit the file or directory named on this line by clicking on it."
  ;; Based on the dired-find-file command.  [note also the existence of the
  ;; dired-mouse-find-file-other-window command, bound to mouse-2 by default.
  ;; -- rgr, 2-Mar-99.]
  (interactive "e")
  (let ((file (save-excursion
		(save-window-excursion
		  (mouse-set-point event)
		  (expand-file-name
		    (file-name-sans-versions (dired-get-filename) t))))))
    (find-file file)))

(defun rgr-install-dired-mouse-commands ()
  ;; Can't call this until the map is built.
  (define-key dired-mode-map [M-mouse-1] 'rgr-mouse-dired-find-file))

(defun rgr-install-mouse-commands ()
  "Install some Symbolics-like mouse commands."
  ;; Probably doesn't work in Lucid emacs.  Meant to be used from a .emacs file,
  ;; but doesn't hurt to have it available as an interactive command, too.
  (interactive)
  (cond ((eq rgr-emacs-flavor 'fsf)
	  (global-set-key [C-down-mouse-1] 'rgr-mouse-mark-text-down)
	  ;; (global-set-key [C-drag-mouse-1] 'mouse-set-secondary)
	  (global-set-key [C-mouse-1] 'rgr-mouse-mark-text-up)
	  (global-set-key [C-mouse-2] 'rgr-mouse-insert-thing)
	  ;; Need to flush C-down-mouse-2, or C-mouse-2 doesn't work in 19.30.
	  ;; -- rgr, 25-Mar-96.
	  (global-set-key [C-down-mouse-2] nil)
	  (global-set-key [M-down-mouse-3] 'mouse-buffer-menu)
	  ;; Need to nuke the down event, or the up seems to get swallowed.
	  (global-set-key [M-down-mouse-1] nil)
	  (global-set-key [M-mouse-1] 'ilisp-mouse-edit-thing))
	(t
	  (global-set-key [(control button1)] 'rgr-mouse-mark-text-down)
	  ;; (global-set-key [(control drag-button1)] 'mouse-set-secondary)
	  (global-set-key [(control button1up)] 'rgr-mouse-mark-text-up)
	  (global-set-key [(control button2)] 'rgr-mouse-insert-thing)
	  (global-set-key [(meta button3)] 'mouse-buffer-menu)
	  (global-set-key [(meta button1)] 'ilisp-mouse-edit-thing)))
  ;; Fix syntax so that names with dots in them work.
  (modify-syntax-entry ?. "_" emacs-lisp-mode-syntax-table)
  (if lisp-mode-syntax-table
      (modify-syntax-entry ?. "_" lisp-mode-syntax-table))
  (add-hook 'dired-load-hook 'rgr-install-dired-mouse-commands))

;; (global-key-binding [M-down-mouse-1])
