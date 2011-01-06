;;;****************************************************************************
;;;
;;;    Emacs mouse hackery.
;;;
;;;    The rgr-mouse package defines a number of Symbolics-compatible mouse
;;; commands that work under X11 in recent emacs versions.  Of greatest interest
;;; is rgr-mouse-edit-thing, which finds the file, directory, or source code
;;; (using either ilisp, Franz Inc's eli, or tags) of the thing clicked on, as
;;; appropriate.  It is mostly compatible with Symbolics Genera
;;; #\Meta-Mouse-Left click on random objects.  Other features are described
;;; below.
;;;
;;;    rgr-document-mouse-commands -- Print the list of all currently-bound
;;; mouse commands and their gestures.  (In emacs 19, "drag" means a drag event,
;;; and "down" means a down event; otherwise, it's an up event, even though the
;;; names of the commands bound to these events would seem to conflict.  The
;;; triple of down, drag, and up must always be bound consistently.)
;;;
;;;    rgr-mouse-insert-thing -- Insert the thing under the mouse at point.
;;; Sets the mark to the beginning of the insertion, and leaves point at the
;;; end.  This works between windows/buffers, including the minibuffer, and does
;;; not change the current buffer.
;;;
;;;    rgr-install-mouse-commands -- binds these to Symbolics-compatible mouse
;;; clicks.  Note that this shadows mouse-set-secondary (on M-left) and moves
;;; mouse-buffer-menu from C-left to M-right (which in turn shadows
;;; mouse-secondary-save-then-kill).
;;;
;;;*****************************************************************************
;;;
;;;    [original] Modification history:
;;;
;;; rgr-mouse-edit-thing, other mouse hackery.  -- rgr, 30-Mar-94.
;;; rgr-mouse-insert-thing, better -edit-thing.  -- rgr, 1-Apr-94.
;;; . . .
;;; *** emacs 19 update ***
;;;	Split out ./rgr-mouse-18.el and ./rgr-rmail-18.el .  -- rgr, 17-Jan-95.
;;; split out of ./rgr-hacks.el as the rgr-mouse package.  -- rgr, 27-Jan-95.
;;; rgr-find-url: teach rgr-ed to find URL's.  -- rgr, 30-Jan-95.
;;; . . .
;;; split out lisp stuff to new ilisp-mouse.el file.  -- rgr, 8-Apr-03.
;;;
;;;    [original] Modification history [from rgr-mouse-21.el]:
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
;;; $Id$

(eval-when-compile
  (require 'ilisp-mouse)
  (require 'browse-url)
  (require 'dired))

(require 'ilisp-mouse)
(require 'browse-url)

(defvar rgr-mouse-insert-whitespace-between-words-p t
  "*Whether to insert a space between words.
This happens when mouse-inserting a wordlike thing (as determined by \\<) just
after another wordlike thing (as determined by \\>).  This may be a good thing
to make buffer-local depending on the mode.")

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
		(with-lisp-syntax
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
	      (insert (with-current-buffer from-buffer
			(buffer-substring-no-properties start end))))
	  (cond (rgr-mouse-insert-whitespace-between-words-p
		  (if need-trailing-space-p
		      (insert " "))
		  (if need-initial-space-p
		      (save-excursion
			(goto-char start-point)
			(insert " ")))))))))

(defun rgr-install-mouse-commands ()
  "Install some Symbolics-like mouse commands."
  ;; Probably doesn't work in Lucid emacs.  Meant to be used from a .emacs file,
  ;; but doesn't hurt to have it available as an interactive command, too.
  (interactive)
  (global-set-key [C-mouse-2] 'rgr-mouse-insert-thing)
  ;; Need to flush C-down-mouse-2, or C-mouse-2 doesn't work in 19.30.
  ;; -- rgr, 25-Mar-96.
  (global-set-key [C-down-mouse-2] nil)
  (global-set-key [M-down-mouse-3] 'mouse-buffer-menu)
  ;; Need to nuke the down event, or the up seems to get swallowed.
  (global-set-key [M-down-mouse-1] nil)
  (global-set-key [M-mouse-1] 'ilisp-mouse-edit-thing))

;; (global-key-binding [M-down-mouse-1])

(provide 'rgr-mouse)
