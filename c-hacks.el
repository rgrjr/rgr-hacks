;;;*****************************************************************************
;;;
;;;    C programming hacks.
;;;
;;; We load this file when needed by autoloading rgr-c-mode-hook and adding it
;;; c-mode-hooks, thus getting this if and only if we look at a C source file.
;;;
;;;    May want to fix c-fill-paragraph to be smarter about not being in a
;;; comment, or being in among multiple /* drivel */ lines.  Also, M-e and M-a
;;; bindings of c-end-of-statement and c-beginning-of-statement are less than
;;; ideal.
;;;
;;;    Modification history:
;;;
;;; created with rgr-insert-char-alias, rgr-c-mode-hook.  -- rgr, 30-Mar-95.
;;; set fill-column to 80.  -- rgr, 12-Apr-95.
;;; rgr-c-ify-identifier: new.  -- rgr, 1-May-95.
;;; rgr-c-ify-identifier: bind to C-c l.  -- rgr, 2-Jun-95.
;;; rgr-c-frob-comment fn.  -- rgr, 19-Jun-95.
;;; rgr-center-line: new.  -- rgr, 7-Jul-95.
;;; rgr-add-to-c-modification-history, compile-this-file fns.  -- rgr, 8-Jul-95.
;;; rgr-c-mode-hook: disable character aliasing.  -- rgr, 10-May-96.
;;; rgr-c-frob-comment-step: flush trailing whitespace.  -- rgr, 6-Jun-96.
;;; rgr-add-to-c-modification-history: new implementation.  -- rgr, 13-Aug-96.
;;; rgr-c-frob-comment: fix Ljubomir's "banner" comments.  -- rgr, 19-Nov-96.
;;; moved compile commands to ./rgr-compile-hacks.el file.  -- rgr, 26-Nov-96.
;;; rgr-c-electric-dash: new trick.  -- rgr, 1-Dec-96.
;;; rgr-make-region-obsolete: fix region bug.  -- rgr, 10-Dec-96.
;;; rgr-add-to-c-modification-history: fix header-comment.  -- rgr, 12-Dec-96.
;;; rgr-c-use-electric-dash-p: global enable.  -- rgr, 9-Nov-99.
;;; rgr-insert-char-alias: flushed.  -- rgr, 9-Nov-99.
;;;

(defvar rgr-c-use-electric-dash-p nil
  "*Set this to non-nil for '-' to insert '_' when typed once, and '-'
when typed twice.")

;;;###autoload
(defun rgr-c-electric-dash-mode (&optional arg)
  "Toggle rgr-c-electric-dash mode.  With arg, turn rgr-c-electric-dash
mode on if and only if arg is positive.  In rgr-c-electric-dash mode,
the first time you type a dash ('-') you get an underscore ('_'); type
it twice to get a dash.

As modes go, though, this is not a very good citizen.  It only operates
globally, in all modes that install the rgr-c-electric-dash command, and
doesn't advertise itself in the mode line."
  (interactive "P")
  (setq rgr-c-use-electric-dash-p
	(if (null arg)
	    (not rgr-c-use-electric-dash-p)
	    (> (prefix-numeric-value arg) 0)))
  (and (interactive-p)
       (message "rgr-c-electric-dash mode turned %s."
		(if rgr-c-use-electric-dash-p "on" "off")))
  rgr-c-use-electric-dash-p)

;;;###autoload
(defun rgr-c-electric-dash ()
  "Insert an underscore, or a dash if typed twice."
  ;; Works, but interaction with undo could be better.  -- rgr, 1-Dec-96.
  ;; autoloaded so that ./rgr-shell-hacks.el can use it.  -- rgr, 13-Dec-96.
  ;; (and perl, and tcl, and . . . )
  (interactive)
  (cond ((not rgr-c-use-electric-dash-p)
	  ;; Insert dash.
	  (self-insert-command 1))
	((and (not (bobp))
	      (= (char-after (1- (point))) ?_))
	  ;; Change underscore to dash.
	  (delete-char -1)
	  (self-insert-command 1))
	(t
	  ;; Insert underscore
	  (let ((last-command-char ?_))
	    (self-insert-command 1)))))

(defun rgr-c-ify-identifier ()
  "Convert an identifier after point from Lisp to C syntax."
  (interactive)
  (skip-chars-forward " \t\n")
  (let ((char (char-after (point))))
    (while (and char
		(or (memq char '(?- ?_))
		    (and (<= ?0 char) (<= char ?9))
		    (and (<= ?a (downcase char)) (<= (downcase char) ?z))))
      (cond ((eq char ?-)
	      (delete-char 1)
	      (insert "_"))
	    ((and (<= ?a (downcase char)) (<= (downcase char) ?z))
	      (downcase-word 1))
	    (t
	      (forward-char)))
      (setq char (char-after (point))))))

(defun rgr-c-frob-comment-step (line-re first-p last-p)
  ;; Subroutine for rgr-c-frob-comment
  (or (looking-at line-re)
      (error "Internal error: No comment line here."))
  (let* ((end-start (match-beginning 2))
	 ;; end-end is the end of the string, so we flush trailing whitespace.
	 (end-end (match-end 0))
	 (comment-text (buffer-substring (match-end 1) end-start)))
    (or first-p
	;; Notice how we carefully replace the string so that end-start and
	;; end-end don't change.  But we must do this first, so the last-p case
	;; can potentially trim this whitespace if there was nothing in the
	;; comment.  -- rgr, 6-Jun-96.
	(progn (goto-char (match-beginning 1))
	       (delete-region (point) (match-end 1))
	       (insert "  ")))
    (or last-p
	(progn (goto-char end-start)
	       (skip-chars-backward " \t")
	       (delete-region (point) end-end)))
    ;; Lousy way to check for blankness.
    (and (string-match "^[ \t]*$" comment-text)
	 (setq blank-lines (1+ blank-lines))))
  (forward-line -1))

(defun rgr-c-frob-comment ()
  "Change a series of /*...*/ lines into a single comment.  Point need
not be on the first line.  Trailing whitespace is eliminated."
  (interactive)
  (let ((line-re
	 ;; Don't use comment-start/comment-end to build this RE; those have
	 ;; extra spaces in them.  -- rgr, 6-Jun-96.
	 "^[ \t]*\\(/\\*\\).*\\(\\*/\\)[ \t]*$")
	(start (point)))
    (beginning-of-line)
    (or (looking-at line-re)
	(error "No comment line here."))
    (let* ((n-lines -1)
	   (blank-lines 0)
	   (first (save-excursion
		    (while (and (not (bobp)) (looking-at line-re))
		      (setq n-lines (1+ n-lines))
		      (forward-line -1))
		    (or (bobp)
			(forward-line 1))
		    (point))))
      ;; first points to the beginning of the first consecutive comment line.
      (while (and (not (eobp)) (looking-at line-re))
	(setq n-lines (1+ n-lines))
	(forward-line 1))
      (or (eobp)
	  (forward-line -1))
      ;; we are now at the start of the last comment line.
      (if (= first (point))
	  (error "Only one consecutive comment line."))
      (save-excursion
	;; delete only the comment-open of the last line . . .
	(rgr-c-frob-comment-step line-re nil t)
	;; . . . both parts of intervening lines . . .
	(while (> (point) first)
	  (rgr-c-frob-comment-step line-re nil nil))
	;; . . . and the comment-close of the first line.
	(rgr-c-frob-comment-step line-re t nil))
	      (message "%d consecutive comment lines, %d of which are blank."
		       n-lines blank-lines)
      (cond ((= (- n-lines blank-lines) 1)
	      (while (> blank-lines 0)
		(delete-indentation)
		(setq blank-lines (1- blank-lines)))
	      (c-indent-command))))))

(defun rgr-center-line (&optional n)
  "Center this and the next n-1 lines, moving point."
  (interactive "p")
  (let ((sign (if (> n 0) 1 -1)))
    (while (not (= n 0))
      (center-line)
      (forward-line sign)
      (setq n (- n sign)))))

(defun rgr-make-region-obsolete (start end)
  "Insert \"#ifdef OBSOLETE\" around the lines of the region."
  ;; Need a better name (or key binding) for this.
  (interactive "r")
  (save-excursion
    ;; Insert the #endif (first, so we don't lose the value of start).
    (goto-char end)
    (if (not (bolp))
	;; Assume the user wants to include the whole line.
	(forward-line))
    (newline)
    (forward-char -1)
    (c-indent-command)
    (insert "#endif /* OBSOLETE */")
    ;; And the #if.
    (goto-char start)
    (beginning-of-line)
    (let ((line-blank-p (eolp)))
      (newline)
      (or line-blank-p
	  (forward-char -1))
      (c-indent-command)
      (insert "#ifdef OBSOLETE"))))

(defun rgr-c-def-name (&optional namep)
  "Return the name of a C definition, assuming that you are at the start
of the body of the code (as if moved there by \\[beginning-of-defun]).
If the form starts with DEF, the form start and the next symbol will be
returned.  Optional NAMEP will return only the name without the defining
symbol."
  ;; namep not yet supported (always acts as if t)
  (save-excursion ;; let ((open-curly (point)))
    (while (and (not (bobp))
		(not (looking-at "^[ \t]*$")))
      (forward-line -1))
    (skip-chars-forward " \t\n")
    (let ((start (point)))
      (forward-sexp)
      (skip-chars-forward " \t\n")
      (if namep
	  (setq start (point)))
      (forward-sexp)
      (buffer-substring start (point)))))

(defun rgr-add-to-c-modification-history (&optional insert-definition-name-p)
  ;; [syntax-independent version.  -- rgr, 13-Aug-96.]
  "Add to a modification history near the top of the file.
Sets the mark before moving there, and starts a new line before the end
of the C comment.  If no history exists (which it determines by
searching for the string in the rgr-modification-history-herald
variable), then you are asked about starting one.  (If you are asked
this when there already is one, then somebody probably inserted extra
crud at the beginning of the file.)  If given a numeric argument,
inserts the definition name (if it can find one) [presently nonworking
for C syntax]."
  (interactive "P")
  (rgr-add-to-modification-history-internal
    "/*    " "^/\\* *" "  " "^ *\\*/$"
    (cond (insert-definition-name-p
	    ;; (error "Can't find C definition names yet.")
	    (save-excursion
	      (if (not (looking-at "^\\s("))
		  (beginning-of-defun))
	      (rgr-c-def-name t))))))

;;;###autoload
(defun rgr-c-mode-hook ()
  (setq fill-column 80)
  ;; Reinstall the global M-a and M-e bindings, since moving by statements seems
  ;; to be broken (and is not enough different from moving by lines to be useful
  ;; in any case).  -- rgr, 19-Nov-96.
  (define-key c-mode-map "\M-e" 'forward-sentence)
  (define-key c-mode-map "\M-a" 'backward-sentence)
  (define-key c-mode-map "\r" 'newline-and-indent)
  ;; . . . let's try this.  -- rgr, 1-Dec-96.
  (define-key c-mode-map "-" 'rgr-c-electric-dash)
  (define-key c-mode-map "\M-#" 'rgr-c-frob-comment)
  (define-key c-mode-map "\M-s" 'rgr-center-line)
  (define-key c-mode-map "\C-cc" 'rgr-clean-c-compilation-buffer)
  (define-key c-mode-map "\M-*" 'rgr-add-to-c-modification-history)
  (define-key c-mode-map "\C-cl" 'rgr-c-ify-identifier)
  (define-key lisp-mode-map "\C-cl" 'rgr-c-ify-identifier))

(provide 'c-hacks)
