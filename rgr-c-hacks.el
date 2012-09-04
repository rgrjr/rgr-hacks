;;;*****************************************************************************
;;;
;;;    C programming hacks.
;;;
;;; We load this file when needed by autoloading rgr-c-mode-hook and adding it
;;; c-mode-hooks, thus getting this if and only if we look at a C source file.
;;;
;;;    May want to fix c-fill-paragraph to be smarter about not being in a
;;; comment, or being in among multiple /* drivel */ lines.
;;;
;;; [created with rgr-insert-char-alias, rgr-c-mode-hook.  -- rgr, 30-Mar-95.]
;;;
;;; $Id$

;; [for some very wierd reason, this doesn't work in emacs 23; it tries to read
;; the ")" at the end of eval-when-compile and fails.  -- rgr, 20-Jul-08.]
'(eval-when-compile
  (require 'cc-mode))

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
	  (insert ?_))))

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

;; [quiet the byte-compiler.  -- rgr, 18-Jan-08.]
(defvar blank-lines)

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

;;;###autoload
(defun rgr-c-def-name (&optional namep)
  "Return the name of the current C definition, prefixed by any
declaration of return type.  Optional NAMEP, if true, returns only the
function name without the return type declaration.  (This is present
mostly for compatibility with the rgr-lisp-def-name fn.)"
  (save-excursion
    (if (not (eq (car (car (c-guess-basic-syntax))) 'defun-open))
	(beginning-of-defun))
    (let ((bod (point))
	  (start nil) (name-end nil))
      (forward-sexp -1)
      (cond ((not (looking-at "("))
	      (error "oops; not prototyped -- syntax %S" (c-guess-basic-syntax))
	      (sit-for 2)))
      (setq name-end (point))
      (cond (namep
	      (forward-sexp -1))
	    (t
	      (while (and (not (bobp))
			  (not (looking-at "^[ \t]*$")))
		(forward-line -1))
	      (skip-chars-forward " \t\n")))
      (let ((result (buffer-substring (point) name-end)))
	;; the first alternative eliminates redundant spaces, while the second
	;; turns each tab and newline into a space.  eventually, all runs of
	;; whitespace get turned into a single space.
	(while (string-match "  +\\|[\t\n]" result)
	  (setq result (replace-match " " t t result)))
	;; Change (e.g.) "struct foo * fn" into "struct foo *fn".  (The "*" is
	;; literal here.)
	(while (string-match "* " result)
	  (setq result (replace-match "*" t t result)))
	result))))

(defun rgr-c-mode-definition-name ()
  ;; Interface to rgr-mode-definition-name
  (rgr-c-def-name t))

(put 'c-mode 'mode-definition-name 'rgr-c-mode-definition-name)
(put 'cc-mode 'mode-definition-name 'rgr-c-mode-definition-name)

;;;###autoload
(defun rgr-c-mode-hook ()
  (setq fill-column 80)
  (define-key lisp-mode-map "\C-cl" 'rgr-c-ify-identifier)
  (rgr-c-mode-hook-internal c-mode-map))

(defun rgr-c-mode-hook-internal (mode-map)
  ;; Reinstall the global M-a and M-e bindings, since moving by statements seems
  ;; to be broken (and is not enough different from moving by lines to be useful
  ;; in any case).  -- rgr, 19-Nov-96.
  (define-key mode-map "\M-e" 'forward-sentence)
  (define-key mode-map "\M-a" 'backward-sentence)
  (define-key mode-map "\r" 'newline-and-indent)
  ;; . . . let's try this.  -- rgr, 1-Dec-96.
  (define-key mode-map "-" 'rgr-c-electric-dash)
  (define-key mode-map "\M-#" 'rgr-c-frob-comment)
  (define-key mode-map "\M-s" 'rgr-center-line)
  (define-key mode-map "\C-cl" 'rgr-c-ify-identifier))

;;;###autoload
(defun rgr-java-mode-hook ()
  (setq fill-column 80)
  (rgr-c-mode-hook-internal java-mode-map))

(provide 'rgr-c-hacks)
