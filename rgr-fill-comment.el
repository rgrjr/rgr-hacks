;;;*****************************************************************************
;;;
;;;; Filling comments.
;;;
;;;    Modification history:
;;;
;;; . . .
;;; split out of the rgr-hacks.el file.  -- rgr, 28-Oct-98.
;;; rgr-find-lisp-fill-prefix: fix after-comment whitespace.  -- rgr, 23-Feb-99.
;;; rgr-fill-string-paragraph: disable lisp-fill-paragraph.  -- rgr, 28-Dec-99.
;;;

(defvar lisp-comment-fill-column 80
  "*Fill Lisp comments to this column number.")
(defvar string-paragraph-fill-column 72
  "*Fill paragraphs in Lisp strings to this column number.")

(defun rgr-move-past-prefix (direction)
  ;; Move pass all occurrences of fill-prefix in the given direction, leaving
  ;; point at the beginning of a non-fill-prefix line.  This requires that
  ;; fill-prefix be non-nil and non-"" in order to avoid infinite loops at
  ;; buffer limits.  Furthermore, fill-prefix is expected to not need regular
  ;; expression quoting.
  (beginning-of-line)
  (while (and (looking-at fill-prefix)
	      (not (bobp)))
    (forward-line direction)))

(defun rgr-find-lisp-fill-prefix (near-point)
  ;; Return a string that is a prefix of the line containing near-point, and
  ;; that looks like " ;; " or some such, else NIL if none.  Mungs the match
  ;; data.  Driven by comment-start-skip if defined; if not, falls back on
  ;; comment-start (but may not work if comment-start is more than a single
  ;; character).  Definitely does *not* work for C /*, but then M-q would
  ;; randomize the */'s anyway.  [new version that handles leading whitespace
  ;; better.  -- rgr, 10-Nov-94.]
  (save-excursion
    (goto-char near-point)
    (beginning-of-line)
    (let ((bol (point)))
      (skip-chars-forward " \t")
      (cond (comment-start-skip
	      (if (or (looking-at comment-start-skip)
		      ;; [kludge around perl-mode wierdness concerning comments
		      ;; that don't start at BOL.  -- rgr, 8-Mar-05.]
		      (and (not (bolp))
			   (progn (forward-char -1)
				  (looking-at comment-start-skip))))
		  (buffer-substring bol (match-end 0))))
	    (t
	      (and (looking-at (concat comment-start "+[ \t]*"))
		   (buffer-substring bol (match-end 0))))))))

;;;###autoload
(defun rgr-fill-prefix-comment (comment-start)
  ;; Helper function for commands below.  Returns T if it was able to do
  ;; anything; NIL means it couldn't find a prefix.
  (let* ((initial (point))
	 (fill-column lisp-comment-fill-column)
	 (fill-prefix (rgr-find-lisp-fill-prefix (point))))
    ;; (message "Fill prefix is %S." fill-prefix)
    (if fill-prefix
	(let ((from nil) (to nil))
	  (save-excursion
	    (rgr-move-past-prefix -1)	;; this gets us to a non-prefix line;
	    (forward-line 1)		;; fix it.
	    (setq from (point))
	    (goto-char initial)
	    (rgr-move-past-prefix 1)
	    (setq to (point))
	    ;; (message "Fill from %d to %d." start (point))
	    (fill-region-as-paragraph from to)
	    t)))))

(defun rgr-fill-string-paragraph (arg)
  ;; Fill a paragraph in the string around point.  Treats this part of the
  ;; buffer as an ordinary text paragraph; if not in a string, this will makes a
  ;; mess of code.  See the rgr-fill-comment command.
  (save-restriction
    (save-excursion
      (narrow-to-region (progn (rgr-backward-up-list)
			       (save-excursion
				 (beginning-of-line)
				 (point)))
			(progn (forward-sexp)
			       (forward-line)
			       (point))))
    (let ((fill-column string-paragraph-fill-column)
	  ;; [don't let lisp-fill-paragraph misinterpret syntax within a string.
	  ;; -- rgr, 28-Dec-99.]
	  (fill-paragraph-function nil))
      (fill-paragraph arg))))

;;;###autoload
(defun rgr-fill-comment ()
  "Fill a paragraph in the comment or string around point.
Lisp comments are filled to lisp-comment-fill-column, and paragraphs in
strings are filled to string-paragraph-fill-column.  (Be careful this
doesn't put a '(' in the first column; that will confuse emacs.)  Only
works on comments that don't have Lisp tokens on the line before them,
and then only when the indentation is consistent."
  ;; [should be called rgr-fill-lisp-comment for consistency.  -- rgr,
  ;; 9-Feb-94.]
  (interactive)
  (if (rgr-in-string-p)
      (rgr-fill-string-paragraph nil)
      (rgr-fill-prefix-comment comment-start)))

(provide 'rgr-fill-comment)

