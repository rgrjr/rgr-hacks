;;;; Tag nesting.
;;;
;;;    The tag nesting variables and the rgr-html-tag-does-not-nest-p function
;;; are in ./rgr-html-hacks, where they are also used by the
;;; rgr-html-forward-markup command.
;;;
;;;    Do (require 'psa-defstruct) to get this to compile without errors.  [done
;;; below via eval-when-compile, but that doesn't work for interactive use.  --
;;; rgr, 4-Dec-97.]
;;;
;;;    Modification history:
;;;
;;; . . .
;;; rgr-html-check-tag-nesting: second implementation.  -- rgr, 6-Mar-96.
;;; option does not nest.  -- rgr, 29-Jan-98.
;;; rgr-html-check-tag-nesting: use compilation buffer errs.  -- rgr, 20-Mar-98.
;;; split out of ./rgr-html-hacks.el file.  -- rgr, 23-Mar-98.
;;; make rgr-html-report-tag-errors do a better job of single errors, fix
;;;	rgr-html-collect-tag-data a-name closes, misc.  -- rgr, 24-Mar-98.
;;; rgr-html-collect-tag-data: kludges for <dir> and <menu>.  -- rgr, 25-Mar-98.
;;; rgr-html-tag-must-be-directly-in: new, and smarter.  -- rgr, 26-Mar-98.
;;; rgr-html-tag-must-be-directly-in: allowed-intermediates.  -- rgr, 22-Apr-98.
;;; rgr-html-parse-tags: test of alternate tag parsing.  -- rgr, 25-Mar-99.
;;; rgr-html-report-tag-errors: require compile for 1 err.  -- rgr, 29-Mar-99.
;;; rgr-html-parse-tags: eql -> eq.  -- rgr, 1-May-00.
;;; rgr-html-collect-tag-data: implicit tag closing.  -- rgr, 6-Aug-02.
;;;

(require 'rgr-html-hacks)
(eval-when-compile (require 'psa-defstruct))

(defvar rgr-html-n-tag-nest-errors 0
  "Internal counter for rgr-html-check-tag-nesting command.")

(defun rgr-remq-once (element list)
  ;; because remq and delq will trash every occurrence.
  (cond ((null list) nil)
	((eq element (car list))
	  (cdr list))
	(t
	  (cons (car list) (rgr-remq-once element (cdr list))))))

;; Data structure describing tag nesting status within this buffer.
(psa-defstruct (rgr-html-tag-data (:conc-name rgr-htd-) (:predicate nil))
  (tag-name nil)                ;; symbolic name
  (unmatched-opens nil)		;; list of points
  ;; [these are now unused.  -- rgr, 24-Mar-98.]
  ;; (opens 0)			;; current number of opens seen
  ;; (closes 0)			;; current number of closes seen
  ;; (suspicious-location nil)	;; point of first rat we smelled.
  ;; (went-negative-p nil)	;; t if </em> before <em>
  ;; (last-location nil)	;; point of last tag
  )

;; [no longer used.  -- rgr, 24-Mar-98.]
'(defun rgr-htd-nest (rgr-html-tag-data)
  (- (rgr-htd-opens rgr-html-tag-data)
     (rgr-htd-closes rgr-html-tag-data)))

(defun rgr-html-report-tag-error-internal (message &optional location)
  ;; Generate a detail line that looks like "grep -n" on the standard-output,
  ;; and increment rgr-html-n-tag-nest-errors by 1.  location defaults to
  ;; (point), naturally.
  (let ((line-number 0))
    (or location
	(setq location (point)))
    (save-excursion
      (goto-char (point-min))
      (while (<= (point) location)
	(setq line-number (1+ line-number))
	(forward-line)))
    (princ (format "%s:%s: %s\n"
		   (if buffer-file-name
		       (file-name-nondirectory buffer-file-name)
		       ;; this doesn't work for C-x `.  -- rgr, 24-Mar-98.
		       (buffer-name))
		   line-number
		   message))
    (setq rgr-html-n-tag-nest-errors (1+ rgr-html-n-tag-nest-errors))))

(defun rgr-html-tag-entry-make-report (tag-entry)
  ;; If tag-entry describes some bogusness, then use
  ;; rgr-html-report-tag-error-internal to generate a message to that effect.
  ;; [this used to be a lot hairier before we did things on the fly; see above.
  ;; -- rgr, 24-Mar-98.]
  (let ((tag-name (rgr-htd-tag-name tag-entry))
	(tail (reverse (rgr-htd-unmatched-opens tag-entry))))
    (while tail
      (rgr-html-report-tag-error-internal
        (format "Probably no match for this <%s> tag" tag-name)
	(car tail))
      (setq tail (cdr tail)))))

(defun rgr-html-report-tag-errors (buffer nesting-tags)
  ;; Now generate messages for all tags that have had some problem, doing the
  ;; simple thing if there's only a single problem.
  (let ((tail nesting-tags))
    (while tail
      (rgr-html-tag-entry-make-report (car tail))
      (setq tail (cdr tail)))
    (cond ((zerop rgr-html-n-tag-nest-errors)
	    (kill-buffer buffer)
	    (message "All tags appear properly nested."))
	  (t
	    ;; Set up the error buffer in compilation mode.  But don't do this
	    ;; until we know we're going to use the buffer; it changes the
	    ;; current compilation buffer.
	    (save-excursion
	      (set-buffer buffer)
	      (require 'compile)
	      (compilation-mode)
	      (setq mode-name "Tag-Errs"))))
    (cond ((= rgr-html-n-tag-nest-errors 1)
	    ;; [somewhat kludgy; if there's only one tag error, we just go to it
	    ;; and report it.  but there's no clean interface for this.  -- rgr,
	    ;; 24-Mar-98.]
	    (push-mark)
	    (save-window-excursion
	      ;; this is from (next-error).
	      (compilation-goto-locus (compilation-next-error-locus 1 t)))
	    (setq compilation-last-buffer nil)
	    (save-excursion
	      (set-buffer buffer)
	      (let ((start (point)))
		(end-of-line)
		(message "%s" (buffer-substring start (point)))))
	    (kill-buffer buffer))	;; since it is redundant now
	  ((> rgr-html-n-tag-nest-errors 1)
	    (let ((original-window (frame-selected-window))
		  (message 
		    "Apparent errors found; use \\[next-error] to visit them."))
	      (switch-to-buffer-other-window buffer)
	      (select-window original-window)
	      (message "%s" (substitute-command-keys message)))))))

(defun rgr-html-tag-name-list-string (list &optional connector)
  (or connector
      (setq connector "and"))
  (cond ((null list) "")
	((atom list)
	  (format "<%s>" list))
	((null (cdr list))
	  (format "<%s>" (car list)))
	((null (cdr (cdr list)))
	  (format "<%s> %s <%s>" (car list) connector (car (cdr list))))
	(t
	  (let ((result nil) (tail list))
	    (while (cdr tail)
	      (setq result (cons (format "<%s>, " (car tail)) result))
	      (setq tail (cdr tail)))
	    (apply (function concat)
		   (nreverse (cons (format "%s <%s>" connector (car tail))
				   result)))))))

(defun rgr-html-tag-must-be-directly-in
    (tag-name tag-stack required-container &optional allowed-intermediates)
  ;; check to see that tag-name is directly in required-container (which may be
  ;; a list of legal choices) on tag-stack, with the possible exception of any
  ;; members of the allowed-intermediates list, generating a tag error
  ;; mentioning the exceptions if it is not.
  (let ((illegal-intermediates nil) (tag nil))
    (while (and tag-stack
		(setq tag (car tag-stack))
		(not (if (atom required-container)
			 (eq tag required-container)
			 (memq tag required-container))))
      (or (memq tag allowed-intermediates)
	  (setq illegal-intermediates (cons tag illegal-intermediates)))
      (setq tag-stack (cdr tag-stack)))
    (cond ((null illegal-intermediates)
	    ;; we're OK.
	    )
	  ((null tag-stack)
	    ;; popped the whole thing; no required tag is there at all.
	    (rgr-html-report-tag-error-internal
	      (format "Found <%s> outside of %s"
		      tag-name
		      (rgr-html-tag-name-list-string required-container "or"))))
	  (t
	    ;; Some required tag is there, but there's intervening garbage.
	    (rgr-html-report-tag-error-internal
	     (format "The <%s> %s in %s%s, but %s %s in the way."
		     tag-name "tag must be directly"
		     (rgr-html-tag-name-list-string required-container "or")
		     (if allowed-intermediates
			 (format " (with the possible exception of %s)"
				 (rgr-html-tag-name-list-string
				   allowed-intermediates "or")))
		     (rgr-html-tag-name-list-string illegal-intermediates)
		     (if (cdr illegal-intermediates) "are" "is")))))
    illegal-intermediates))

(defun rgr-html-find-tag-data (tag-name nesting-tags)
  ;; helper for rgr-html-collect-tag-data
  (let ((tail nesting-tags) (result nil))
    (while tail
      (if (eq (rgr-htd-tag-name (car tail)) tag-name)
	  (setq result (car tail) tail nil)
	  (setq tail (cdr tail))))
    result))

(defun rgr-html-parse-tags ()
  ;; [testing function; this will be used to update the
  ;; rgr-html-collect-tag-data search in order to detect malformed tags.  --
  ;; rgr, 25-Mar-99.]
  (interactive)
  (with-output-to-temp-buffer (concat "*" (buffer-name) " tags*")
    (goto-char (point-min))
    (while (search-forward "<" nil t)
      (let* ((tag-start (match-beginning 0))
	     (tag-end (save-excursion
			(goto-char tag-start)
			(condition-case error
			    (progn (forward-sexp)
				   (point))
			  (error
			    ;; Allow for isolated "<" characters.  (Not to
			    ;; mention the fact that emacs can't deal with '
			    ;; vs. " in tags vs. text.  -- rgr, 25-Mar-99.)
			    (message "Got error %S." error)
			    (sit-for 1)
			    nil))))
	     (tag-name nil) (delta-nest +1))
	(cond ((eq (char-after (point)) ?!)
	        (setq tag-name '!)
	        (forward-char))
	      (t
		(cond ((eq (char-after (point)) ?/)
			(setq delta-nest -1)
			(forward-char)))
		(let ((start (point)))
		  (skip-chars-forward "-.a-zA-Z0-9")
		  (and (not (= start (point)))
		       (setq tag-name
			     (intern (downcase (buffer-substring-no-properties
						 start (point)))))))))
	(if (and (eq tag-name 'a)
		 (save-excursion
		   ;; [we could parse the attributes instead, but that seems
		   ;; excessive; the caller may need to parse them anyway.  --
		   ;; rgr, 4-Dec-97.]
		   (save-match-data
		     (re-search-forward "\\<name[ \t\n]*=" tag-end t))))
	    (setq tag-name 'a-name))
	(princ (format "%d\t%s\t%s\t%d\n"
		       (point) tag-end tag-name delta-nest))))))

(defun rgr-html-collect-tag-data ()
  ;; returns nesting-tags, a list of rgr-html-tag-data structures.  The
  ;; nesting-depth is always a small integer, suspicious-location a point or
  ;; nil, and last-location is always a point.  [None of these are used anymore;
  ;; the main thing is the dynamic rgr-htd-unmatched-opens and tag-stack values,
  ;; from which possible problems are identified on the fly.  -- rgr,
  ;; 24-Mar-98.]  Collect these from point to the bottom of the buffer.
  (let ((nesting-tags nil) (last-a-tag nil) (tag-stack nil))
    ;; [do we need to check for other tag identification?  -- rgr, 6-Mar-96.]
    (while (re-search-forward rgr-html-tag-regexp nil t)
      (let* ((tag-start (match-beginning 0))
	     (tag-end (save-excursion
			(goto-char tag-start)
			(forward-sexp)
			(point)))
	     (tag-name (rgr-html-matched-tag-name))
	     (delta-nest (if (match-beginning 1) -1 +1))
	     (tag-entry
	       (cond ((and last-a-tag (eq tag-name 'a) (= delta-nest -1))
		       ;; This matches an </a> form with the right <a name=...>
		       ;; or <a href=...>, if it exists.  [***bug***: no, we
		       ;; would have to maintain a stack.  -- rgr, 4-Dec-97.]
		       ;; [actually works well enough.  -- rgr, 24-Mar-98.]
		       (setq tag-name (rgr-htd-tag-name last-a-tag))
		       last-a-tag)
		     ((rgr-html-find-tag-data tag-name nesting-tags))
		     (t
		       (setq nesting-tags
			     (cons (make-rgr-html-tag-data :tag-name tag-name)
				   nesting-tags))
		       (car nesting-tags)))))
	'(message "Tag <%s%S> at %d, entry %s"
		 (if (< delta-nest 0) "/" "") tag-name tag-start tag-entry)
	(if (and (memq tag-name '(a a-name)) (= delta-nest 1))
	    (setq last-a-tag tag-entry))
	;; Process implicit closes.  [still in development.  -- rgr, 6-Aug-02.]
	(let ((implicitly-closed
	       (nth (if (= delta-nest 1) 1 2)
		    (assoc tag-name rgr-html-tags-implicitly-closed))))
	  (while (member (car tag-stack) implicitly-closed)
	    (let ((closed-entry (rgr-html-find-tag-data (car tag-stack)
							nesting-tags)))
	      ;; [it's probably a bug if this doesn't exist.  -- rgr, 6-Aug-02.]
	      (if closed-entry
		  (psa-setf (rgr-htd-unmatched-opens closed-entry)
			    (cdr (rgr-htd-unmatched-opens closed-entry)))))
	    (setq tag-stack (cdr tag-stack))))
	;; Look for placement problems.  [new; just a testing hack now.  -- rgr,
	;; 20-Mar-98.]
	(cond ((< delta-nest 0))
	      ((memq tag-name '(dd dt))
		(rgr-html-tag-must-be-directly-in tag-name tag-stack 'dl))
	      ((eq tag-name 'li)
		(rgr-html-tag-must-be-directly-in tag-name tag-stack
						  '(ol ul dir menu)))
	      ((memq tag-name '(h1 h2 h3 h4 h5 h6 address))
	        ;; These must be directly in the body.  [actually, <body> is an
	        ;; interesting special case, because it may be implicit, in
	        ;; which case the tag stack must be empty.  -- rgr, 26-Mar-98.]
		(rgr-html-tag-must-be-directly-in tag-name tag-stack
						  'body '(center div))))
	;; Update stack.
	(cond ((rgr-html-tag-does-not-nest-p tag-name))
	      ((> delta-nest 0)
		(psa-setf (rgr-htd-unmatched-opens tag-entry)
			  (cons tag-start (rgr-htd-unmatched-opens tag-entry)))
		(setq tag-stack (cons tag-name tag-stack)))
	      ((memq tag-name tag-stack)
		(psa-setf (rgr-htd-unmatched-opens tag-entry)
			  (cdr (rgr-htd-unmatched-opens tag-entry)))
		;; [should check for inappropriate nesting here, i.e. having to
		;; skip an ol to pop a ul.  -- rgr, 24-Mar-98.]
		(setq tag-stack (rgr-remq-once tag-name tag-stack)))
	      (t
	        (rgr-html-report-tag-error-internal
		      (format "</%s> without <%s>?" tag-name tag-name)
		      (point))))))
    nesting-tags))

;;;###autoload
(defun rgr-html-check-tag-nesting ()
  ;; Second pass.  [third, now.  -- rgr, 9-Oct-97.]  [fourth pass.  -- rgr,
  ;; 20-Mar-98.]
  "Go through the entire current buffer, looking for possible HTML tag
nesting errors (the definition of which appears below).  If one problem
is found, then print a message and go there.  If two or more problems
are found, then they are described in a `compilation' buffer, which is
displayed in another window -- invoking \\[next-error] will visit the errors
one at a time.

   I don't really understand the whole deal with nesting rules (is there
an HTML ref somewhere?), but I do know the following:

   1.  Every <b> needs a </b> to match.  This applies to all tags except
a few such as <hr>, <li>, etc.  Those tags appear as symbols on the
rgr-html-tags-that-do-not-nest list.

   2.  Some tags, most notably `<p>', may or may not nest.  These are
listed on rgr-html-tags-that-may-nest, which means that all of them must
nest, or none of them.  Following current practice, `<p>' is on this
list; remove it if you wish to enforce HTML 3.0 containerization of
paragraph tags.  Also present is `a-name', which is short for
`<a name=...>'.

   3.  For the most part, tags nest independently of each other.  That
is, `<b>this<i>is</b>wierd</i>' is legal, if eccentric.  (And the `is'
should come out in italic-bold -- most browsers these days are smart
enough.)  The exceptions (e.g. `<ol><ul></ol></ul>') are *not* detected.

   4.  Nesting depths of more than one, as in `<em>foo<em>bar</em>baz</em>',
are perfectly fine, though `bar' is not likely to be doubly emphasized
on that account.

   Since it is a common error to put `<em>' where `</em>' is meant, the
first place the nesting depth goes over 1 is used as the error point if
there is an unequal number of open and close tags.  Unfortunately, this
heuristic loses for badly nested `<ol>' and `<ul>' constructs; since
these are frequently nested, this command will erroneously report the
first nested case as being in error."
  (interactive)
  (let* ((orig-buffer-name (buffer-name))
	 (buffer (get-buffer-create (concat "*" orig-buffer-name " tags*"))))
    ;; initialize error buffer
    (save-excursion
      (set-buffer buffer)
      (erase-buffer)
      (insert "HTML tag nesting errors in " orig-buffer-name "\n\n"))
    (let* ((start (point))
	   (standard-output buffer)
	   (rgr-html-n-tag-nest-errors 0)
	   (nesting-tags
	     (progn;; save-excursion
	       (goto-char (point-min))
	       (rgr-html-collect-tag-data))))
      ;; Do this kludge replacement for save-excursion so that the positions of
      ;; errors (especially the <a> syntax check) are not lost.
      (goto-char start)
      ;; (insert (format "Nesting tags:\n  %S\n" nesting-tags))
      (rgr-html-report-tag-errors buffer nesting-tags))))

(provide 'rgr-html-nest)
