;;;; Tag nesting.
;;;
;;;    The tag nesting variables
;;; are in ./rgr-html-hacks, where they are also used by the
;;; rgr-html-forward-markup command.
;;;
;;;    [old] Modification history:
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
;;; $Id$

(require 'rgr-html-hacks)
(require 'thingatpt)
(eval-when-compile
  (require 'cl)
  (require 'thingatpt))

(defvar rgr-html-n-tag-nest-errors 0
  "Internal counter for rgr-html-check-tag-nesting command.")

;; Data structure describing tag nesting status within this buffer.
(defstruct (rgr-html-tag-data (:conc-name rgr-htd-) (:predicate nil))
  (tag-name nil)	;; symbolic name
  (open nil)		;; point of open tag
  )

(defun rgr-html-report-tag-error-internal
    (message &optional location tag-stack)
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
    (princ (format "%s:%s: %s"
		   (if buffer-file-name
		       (file-name-nondirectory buffer-file-name)
		       ;; this doesn't work for C-x `.  -- rgr, 24-Mar-98.
		       (buffer-name))
		   line-number
		   message))
    (if tag-stack
	(princ (format ", tag stack %S at %S"
		       (mapcar #'rgr-htd-tag-name tag-stack)
		       (point))))
    (princ "\n")
    (setq rgr-html-n-tag-nest-errors (1+ rgr-html-n-tag-nest-errors))))

(defun rgr-html-report-tag-errors (buffer)
  ;; Now generate messages for all tags that have had some problem, doing the
  ;; simple thing if there's only a single problem.
  (let ((emacs-22-p (fboundp 'compilation-next-error-function)))
    (cond ((zerop rgr-html-n-tag-nest-errors)
	    (kill-buffer buffer)
	    (message "All tags appear properly nested."))
	  (t
	    ;; Set up the error buffer in compilation mode.  But don't do this
	    ;; until we know we're going to use the buffer; it changes the
	    ;; current compilation buffer.
	    (with-current-buffer buffer
	      (require 'compile)
	      (compilation-mode)
	      (setq mode-name "Tag-Errs")
	      (if emacs-22-p
		  ;; Emacs 22 setup.
		  (setq next-error-function
			'compilation-next-error-function)))))
    (cond ((= rgr-html-n-tag-nest-errors 1)
	    ;; [somewhat kludgy; if there's only one tag error, we just go to it
	    ;; and report it.  but there's no clean interface for this.  -- rgr,
	    ;; 24-Mar-98.]
	    (push-mark)
	    (save-window-excursion
	      ;; this is from (next-error).
	      (cond (emacs-22-p
		      ;; this doesn't work within save-excursion . . .
		      (set-buffer buffer)
		      (funcall next-error-function 1 t))
		    (t
		      ;; [Emacs 21 version -- this gets no less than two
		      ;; compiler warnings under 22.  -- rgr, 28-Dec-06.]
		      (compilation-goto-locus
		        (compilation-next-error-locus 1 t)))))
	    (setq compilation-last-buffer nil)
	    (with-current-buffer buffer
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
		(setq tag (rgr-htd-tag-name (car tag-stack)))
		(not (if (atom required-container)
			 (eq tag required-container)
			 (memq tag required-container))))
      (or (memq tag allowed-intermediates)
	  (setq illegal-intermediates
		(cons (car tag-stack) illegal-intermediates)))
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
				   allowed-intermediates "or"))
			 "")
		     (rgr-html-tag-name-list-string
		       (mapcar #'rgr-htd-tag-name illegal-intermediates))
		     (if (cdr illegal-intermediates) "are" "is")))
	    (dolist (intermediate illegal-intermediates)
	      (rgr-html-report-tag-error-internal
	        (format "Location of intervening <%s>"
			(rgr-htd-tag-name intermediate))
		(rgr-htd-open intermediate)))))
    illegal-intermediates))

(defun rgr-html-find-tag-data (tag-name-or-set nesting-tags)
  ;; helper for rgr-html-collect-tag-data
  (let ((tail nesting-tags) (result nil))
    (while tail
      (if (let ((stack-tag (rgr-htd-tag-name (car tail))))
	    (if (listp tag-name-or-set)
		(member stack-tag tag-name-or-set)
		(eq stack-tag tag-name-or-set)))
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

(defun rgr-html-same-line-p (point1 point2)
  ;; Two points are on the same line if there is no intervening newline.
  (cond ((< point1 point2)
	  (save-excursion
	    (goto-char point1)
	    (skip-chars-forward "^\n" point2)
	    (= (point) point2)))
	(t
	  (rgr-html-same-line-p point2 point1))))

(defun rgr-html-tag-must-not-nest (tag-name tag-stack must-not-be-in)
  ;; Check that tag-name is not being nested.
  (let ((other-form (rgr-html-find-tag-data must-not-be-in tag-stack)))
    (if other-form
	(let ((other-tag (rgr-htd-tag-name other-form))
	      (other-start (rgr-htd-open other-form)))
	  (rgr-html-report-tag-error-internal
	    (if (eq other-tag tag-name)
		(format "Nesting <%s> within itself does not work." tag-name)
		(format "Nesting <%s> within <%s> does not work."
			tag-name other-tag)))
	  (if (not (rgr-html-same-line-p other-start (point)))
	      (rgr-html-report-tag-error-internal
	        (format "Location of enclosing <%s>" other-tag)
		other-start))))))

(defun rgr-html-process-implicit-closes (tag-name close-p tag-stack)
  ;; Process implicit closes by winnowing the stack.  Typically, the start of a
  ;; block-level element closes other block-level elements, and elements that
  ;; belong to certain containers are closed by closing the container.  This
  ;; allows for weird floating constructs such as <form> to cross block-level
  ;; boundaries.  See rgr-html-tag-rules for the full set.
  (let* ((rules (assoc tag-name rgr-html-tag-rules))
	 (implicitly-closed (nth (if close-p 2 1) rules))
	 (containers (nth 3 rules)))
    (when tag-stack
      (let* ((head (car tag-stack))
	     (name (rgr-htd-tag-name head))
	     (tail (cdr tag-stack))
	     (new-tail nil))
	(cond ((member name implicitly-closed)
		'(message "implicitly closing %s because of <%s%s> at %S"
			 name (if close-p "/" "") tag-name (point))
	        (rgr-html-process-implicit-closes tag-name close-p tail))
	      ((if close-p
		   ;; A close tag can only close tags that started within it.
		   (eq name tag-name)
		   ;; Open tags can only close tags within containers.
		   (memq name containers))
		;; End of scope; return the stack unmodified.
		tag-stack)
	      ((eq name 'table)
		;; [kludge: don't recur past tables.  -- rgr, 14-Mar-16.]
		tag-stack)
	      ((eq (setq new-tail (rgr-html-process-implicit-closes
				    tag-name close-p tail))
		   tail)
		;; Stack unchanged.
		tag-stack)
	      (t
		(cons head new-tail)))))))

(defun rgr-html-collect-tag-data ()
  ;; Look for tags from point to the bottom of the buffer, using
  ;; rgr-html-report-tag-error-internal to report problems.
  (let ((last-a-tag nil) (tag-stack nil))
    ;; [do we need to check for other tag identification?  -- rgr, 6-Mar-96.]
    (while (re-search-forward rgr-html-tag-regexp nil t)
      (let* ((tag-start (match-beginning 0))
	     (tag-end (save-excursion
			(goto-char tag-start)
			(forward-sexp)
			(point)))
	     (tag-name (rgr-html-matched-tag-name))
	     (close-p (match-beginning 1))
	     (rules nil))
	(cond ((and last-a-tag (eq tag-name 'a) close-p)
		;; This matches an </a> form with the right <a name=...> or <a
		;; href=...>, if it exists.  [strictly, we should maintain a
		;; stack, but this works well enough.  -- rgr, 24-Mar-98.]
		(setq tag-name (rgr-htd-tag-name last-a-tag))))
	'(message "Tag <%s%S> at %d"
		 (if close-p "/" "") tag-name tag-start)
	(setq rules (assoc tag-name rgr-html-tag-rules))
	;; Process implicit closes.
	(setq tag-stack (rgr-html-process-implicit-closes
			  tag-name close-p tag-stack))
	;; Look for placement problems for open tags.
	(unless close-p
	  (let ((must-be-in (nth 3 rules))
		(must-not-be-in (nth 5 rules)))
	    (when must-be-in
	      (rgr-html-tag-must-be-directly-in tag-name tag-stack
						must-be-in (nth 4 rules)))
	    (when must-not-be-in
	      (rgr-html-tag-must-not-nest tag-name tag-stack must-not-be-in))))
	;; Update stack.
	(cond ((memq tag-name rgr-html-empty-tags)
		(if close-p
		    (rgr-html-report-tag-error-internal
		      (format "</%s> for non-content tag" tag-name))))
	      ((not close-p)
		(let ((tag-entry (make-rgr-html-tag-data :tag-name tag-name
							 :open (point))))
		  (if (memq tag-name '(a a-name))
		      (setq last-a-tag tag-entry))
		  (setq tag-stack (cons tag-entry tag-stack))))
	      ((rgr-html-find-tag-data tag-name tag-stack)
		;; Check for inappropriate nesting here, e.g. having to skip an
		;; <ol> to pop a <ul>.
	        (while (not (eq (rgr-htd-tag-name (car tag-stack)) tag-name))
		  (let* ((open-entry (car tag-stack))
			 (unmatched-open (rgr-htd-open open-entry))
			 (other-name (rgr-htd-tag-name open-entry)))
		    (rgr-html-report-tag-error-internal
		      (format "<%s> implicitly closed by <%s>"
			      other-name tag-name))
		    (if (not (rgr-html-same-line-p unmatched-open (point)))
			(rgr-html-report-tag-error-internal
			  (format "Location of <%s>" other-name)
			  unmatched-open))
		    (setq tag-stack (cdr tag-stack))))
	        ;; Pop the tag.
		(setq tag-stack (cdr tag-stack)))
	      (t
	        (rgr-html-report-tag-error-internal
		  (format "</%s> without <%s>?" tag-name tag-name))))))
    ;; Report missing closes at top level.
    (while tag-stack
      (let* ((open-entry (car tag-stack))
	     (tag-name (rgr-htd-tag-name open-entry)))
	(unless (memq tag-name '(html body))	;; closed by EOF.
	  (rgr-html-report-tag-error-internal
	    (format "Missing close for <%s>" tag-name)
	    (rgr-htd-open open-entry)))
	(setq tag-stack (cdr tag-stack))))))

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
a few such as <hr>, <option>, etc.  Those tags appear as symbols on the
rgr-html-empty-tags list.

   2.  Tags such as <li> and <td> that appear inside containers
can usually be closed implicitly by closing the container.  Some
tags, most notably `<p>', may or may not be closed explicitly.
This is also true of `a-name', which is short for `<a name=...>'.

   3.  A few tags such as <form> are not allowed to appear within
themselves.  Otherwise, nesting depths of more than one, as in
`<em>foo<em>bar</em>baz</em>', are perfectly fine, though `bar'
is not likely to be doubly emphasized on that account.

The rules used are defined by the rgr-html-tag-rules alist (q.v.)."
  (interactive)
  (let* ((orig-buffer-name (buffer-name))
	 (buffer (get-buffer-create (concat "*" orig-buffer-name " tags*"))))
    ;; initialize error buffer
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert "HTML tag nesting errors in " orig-buffer-name "\n\n"))
    (let ((standard-output buffer)
	  (rgr-html-n-tag-nest-errors 0))
      (save-excursion
	(goto-char (point-min))
	(rgr-html-collect-tag-data))
      (rgr-html-report-tag-errors buffer))))

(provide 'rgr-html-nest)
