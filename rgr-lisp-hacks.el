;;;****************************************************************************
;;;
;;;    GNU Emacs hackery.
;;;
;;; [created (split out of the rgr-hacks.el file).  -- rgr, 20-Nov-98.]
;;;
;;; $Id$

(defun rgr-nth-buffer (n)
  ;; Find the nth previous buffer.  This would be equivalent to
  ;;	(nth n (buffer-list))
  ;; except that internal buffers are ignored.
  (let ((tail (buffer-list))
	(result nil))
    (while tail
      (let ((first (car tail)))
	(cond ((eq (aref (buffer-name first) 0) ? )
		;; invisible buffer; don't count it.
		(setq tail (cdr tail)))
	      ((<= n 0)
		(setq result first)
	        ;; force loop exit
	        (setq tail nil))
	      (t
		(setq tail (cdr tail))
		(setq n (1- n))))))
    result))

;;;###autoload
(defun rgr-switch-to-other-buffer (&optional arg)
  "Select the previous buffer in this window.
With a numeric arg, select the nth previous (defaults to 2)."
  ;; This is what C-M-L does on the Lispm.  The CMULisp package overrides this
  ;; with essentially the same behavior.  Note that the default behavior (the
  ;; case for arg=2) may be different from what other-buffer returns.
  (interactive "P")
  (setq arg (if (null arg)
		2
		(prefix-numeric-value arg)))
  (switch-to-buffer (or (rgr-nth-buffer (1- arg))
			(error "Number out of range."))))

;; [this was originally lisp-def-name out of the ilisp package, but i got tired
;; of requiring ilisp just to grab an elisp definition name.  could be
;; simplified on that account.  -- rgr, 15-Sep-99.]
(defun rgr-original-lisp-def-name (&optional namep)
  "Return the name of a definition assuming that you are at the start of
the sexp.  If the form starts with DEF, the form start and the next
symbol will be returned.  Optional NAMEP will return only the name
without the defining symbol."
  (let ((case-fold-search t))
    (if (looking-at
	 ;; (( \( (def*) (( \( (setf)) | \(?)) | \(?) (symbol)
	 ;; 12    3    3 45    6    65      42      1 7      7
	 ;;0011\(22 def*        22         32 43\(54 setf54         43   \(?32 11      00 60           60
	 "\\(\\((\\(def[^ \t\n]*\\)[ \t\n]+\\(\\((\\(setf\\)[ \t\n]+\\)\\|(?\\)\\)\\|(?\\)\\([^ \t\n)]*\\)")
	(let ((symbol (buffer-substring (match-beginning 7) (match-end 7))))
	  (if (match-end 6)
	      (concat (if (not namep) 
			  (concat 
			   (buffer-substring (match-beginning 3) (match-end 3))
			   " "))
		      "("
		      (buffer-substring (match-beginning 6) (match-end 6))
		      " " symbol ")")
	      (if (match-end 3)
		  (concat (if (not namep)
			      (concat 
			       (buffer-substring (match-beginning 3) 
						 (match-end 3))
			       " "))
			  symbol)
		  symbol))))))

(defun rgr-lisp-snarf-definition-name ()
  ;; Given that we are just before a definition name, return it as a string.
  ;; [We don't try emacs lisp symbols for this because (a) it may be a (setf
  ;; foo) form, and (b) the emacs 'read' function gets confused by the
  ;; differences between CL and elisp syntax.  In particular, trying to get the
  ;; name of "(defmethod foo? ((x bar))..." results in "(method foo 32 (bar))".
  ;; -- rgr, 17-Jan-02.
  (let ((start (point)))
    (if (eq (char-after) ?\()
	;; list syntax.
	(forward-sexp)
	;; "atom" syntax (extremely simpleminded version).
	(skip-chars-forward "^ \t\n"))
    (buffer-substring start (point))))

(defun rgr-lisp-def-name (&optional namep)
  "Return the name of a definition assuming that you are at the start of
the sexp.  This is like rgr-original-lisp-def-name, but is smarter about
defmethod forms."
  (let ((case-fold-search t))
    (if (looking-at "(defmethod[ \t\n]+")
	(save-excursion
	  (goto-char (match-end 0))
	  (let ((qualifiers-and-name (list (rgr-lisp-snarf-definition-name)))
		(arglist (read (current-buffer)))
		(specializers nil))
	    ;; Read past qualifiers.
	    (while (not (listp arglist))
	      (setq qualifiers-and-name (cons arglist qualifiers-and-name))
	      (setq arglist (read (current-buffer))))
	    ;; Convert arglist to specializers.
	    (while arglist
	      (let ((arg (car arglist)))
		(cond ((member arg '(&rest &optional &key &aux))
			(setq arglist nil))
		      ((consp arg)
			(setq specializers (cons (car (cdr arg)) specializers)))
		      (t
			(setq specializers (cons t specializers)))))
	      (setq arglist (cdr arglist)))
	    ;; Put it all together (but as a string).
	    (format "%s%s"
		    (if namep "defmethod " "")
		    (append '(method) (nreverse qualifiers-and-name)
			    (list (nreverse specializers))))))
	;; Try the standard recipe.
	(rgr-original-lisp-def-name namep))))

(defun rgr-lisp-mode-definition-name ()
  ;; interface to rgr-mode-definition-name
  (if (not (looking-at "^("))
      (beginning-of-defun))
  (rgr-lisp-def-name t))

(put 'lisp-mode 'mode-definition-name 'rgr-lisp-mode-definition-name)
(put 'emacs-lisp-mode 'mode-definition-name 'rgr-lisp-mode-definition-name)

;;;###autoload
(defun rgr-add-to-lisp-modification-history (&optional insert-definition-name-p)
  "Add to a modification history at the end of the first paragraph.
Sets the mark before moving there, and inserts a fresh comment line.
If no history exists (which it determines by searching for the string
in the rgr-modification-history-herald variable), then you are asked
about starting one.  (If you are asked this when there already is one,
then somebody probably inserted a blank line at the beginning of the
file.)  If given a numeric argument, inserts the definition name (if it
can find one).  Presently it doesn't do anything smart about inserting
name or date automatically.  It probably should, if for no other reason
than to standardize the format.  It also depends on Lisp syntax."
  (interactive "P")
  (require 'rgr-hacks)
  (rgr-add-to-modification-history-internal
    ";;;    " "^;+ *" ";;;"
    ;; Use ;* so we don't skip blank lines.
    "^;* *$"
    (cond (insert-definition-name-p
	    (save-excursion
	      (if (not (looking-at "^("))
		  (beginning-of-defun))
	      (rgr-lisp-def-name t))))))

;; Comment region
;; [taken from the ilisp comment-region-lisp function, with an added space for
;; readability.  -- rgr, 7-Sep-99.]
;;;###autoload
(defun rgr-comment-region-lisp (start end prefix)
  "If prefix is positive, insert prefix copies of comment-start at the
start and comment-end at the end of each line in region.  If prefix is
negative, remove all comment-start and comment-end strings from the
region."
  (interactive "r\np")
  (save-excursion
    (goto-char end)
    (if (and (not (= start end)) (bolp))
	(setq end (1- end)))
    (goto-char end)
    (beginning-of-line)
    (let ((comment-marker (make-marker)))
      (set-marker comment-marker (point))
      (untabify start end)
      (goto-char start)
      (beginning-of-line)
      (let* ((count 1)
	     (comment comment-start)
	     (comment-end (if (not (equal comment-end "")) comment-end)))
	(cond ((> prefix 0)
		(while (< count prefix)
		  (setq comment (concat comment-start comment)
			count (1+ count)))
	        ;; add a space for readability.
	        (setq comment (concat comment " "))
		(while (<= (point) comment-marker)
		  (beginning-of-line)	;; i think this is redundant.
		  (insert comment)
		  (if comment-end (progn (end-of-line) (insert comment-end)))
		  (forward-line 1)))
	      (t
		(setq comment (concat comment "+ "))
		(while (<= (point) comment-marker)
		  (back-to-indentation)
		  (if (looking-at comment) (replace-match ""))
		  (if comment-end
		      (progn
			(re-search-backward comment-end)
			(replace-match "")))
		  (forward-line 1)))))
      (set-marker comment-marker nil))))

;;; Replacing a binding.

(defun rgr-frob-binding ()
  "Replace an expression with a variable name.
This must be invoked within a let, let*, do, or do* binding form,
from which both the variable name and bound value are extracted.
Within the scope of the binding, all occurrences of the
expression are replaced with the bound variable name."
  (interactive)
  ;; First, find out where we are.
  (let ((binding-form-start nil)	;; Before the open paren.
	(star-p nil)			;; Whether a let* or do* form.
	(scope-start nil)		;; Before the body, for non-star style.
	(form-end nil)			;; End of scope.
	(initial-point (point)))
    (let ((paren-starts nil)
	  (done-p nil))
      (save-excursion
	(while (not (or (looking-at "(\\(let\\|do\\)\\(*\\)?[ \t\n]")
			done-p))
	  (if (eq (char-after (point)) ?\()
	      (setq paren-starts (cons (point) paren-starts)))
	  (condition-case err
	      (backward-up-list)
	    (error (message "%S" err)
		   (setq done-p t))))
	(if done-p
	    (error "Not in a LET or DO binding subform [unmatched]."))
	(setq star-p (not (null (match-string 2))))
	'(message "paren starts %S star-p %S" paren-starts star-p)
	(setq binding-form-start
	      (or (car (cdr paren-starts))
		  (error "Not inside a LET or DO binding subform.")))
	;; Find the end of the form, since this is the end of the scope.
	(save-excursion
	  (forward-sexp 1)
	  (setq form-end (point)))
	;; Make sure that binding-form-start is really inside the list of
	;; binding subforms, and not the body.
	(forward-char 1)
	(forward-sexp 2)
	(if (< (point) binding-form-start)
	    (error "Not in a LET or DO binding subform [body]."))
	;; Find the start of the scope.  This will be after the chosen binding
	;; form if star-p, else before the body (which is where we are now.
	(cond (star-p
		(goto-char binding-form-start)
		(forward-sexp 1)))
	(setq scope-start (point))))
    (message "binding-form-start %S scope-start %S form-end %S star-p %S"
	     binding-form-start scope-start form-end star-p)
    ;; Parse out the variable and initial value expression.  We can't just read
    ;; the binding form, because we want the variable and expression as separate
    ;; strings.
    (let ((variable-name nil) (expression-regexp nil) (count 0))
      (goto-char binding-form-start)
      (forward-char 1)
      (let ((var-end (progn (forward-sexp 1) (point)))
	    (init-end (progn (forward-sexp 1) (point)))
	    (init-start (progn (forward-sexp -1) (point)))
	    (var-start (progn (forward-sexp -1) (point))))
	(setq variable-name (buffer-substring var-start var-end))
	(setq expression-regexp
	      ;; [bug:  should generalize whitespace.  -- rgr, 30-Dec-07.]
	      (regexp-quote (buffer-substring init-start init-end))))
      ;; Do the replacement.
      (goto-char scope-start)
      (while (re-search-forward expression-regexp form-end t)
	(replace-match variable-name)
	(setq count (1+ count)))
      (message "Done; %d replacements." count)
      (if (zerop count)
	  (goto-char initial-point)))))

;;;; Slime stuff.

;;;###autoload
(defun rgr-load-slime ()
  (interactive)
  (let ((slime-dir nil)
	(tail '("/usr/local/emacs/slime-2.0"
		"/usr/local/src/emacs/slime-2.0"
		"/shared/emacs/slime")))
    (while tail
      (if (file-directory-p (car tail))
	  (setq slime-dir (car tail) tail nil)
	  (setq tail (cdr tail))))
    (if (not slime-dir)
	(error "Can't find slime."))
    (message "Loading slime.")
    (add-to-list 'load-path (expand-file-name slime-dir)))
  ;; Set up for CMU Common Lisp.
  (setq inferior-lisp-program "/usr/local/bin/lisp")
  ;; Set up for Steel Bank Common Lisp.  [not working yet.  -- rgr, 2-Mar-08.]
  ;(setq inferior-lisp-program "/usr/local/bin/sbcl")
  (require 'slime)
  (slime-setup))

;;;###autoload
(defun rgr-lisp-mode-hook ()
  (rgr-define-lisp-mode-commands lisp-mode-map)
  ;; Undo these, which don't add much, and can shadow good find-file-at-point
  ;; things (when running at home).  -- rgr, 6-Feb-98.
  (define-key lisp-mode-map "\C-x\C-f" nil)
  ;; Indent comments to 80 characters.
  (setq fill-column 80)
  ;; Fix lisp indentation.  Don't know why lisp-indent-hook defaults to
  ;; lisp-indent-hook, which clearly does the wrong thing.
  (rgr-common-lisp-indentation))

;;;; Common Lisp indentation.

;;;###autoload
(defun rgr-common-lisp-indentation ()
  "Fix Lisp indentation.  Note that this changes how the Lisp indenting
commands work globally (regardless of major mode, dialect, etc.)."
  ;; Don't know why lisp-indent-hook defaults to lisp-indent-hook, which clearly
  ;; does the wrong thing.  [but it's called lisp-indent-function in emacs 19.
  ;; -- rgr, 26-Oct-94.]
  ;; (interactive)
  (let* ((lisp-indent (if (eq rgr-emacs-major-version 18)
			  'lisp-indent-hook
			  'lisp-indent-function))
	 (common-lisp-indent
	  (intern (concat "common-" (symbol-name lisp-indent)))))
    (set lisp-indent common-lisp-indent)
    (mapcar '(lambda (entry)
	      ;; Stolen from the bottom of cl-indent.el.
	      (let ((name (car entry)) (spec (cdr entry)))
		(put name common-lisp-indent
		     (if (symbolp spec)
			 (get spec common-lisp-indent)
			 (car spec)))
		name))
	    '((cond	(&rest (&whole 1 &body)))
	      (defun	(4 (&whole 7 &rest 1) &body))
	      (defmacro	(4 (&whole 10 &rest 1) &body))
	      (deftype	(4 (&whole 9 &rest 1) &body))
	      (defvar	(4 (&whole 8 &rest 1) &body))
	      (defparameter	(4 (&whole 14 &rest 1) &body))
	      (defconstant	(4 (&whole 13 &rest 1) &body))
	      ;; [probably need defconst & defparameter too.  -- rgr,
	      ;; 27-Jan-95.]
	      ;; (get 'progn 'common-lisp-indent-function) -> 0
	      (let	((&whole 4 &rest (&whole 1 2)) &body))
	      (let* . let)
	      (prog1 . nil);; this doesn't work.
	      ;; Garnet
	      (create-instance (2 (&whole 17 &rest 1) &body))
	      ;; rgr-hacks
	      (rgr-emacs-major-version-case . cond)
	      ;; other
	      (check-pointer-slot ((&whole 4 &rest 1) &body))
	      ))))

;;; Conclusion.

(provide 'rgr-lisp-hacks)

