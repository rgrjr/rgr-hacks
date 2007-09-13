;;;****************************************************************************
;;;
;;;    GNU emacs-lisp hackery.
;;;
;;;    Do (setq debug-on-error t) to debug this stuff.  Use
;;; (read-key-sequence "Testing? ") to discover the arcane encodings used for
;;; non-ASCII character in FSF emacs 19.  Also, doing
;;;
;;;	(let ((e (read-event))) (cons (event-modifiers e) (event-basic-type e)))
;;;
;;; will give you the raw events and their symbolic decoding.  There is
;;; apparently no way provided to encode these symbolic characters into what
;;; read-event returns (but see ./encode-key.el).  Use (e.g.) [?\C-\M-%] in
;;; define-key calls.
;;;
;;; $Id$

(eval-when-compile
  (require 'debug))

;;;; elisp hackery.

;;;; elisp-oriented commands.

;;;###autoload
(defun rgr-count-code-lines ()
  "Count lines of lisp code in the current buffer.
A Lisp code line is one that is nonblank and not entirely a comment."
  (interactive)
  (save-excursion
    (let ((lines 0) (count 0))
      (goto-char (point-min))
      (while (not (eobp))
	(setq lines (1+ lines))
	(if (looking-at "^[ \t]*[^; \t\n]")
	    (setq count (1+ count)))
	(forward-line))
      (message "%d source code lines out of %d total (%d%%) in %s"
	       count lines
	       (if (= lines 0) 0 (/ (* 100 count) lines))
	       (buffer-name))
      count)))

;;;; Statistics.

;; [this is here for lack of a better place.  -- rgr, 29-Apr-05.]

;;;###autoload
(defun rgr-mean-and-sd (observations)
  "Compute mean and standard deviation of a list of observations."
  (let ((n 0) (sigma-x 0) (sigma-x^2 0))
    (while observations
      (let ((x (car observations)))
	(setq n (1+ n))
	(setq sigma-x (+ sigma-x x))
	(setq sigma-x^2 (+ sigma-x^2 (* x x))))
      (setq observations (cdr observations)))
    (cond ((= n 0) (list 0 0))
	  ((= n 1) (list sigma-x 0))
	  (t
	    (let ((mean (/ sigma-x n)))
	      (list mean
		    (sqrt (/ (+ sigma-x^2
				(* -2 mean sigma-x)
				(* n mean mean))
			     (1- n)))))))))

;; This is here because I don't have a better place for it.
;;;###autoload
(defun rgr-renumber-sharps ()
  "From point to the end of the buffer, renumber indices between sharps.
E.g. '#3# x #7# y #2# zz #7#' gets turned into
'#3# x #4# y #5# zz #4#', preserving the value of the first number seen,
and renumbering the others sequentially thereafter, while preserving 
the correspondence of matching numbers."
  (interactive)
  (let ((old-to-new nil)
	(last-used nil))
    (while (re-search-forward "#\\([0-9]+\\)#" nil t)
      (let* ((old (string-to-number (match-string 1)))
	     (new (cdr (assoc old old-to-new))))
	(unless new
	  (setq last-used
		(if last-used (1+ last-used) old))
	  (setq new last-used)
	  (setq old-to-new (cons (cons old new) old-to-new)))
	(if (not (= old new))
	    (replace-match (format "#%d#" new) t t))))))

;;;; Tags files and tables.

;;;###autoload
(defun rgr-make-tags-table-list (directories)
  ;; Build the list of all tags files that can be found in the list of
  ;; directories.
  (let ((result nil) (tail directories))
    (while tail
      (if (car tail)
	  (let ((file (expand-file-name "TAGS" (car tail))))
	    (if (file-readable-p file)
		(setq result (cons file result)))))
      (setq tail (cdr tail)))
    (nreverse result)))

(defun rgr-make-tags-table-list-internal ()
  ;; FSF emacs version.
  (setq tags-table-set-list
	(list
	  (rgr-make-tags-table-list
	    (append (list "/shared/mgi/oligo" "~/projects/mgi/oligo"
			  "~/projects/system/scripts"
			  "/usr/local/src/rogers/bmerc/pima-profile-0.1/src"
			  "/usr/local/src/rogers/bmerc/PIMA-0.6_2"
			  ;; [this works for SuSE 9.0.  -- rgr, 28-Jan-05.]
			  "/usr/lib/perl5/site_perl/5.8.1/Bio"
			  ;; [this is for SuSE 8.1.  -- rgr, 15-Jun-04.]
			  "/usr/lib/perl5/site_perl/5.6.1/Bio"
			  ;; and this needs to come after.
			  "~/projects/database/code")
		    load-path
		    ;; BMERC stuff.
		    (list "~psa/psa-test/bin"
			  ;; [way old.  -- rgr, 10-Feb-05.]
			  ;; "~thread/code/stat"
			  ;; this will only be available on the alphas, but
			  ;; rgr-make-tags-table-list will figure this out.  --
			  ;; rgr, 4-Nov-98.
			  "/usr/local/etc/httpd/htdocs/needle-doc/new")))))
  (setq tags-table-list (car tags-table-set-list))
  (let ((oligo-web (expand-file-name "~/projects/oligo")))
    ;; these directories contain just the essentials for oligo tools web
    ;; development.  if they exist, add them as a second (separate) set.  --
    ;; rgr, 10-Feb-05.
    (if (file-directory-p oligo-web)
	(setq tags-table-set-list
	      (append tags-table-set-list
		      (list (rgr-make-tags-table-list
			      (list oligo-web
				    "/usr/lib/perl5/site_perl/5.8.1/Bio"
				    load-path))))))))

(defun rgr-make-tag-table-alist-internal ()
  ;; xemacs version, still in development.  note the change of variable name and
  ;; format.  unfortunately, M-x visit-tags-table doesn't add to this list.  --
  ;; rgr, 26-Jul-01.
  ;; (setq tags-file-name nil)
  (setq tag-table-alist
	(mapcar (function (lambda (file)
		  (cons "\\.el$" file)))
		(rgr-make-tags-table-list load-path))))

;;;###autoload
(defun rgr-make-tags-table-list-hook ()
  "Hook function that initializes the tags-table-list variable to
include all emacs-lisp files, plus other interesting directories.  This
should be called from .emacs files."
  (if (eq rgr-emacs-flavor 'fsf)
      (rgr-make-tags-table-list-internal)
      (rgr-make-tag-table-alist-internal)))

;;;; Arglists

(defun rgr-guess-arglist-from-documentation (name &optional errorp)
  ;; If name has documentation that ends in a form, then take that as a sample
  ;; invocation and extract the arglist.  (The car of the form should be the
  ;; same as the name, but we don't bother to check that.)
  (let ((doc (documentation name)))
    (cond ((and doc
		(string-match "([^(]+)[ \t\n]*\\'" doc))
	    (condition-case ignore
		(cdr (car (read-from-string doc (match-beginning 0))))
	      ;; [not exactly according to spec . . . but at least it keeps
	      ;; (e.g.) save-excursion from crapping out.  -- rgr, 28-Jan-95.]
	      (error errorp)))
	  ((eq errorp t)
	    (error "Can't guess the args of %s." name))
	  (t errorp))))

;;;###autoload
(defun rgr-elisp-arglist (name)
  ;; Attempt to return the arglist of name, an elisp function.
  (let ((fn name))
    (while (symbolp fn)
      (if (not (fboundp fn))
	  (error "%s is not bound." fn))
      (setq fn (symbol-function fn)))
    (if (and (consp fn) (eq (car fn) 'macro))
	(setq fn (cdr fn)))
    (cond ((subrp fn)
	    (rgr-guess-arglist-from-documentation name t))
	  ((and (rgr-emacs-version-p 19)
		(byte-code-function-p fn))
	    ;; byte-code object (acts like a vector, though).
	    (aref fn 0))
	  ((or (not (consp fn)) (eq (car fn) 'autoload))
	    (error "%s is %s, can't get its args." name fn))
	  ((not (eq (car fn) 'lambda))
	    (error "%s is %s; expected a lambda." name fn))
	  (t
	    ;; Pluck out the lambda list.
	    (car (cdr fn))))))

(defun rgr-find-elisp-function-tag-default ()
  ;; Look for the relevant function name near point.  This will generally be the
  ;; one inside of whose arglist we are at present.
  (condition-case ignore
      (save-excursion
	(backward-up-list 1)
	(forward-char 1)
	(find-tag-default))
    (error
      ;; fall back on regular default.
      (find-tag-default))))

;;;###autoload
(defun rgr-show-elisp-arglist (name)
  "Tries to print the arglist of function NAME, prompting if interactive."
  ;; [oops -- i guess we have to require tags at top level after all.  -- rgr,
  ;; 13-Apr-94.]
  (interactive
    (let ((find-tag-default-function 'rgr-find-elisp-function-tag-default))
      (find-tag-tag "Function name: ")))
  (message "%s %s" name (rgr-elisp-arglist (intern name))))

;;;###autoload
(defun rgr-quick-show-elisp-arglist (name)
  "Tries to print the arglist of function NAME.
Guesses the function without prompting if it can."
  (interactive (let ((default (rgr-find-elisp-function-tag-default)))
		 (if default
		     (list default)
		     (find-tag-tag "Function name: "))))
  (rgr-show-elisp-arglist name))

;;; Editing emacs commands.

(defun rgr-elisp-find-tag-for-emacs-key (key)
  ;; based heavily on the Info-goto-emacs-key-command-node fn.
  "Find the source for the command bound to KEY, a string.
Interactively, if the binding is execute-extended-command, a command is read.
The source is found by using the \\[find-tag] command."
  (interactive "kFind tag for key: ")
  (let ((command (key-binding key))
	(finder (or ;; [this breaks in ilisp.  -- rgr, 26-Dec-00.]
		    ;; (key-binding "\M-.")
		    (function find-tag))))
    (cond ((null command)
	    (message "%s is undefined" (key-description key)))
	  ((and (interactive-p)
		(eq command 'execute-extended-command))
	    (funcall finder (symbol-name
			      (read-command "Find tag for command: "))))
	  (t
	    (funcall finder (symbol-name command))))))

;;; Other stuff.

(defun rgr-atom-subrp (atom)
  ;; helper for rgr-document-all-subrs
  (and (fboundp atom) (subrp (symbol-function atom))))

(defun rgr-document-all-subrs ()
  "List all built-in functions, by name and arglist."
  ;; Could be made faster & more useful by calling apropos-internal (a subr
  ;; itself) instead.  -- rgr, 28-Jan-95.
  (interactive)
  (let ((all-subrs nil))
    (mapatoms (function (lambda (atom)
		(if (rgr-atom-subrp atom)
		    (setq all-subrs (cons atom all-subrs))))))
    (setq all-subrs (sort all-subrs 'string<))
    (message "Found %d subrs." (length all-subrs))
    (with-output-to-temp-buffer "*subrs*"
      (let ((tail all-subrs))
	(while tail
	  (let* ((atom (car tail))
		 (doc (rgr-guess-arglist-from-documentation atom 'unknown)))
	    (cond ((eq doc 'unknown)
		    (princ (format "%S ??\n" atom)))
		  ((null doc)
		    (princ (format "%S ()\n" atom)))
		  (t
		    (princ (format "%S %S\n" atom doc)))))
	  (setq tail (cdr tail)))))))

;; Installation (also used for ilisp).  -- rgr, 26-Mar-96.

;;;###autoload
(defun rgr-define-lisp-mode-commands (map)
  ;; Install rgr command hacks, such as rgr-fill-comment.
  ;; [But use the default in Emacs 22.  -- rgr, 21-Apr-07.]
  (if (not (rgr-emacs-version-p 22))
      (define-key map "\M-q" 'rgr-fill-comment))
  (define-key map "\M-*" 'rgr-add-to-lisp-modification-history)
  (if (eq map emacs-lisp-mode-map)
      ;; For consistency with Common Lisp interfaces.  -- rgr, 8-Nov-06.
      (define-key map "\C-c\C-d" 'describe-function))
  (cond ((or (eq map emacs-lisp-mode-map)
	     (eq map lisp-interaction-mode-map))
	  ;; ["C-z c" and "C-z a" are no longer ilisp-compatible, and i'm using
	  ;; slime these days anyway.  -- rgr, 4-Sep-06.]
	  (define-key map "\C-c\C-c" 'compile-defun)
	  (define-key map "\C-c\C-a" 'rgr-quick-show-elisp-arglist)))
  ;; [kludge:  this is wrong, but we'll fix it later.  -- rgr, 26-Jul-01.]
  ;; [actually, xemacs doesn't seem to bind this.  -- rgr, 26-Jul-01.]
  (if (and (not (eq rgr-emacs-flavor 'xemacs))
	   (rgr-emacs-version-p 19))
      ;; Override mark-sexp (C-M-Space) so it doesn't shadow the binding made by
      ;; the rgr-install-global-hacks function.
      (define-key map [?\M-\C- ] nil))
  ;; Undo stupid ilisp binding of close-all-lisp to "]".  Useless, and it makes
  ;; the [] brackets harder to type.  -- rgr, 25-Jan-95.
  (define-key map "]" 'self-insert-command)
  ;; Make RET also indent, as ^J (the newline character) does defaultly, by
  ;; swapping the command bindings.  This is also Lispm standard doctrine.
  ;; [actually not, but i once had habit of hitting the lispm 'line' key instead
  ;; of return, and the sun doesn't have a line key at all.  -- rgr, 13-Apr-94.]
  (define-key map "\r" 'newline-and-indent)
  (define-key map "\n" 'newline))

;;;###autoload
(defun rgr-debugger-mode-hook ()
  ;; debugger-mode is used in *Backtrace* buffer.  Unfortunately, debugger-mode
  ;; doesn't run a hook, so we can't install this.
  ;; [This will be fixed in Emacs 22.  -- rgr, 4-Sep-06.]
  (define-key debugger-mode-map "\C-c\C-a" 'rgr-quick-show-elisp-arglist))

(provide 'rgr-elisp-hacks)

