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
;;;    To compile this (almost) without error, do the following:
;;;
;;; (progn (mapcar 'require '(discus)) (load "debug"))
;;;
;;; Discus *must* be loaded, so that with-discus-log-output will be expanded
;;; correctly.  -- rgr, 25-Jan-95.
;;;
;;;    Modification history:
;;;
;;; started history.  -- rgr, 2-Mar-94.
;;; rgr-show-elisp-arglist, etc.  -- rgr, 13-Apr-94.
;;; rgr-quick-show-elisp-arglist  -- rgr, 15-Sep-94.
;;; teach rgr-elisp-arglist about macros.  -- rgr, 22-Sep-94.
;;; rgr-count-code-lines: new fn.  -- rgr, 2-Nov-94.
;;; rgr-find-lisp-fill-prefix: made smarter.  -- rgr, 10-Nov-94.
;;; *** emacs 19 update ***
;;; rgr-guess-arglist-from-documentation, . . .  -- rgr, 25-Jan-95.
;;; rgr-document-all-subrs: new hack.  -- rgr, 28-Jan-95.
;;; moved discus-emacs-update here.  -- rgr, 31-Jan-95.
;;; discus-emacs-update: cleaned up a little.  -- rgr, 15-Feb-95.
;;; rgr-time-evaluation, binding compile-defun.  -- rgr, 22-Feb-95.
;;; rgr-elisp-arglist: deal with compiled macros.  -- rgr, 5-Jun-95.
;;; split elisp stuff out of ./rgr-hacks.lisp file.  -- rgr, 26-Mar-96.
;;; moved rgr-mapcar2 here, added autoloads.  -- rgr, 20-Jul-96.
;;; rgr-define-lisp-mode-commands: rgr-add-to-lisp-...  -- rgr, 13-Aug-96.
;;; rgr-make-tags-table-list-hook: add "~thread/code/stat".  -- rgr, 13-Dec-96.
;;; rgr-make-tags-table-list-hook: add seqaln package.  -- rgr, 1-Apr-97.
;;; rgr-define-lisp-mode-commands: tweak C-M-Spc rebinding.  -- rgr, 19-Jul-97.
;;; rgr-debugger-mode-hook: but no way to install it.  -- rgr, 6-Aug-97.
;;; rgr-make-tags-table-list-hook: ~psa/psa-test/bin.  -- rgr, 2-Apr-98.
;;; rgr-make-tags-table-list-hook: add needle-doc dir.  -- rgr, 4-Nov-98.
;;; flush some old code, clean up inits.  -- rgr, 10-Sep-99.
;;; rgr-find-elisp-function-tag-default: smarter default.  -- rgr, 28-Dec-99.
;;; rgr-elisp-find-tag-for-emacs-key: new.  -- rgr, 17-Dec-00.
;;; rgr-elisp-find-tag-for-emacs-key: don't use M-. key-binding; this breaks
;;;	under ilisp.  -- rgr, 27-Dec-00.
;;; try to make tags hacks work for xemacs, rgr-define-lisp-mode-commands
;;;	doesn't need to unbind C-M-Space.  -- rgr, 26-Jul-01.
;;; rgr-make-tags-table-list-hook: correct autoload problem.  -- rgr, 20-Aug-01.
;;; rgr-make-tags-table-list: don't expand nil.  -- rgr, 23-Aug-02.
;;;

;;;; elisp hackery.

;; Should also make rgr-mapc and rgr-mapc2 as well.  -- rgr, 16-Jul-96.

;;;###autoload
(defun rgr-mapcar2 (function list1 list2)
  "Like mapcar, but takes a function of two arguments, and does the
obvious thing with two lists.  Stops when it gets to the end of the
shorter list."
  (let* ((result (make-list (min (length list1) (length list2)) nil))
	 (result-tail result) (tail1 list1) (tail2 list2))
    (while result-tail
      (setcar result-tail (funcall function (car tail1) (car tail2)))
      (setq result-tail (cdr result-tail)
	    tail1 (cdr tail1) tail2 (cdr tail2)))
    result))

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
  (setq tags-table-list
	(rgr-make-tags-table-list
	  (append (list (expand-file-name "~thread/code/stat")
			;; the htdocs tree will only be available on the alphas,
			;; but rgr-make-tags-table-list will figure this out.
			;; -- rgr, 4-Nov-98.
			"/usr/local/etc/httpd/htdocs/needle-doc/new"
			;; these are two alternative names for the same place.
			(expand-file-name "~/projects/mgi/oligo")
			(expand-file-name "/shared/mgi/oligo")
			;; and this needs to come after.
			(expand-file-name "~/projects/database/code"))
		  load-path
		  (list (expand-file-name "~psa/psa-test/bin")
			;; [these are not the right places for SuSE 9.0.
			;; -- rgr, 15-Jun-04.]
		        ;; "/usr/lib/perl5/site_perl/5.6.1/Bio"
			;; "/usr/lib/perl5/site_perl/5.6.1/i586-linux/Tk"
			;; [wicked obsolete.  -- rgr, 15-Jun-04.]
			;; (expand-file-name "~thread/code/ctserv/bin")
			)))))

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

(defun rgr-analyze-run-time (start-time end-time)
  (require 'discus-date)
  (let ((time (- (cdr (discus-parse-date end-time))
		 (cdr (discus-parse-date start-time)))))
    (message "Evaluation of form took %d second%s."
	     time (if (= time 1) "" "s"))))

(defmacro rgr-time-evaluation (&rest body-forms)
  "After evaluating the body-forms, print a message telling how long."
  (` (let ((end-time nil) (start-time (current-time-string)))
       (prog1 (progn (,@ body-forms))
	 (rgr-analyze-run-time start-time (current-time-string))))))

;; Installation (also used for ilisp).  -- rgr, 26-Mar-96.

;;;###autoload
(defun rgr-define-lisp-mode-commands (map)
  ;; Install rgr command hacks, such as rgr-fill-comment.
  (define-key map "\M-q" 'rgr-fill-comment)
  (define-key map "\M-*" 'rgr-add-to-lisp-modification-history)
  (cond ((or (eq map emacs-lisp-mode-map)
	     (eq map lisp-interaction-mode-map))
	  (define-key map "\C-zc" 'compile-defun)
	  (define-key map "\C-za" 'rgr-quick-show-elisp-arglist)))
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
  (define-key debugger-mode-map "\C-za" 'rgr-quick-show-elisp-arglist))

(provide 'rgr-elisp-hacks)

