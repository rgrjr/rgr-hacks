;;;****************************************************************************
;;;
;;;    GNU Emacs hackery.
;;;
;;;    [This also has ilisp hacks here, because I'm too lazy to separate them
;;; out.  -- rgr, 28-Jan-00.]
;;;
;;;    To compile (almost) without error, do (mapc 'require '(ilisp ilisp-src)).
;;;
;;;    Modification history:
;;;
;;; split out of the rgr-hacks.el file.  -- rgr, 20-Nov-98.
;;; rgr-comment-region-lisp: new.  -- rgr, 7-Sep-99.
;;; rgr-lisp-def-name: avoid loading ilisp.  -- rgr, 15-Sep-99.
;;; rgr-ilisp-mode-hook: move here, restore comint bindings.  -- rgr, 28-Jan-00.
;;; rgr-cmucl-ilisp-display-output: new.  -- rgr, 28-Feb-00.
;;; don't use rgr-cmucl-ilisp-display-output in ilisp 5.10.1.  -- rgr, 6-Dec-00.
;;; rgr-ilisp-mode-hook: ilisp-display-output-function here.  -- rgr, 7-Dec-00.
;;; rgr-ilisp-mode-hook: cmulisp-local-source-directory.  -- rgr, 27-Dec-00.
;;; rgr-ilisp-mode-hook: output suppression for v9 and v10.  -- rgr, 29-Dec-00.
;;; rgr-lisp-def-name: teach this about defmethod.  -- rgr, 9-Nov-01.
;;; rgr-lisp-snarf-definition-name: fix a syntax issue.  -- rgr, 17-Jan-02.
;;; rgr-lisp-def-name: obey namep for methods.  -- rgr, 27-Feb-02.
;;; work around PCL *load-truename* problem.  -- rgr, 28-Aug-02.
;;; rgr-cmulisp-mode-hook: more pathname fixes.  -- rgr, 1-Sep-02.
;;;

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

;;;; CMU-CL specific stuff

(defun rgr-cmucl-flush-noise (string)
  ;; Take uninteresting boilerplate out of Python (CMU-CL) compiler messages.
  (cond ((string-match "Compiling LAMBDA NIL: *\nCompiling Top-Level Form: *\n"
		       string)
	  ;; Got a compiler message.  If both string-match'es hit, this should
	  ;; leave just the compiled function name.
	  (setq string (replace-match "" t t string))
	  (if (string-match "Converted .+\\.\n" string)
	      (setq string (replace-match "" t t string)))))
  ;; Now flush leading whitespace.
  (if (string-match "\\`[ \t\n]+" string)
      (setq string (replace-match "" t t string)))
  string)

(defun rgr-cmucl-ilisp-9-display-output (string)
  ;; use the default display technique, but flush noise first.  [for ilisp
  ;; version 9 and earlier.  -- rgr, 29-Dec-00.]
  (ilisp-display-output-default (rgr-cmucl-flush-noise string)))

(defun rgr-cmucl-ilisp-10-display-output (string output-sink)
  ;; use the default display technique, but flush noise first.  [for ilisp
  ;; version 10 and later.  -- rgr, 29-Dec-00.]
  (ilisp-display-output-default (rgr-cmucl-flush-noise string) output-sink))

;;;; ilisp hackery.

(defun rgr-cmulisp-mode-hook ()
  ;; Fix source file recording braindeath.
  (setq ilisp-source-directory-fixup-alist
	'(;; [need to suss out CLC source file recording.  -- rgr, 1-Sep-02.]
	  ("/usr/lib/common-lisp/cmucl-normal/clsql/\\(.*\\)/bin/\\(.*\\)\\.x86f"
	   . "/usr/share/common-lisp/repositories/clsql-0.7.1/\\1/\\2.cl")
	  ;; [this is probably due to moving something around after compiling.
	  ;; -- rgr, 1-Sep-02.]
	  ("/usr/share/common-lisp/source/clsql/"
	   . "/usr/share/common-lisp/repositories/clsql-0.7.1/")
	  ;; [this is necessary because PCL in 18d remembers *load-truename*
	  ;; instead of *load-pathname* when defining things.  -- rgr,
	  ;; 28-Aug-02.]
	  ("/prj/cmucl/release-18d/linux/\\(.*\\)\\.x86f"
	   . "/usr/local/src/cmucl-18d/src/\\1.lisp"))))

(add-hook 'cmulisp-hook 'rgr-cmulisp-mode-hook)

;;;###autoload
(defun rgr-ilisp-mode-hook ()
  (setq ilisp-mode-hook nil)
  (setq lisp-mode-hook nil)
  (setq ilisp-*prefix* "\C-c")
  (setq ilisp-*use-fsf-compliant-keybindings* t))

(defun rgr-old-ilisp-mode-hook ()
  ;; [this disables the annoying hack where ilisp grovels through all buffers if
  ;; the lisp doesn't know where to find the definition.  though, since cmucl
  ;; loses interactive definition source files, this may cause trouble.  -- rgr,
  ;; 30-Jul-02.]  [no longer true; patched in ilisp.  -- rgr, 28-Aug-02.]
  ;; [ilisp M-. now ignores this setting.  -- rgr, 27-Dec-03.]
  ;; (setq lisp-edit-files nil)
  ;; [this doesn't seem to work -- ilisp-mode-map is nil.  -- rgr, 5-Apr-94.]
  ;; [so we need this incantation to build it.  (ilisp-mode-map contains key
  ;; bindings for the inferior lisp.)  -- rgr, 22-Dec-99.]  [no; put it on
  ;; ilisp-mode-hook instead.  -- rgr, 28-Jan-00.]
  (or ilisp-mode-map
      (ilisp-bindings))
  (define-key ilisp-mode-map "\M-q" 'rgr-fill-comment)
  ;; get rid of shadowing C-M-l binding.  [in ilisp 5.9.  -- rgr, 22-Dec-99.]
  (define-key ilisp-mode-map "\C-\M-l" 'comint-show-output)
  (define-key ilisp-mode-map "\C-c\r" 'comint-copy-old-input)
  ;; don't know why i need this too . . .
  (define-key ilisp-mode-map [?\C-c return] 'comint-copy-old-input)
  ;; [this needs better installation; we don't need it for allegro, for
  ;; instance.  -- rgr, 28-Feb-00.]  [it also breaks ilisp version 5.10.1, in
  ;; which ilisp-display-output-default requires two args.  -- rgr, 6-Dec-00.]
  (setq ilisp-display-output-function
	(if (and (boundp 'ilisp-*version*)
		 (string-match "^5\\.[1-9][0-9]" ilisp-*version*))
	    'rgr-cmucl-ilisp-10-display-output
	    'rgr-cmucl-ilisp-9-display-output)))

;;;###autoload
(defun rgr-lisp-mode-hook ()
  (rgr-define-lisp-mode-commands lisp-mode-map)
  ;; Undo these, which don't add much, and can shadow good find-file-at-point
  ;; things (when running at home).  -- rgr, 6-Feb-98.  [bug: ilisp-mode-map is
  ;; not defined until ilisp is loaded.  -- rgr, 8-Feb-98.]
  (define-key lisp-mode-map "\C-x\C-f" nil)
  ;(define-key ilisp-mode-map "\C-x\C-f" nil)
  ;; ilisp, curse its soul, bashes this to the wrong thing.
  (rgr-define-lisp-mode-commands emacs-lisp-mode-map)
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

