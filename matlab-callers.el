;;;; Callers.
;;
;; [this is experimental; should be given a new home.  -- rgr, 21-Sep-99.]
;; [superseded by the ~rogers/projects/matlab/parse.lisp code; matlab is too
;; squishy a language to be parsed unambiguously unless you happen to be
;; interpreting it at the time.  -- rgr, 9-Mar-00.]
;;

(defun bmerc-find-matlab-primitives-internal (path)
  (let ((files (directory-files path t nil t)))
    (while files
      (let ((file (car files)))
	(cond ((and (eq (car (file-attributes file)) t)
		    ;; don't redo our path names
		    (not (string-match "/\\.\\.?$" file))
		    ;; don't find files in object directories.
		    ;; [why not?  -- rgr, 4-Jan-99.]
		    ;; (not (string-match "@" file))
		    )
		(bmerc-find-matlab-primitives-internal (concat file "/")))
	      ((string-match "/\\([^/]+\\)\\.m$" file)
		(let ((name (intern (match-string 1 file))))
		  (or (member name bmerc-matlab-primitive-functions)
		      (setq bmerc-matlab-primitive-functions
			    (cons name bmerc-matlab-primitive-functions)))))))
      (setq files (cdr files)))))

(defun bmerc-find-matlab-primitives (paths)
  (message "Finding matlab primitives on paths...")
  (setq bmerc-matlab-primitive-functions nil)
  (mapcar (function bmerc-find-matlab-primitives-internal) paths)
  (message "Finding matlab primitives on paths...done; %d functions"
	   (length bmerc-matlab-primitive-functions))
  bmerc-matlab-primitive-functions)

(defvar bmerc-matlab-primitive-functions
	nil ;; (bmerc-find-matlab-primitives matlab-mode-install-path)
  "Obsolete; replaced by the ~rogers/projects/matlab/parse.lisp code.")

;; (member 'size bmerc-matlab-primitive-functions)

(defun bmerc-matlab-extract-name-before-point ()
  (save-excursion
    (skip-chars-backward " \t\n")
    (let ((end (point)))
      (skip-chars-backward "a-zA-Z0-9_")
      (if (= (point) end)
	  ;; No identifier before this paren; probably a parenthesized
	  ;; subexpression.
	  nil
	  ;; Found a valid identifier.
	  (intern (buffer-substring (point) end))))))

(defun bmerc-matlab-var-assignment-p ()
  ;; return non-nil if the variable (named before point) is getting assigned.
  ;; only handles scalar assignment at this point.
  (save-excursion
    (skip-chars-forward " \t\n")
    (and (= (char-after (point)) ?\()
	 ;; skip subscript.
	 (forward-sexp))
    (looking-at "[ \t\n]*=")))

(defun bmerc-matlab-show-called-functions (&optional show-primitives-too-p)
  "Numeric arg to show primitives, too."
  ;; [Can't completely distinguish variables from functions, though, since
  ;; parens are also used for subscripts (though we do rule out assignments to
  ;; subscripted variables).  Keeping track of passed & returned arguments (in
  ;; the "function" line) would also help.  But we might just have to assume
  ;; that if we can't find a .m file, then its a variable. -- rgr, 4-Jan-99.]
  (interactive "P")
  (let ((functions nil) (variables nil))
    (save-excursion
      (goto-char (point-min))
      ;; Primitive matlab lexing.
      (skip-chars-forward "^(%'")
      (while (not (eobp))
	(let ((char (char-after (point))))
	  (cond ((= char ?\()
		  ;; function
		  (let ((name (bmerc-matlab-extract-name-before-point)))
		    (message "found %S " name)
		    (cond ((or (null name)
			       ;; The already-done case.  Don't check whether
			       ;; its on functions yet, because we might decide
			       ;; later it was a variable after all.
			       (member name variables)))
			  ((bmerc-matlab-var-assignment-p)
			    ;; (message "Found variable %S" name)
			    ;; (sit-for 1)
			    (setq variables (cons name variables))
			    ;; Now we know better.
			    (setq functions (delete name functions)))
			  ((or (member name functions)
			       (and (not show-primitives-too-p)
				    (member name
					    bmerc-matlab-primitive-functions))))
			  (t
			    ;; define new name.
			    (setq functions (cons name functions)))))
		  ;; Skip the paren.
		  (forward-char))
		((= char ?\%)
		  ;; comment
		  (forward-line))
		(t
		  ;; quoted string.  [***bug***: the transpose operator is an
		  ;; unmatched single quote!  -- rgr, 5-Jan-99.]
		  (forward-char)
		  (skip-chars-forward "^'")
		  (forward-char))))
	(skip-chars-forward "^(%'")))
    ;; Got 'em.
    (if (null functions)
	(message "No functions called in this .m file.")
	(with-output-to-temp-buffer (concat "*" (buffer-name) " fns*")
	  (setq functions (sort functions (function string<)))
	  (let ((tail functions))
	    (while tail
	      (princ "  ")
	      (princ (symbol-name (car tail)))
	      (princ "\n")
	      (setq tail (cdr tail))))
	  (message "%d functions called in this .m file." (length functions))))
    functions))
