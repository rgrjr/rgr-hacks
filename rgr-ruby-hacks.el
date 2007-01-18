;;;*****************************************************************************
;;;
;;;; Hacking ruby-mode.
;;;
;;; Copyright (C) 2006 Robert G. Rogers Jr
;;;
;;; [created.  -- rgr, 1-Jan-07.]
;;;
;;; $Id$

(defun ruby-backward-up-list (&optional arg)
  "Like backward-up-list, but handles nested Ruby constructs.
Unfortunately, it uses ruby-backward-sexp, which get confused by strings,
comments, and regular expressions, so it often gets lost."
  (interactive "p")
  (or arg (setq arg 1))
  (if (< arg 0)
      (error "ruby-backward-up-list:  Can't handle arg %S." arg))
  (let ((original-start (point))
	(block-start-re
	  "\\(def\\|class\\|module\\|while\\|do\\|{\\|if\\|elsif\\)"))
    (while (> arg 0)
      (cond ((bobp)
	      ;; Don't let this affect point.
	      (goto-char original-start)
	      (error "At top level")))
      (let ((start-point (point)))
	(ruby-backward-sexp 1)
	(if (and (looking-at block-start-re)
		 (not (save-excursion
			(beginning-of-line)
			(looking-at "^[ \t]*#")))
		 (>= (save-excursion
		       (beginning-of-line)
		       (ruby-forward-sexp 1)
		       (point))
		    start-point))
	    (setq arg (1- arg)))))))

(defvar rgr-ruby-def-name-re
	"^[ \t]*\\(def\\|class\\|module\\) +\\([^ \t\n<>()]+\\)")

(defun rgr-ruby-enclosing-class-or-module-name ()
  "Helper for rgr-ruby-def-name, below."
  (save-excursion
    (let ((start-point (point)) (name nil))
      (while (and (null name)
		  (re-search-backward rgr-ruby-def-name-re nil t))
	(if (member (intern (downcase (match-string-no-properties 1)))
		    '(class module))
	    (setq name (match-string-no-properties 2))))
      (cond (name
	      ;; Now make sure it's really for the enclosing class.
	      (beginning-of-line)
	      (ruby-forward-sexp 1)
	      (and (> (point) start-point)
		   name))))))

(defun rgr-ruby-def-name ()
  "Return the name for the current definition.  The following formats
are supported:

    Class.method    class method (if inside a def with that syntax).
    Class#method    instance method (if inside an unqualifed def).
    class Class     if inside the class but not a def.
    module Module   if inside a module but not a class or def.

\[bug:  can't yet handle situations where the name should be
'Module::Class'.  -- rgr, 1-Jan-07.]"
  (let ((start-point (point)) (name nil) (class nil) (type nil))
    (save-excursion
      (cond ((not (re-search-backward rgr-ruby-def-name-re nil t))
	      nil)
	    ((member (setq type (intern (downcase (match-string 1))))
		     '(class module))
	      ;; Class or module body.
	      (concat (match-string 1) " " (match-string 2)))
	    ((< (save-excursion
		  (save-match-data
		    (beginning-of-line)
		    (ruby-forward-sexp 1)
		    (point)))
		start-point)
	      ;; This def doesn't contain the starting point, so we must be at
	      ;; top level (i.e. between defs) in the class.
	      (rgr-ruby-enclosing-class-or-module-name))
	    ((string-match "\\." (setq name (match-string 2)))
	      ;; Class method.
	      name)
	    (t
	      ;; Instance method.
	      (let ((class-name (rgr-ruby-enclosing-class-or-module-name)))
		(if class-name
		    ;; use the PickAxe convention for class methods.
		    (concat class-name "#" name)
		    name)))))))

;;;###autoload
(defun rgr-ruby-mode-hook ()
  (define-key ruby-mode-map "\C-\M-u" 'ruby-backward-up-list)
  (define-key ruby-mode-map "-" 'rgr-c-electric-dash)
  (define-key ruby-mode-map "\r" 'newline-and-indent)
  (define-key ruby-mode-map "\n" 'newline)
  (setq ruby-indent-level 4)
  (setq fill-column 78)
  ;; (setq ruby-continued-statement-offset 4)
  ;; (setq ruby-close-paren-offset 0)
  )

(provide 'rgr-ruby-hacks)

;; End of rgr-ruby-hacks.el
