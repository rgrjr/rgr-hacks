;;;*****************************************************************************
;;;
;;;; Hacking python mode.
;;;
;;; Copyright (C) 2009 Robert G. Rogers Jr
;;;
;;; This file is not part of GNU Emacs, but it customizes python-mode, which is.
;;; Accordingly, it is distributed under the same terms as GNU Emacs; in emacs
;;; type "C-h C-c" for detailed license terms.
;;;
;;; [created.  -- rgr, 14-Mar-09.]
;;;
;;; $Id:$

(defvar rgr-python-def-name-re
	"^[ \t]*\\(def\\|class\\) +\\([a-zA-Z0-9_]+\\)")

(defun python-forward-sexp (&optional count)
  (interactive "p")
  (or count
      (setq count 1))
  (beginning-of-line)
  (back-to-indentation)
  (if (< count 0)
      (error "backwards is not supported."))
  (let ((indentation (current-indentation)))
    (while (and (not (eobp))
		(> count 0))
      (forward-line)
      (while (and (not (eobp))
		  (or (python-comment-line-p)
		      (python-blank-line-p)
		      (> (current-indentation) indentation)))
	(forward-line))
      (setq count (1- count)))
    count))

(defun rgr-python-enclosing-class-or-module-name ()
  "Helper for rgr-python-def-name, below."
  (save-excursion
    (let ((start-point (point)) (name nil))
      (while (and (null name)
		  (re-search-backward rgr-python-def-name-re nil t))
	(if (member (intern (downcase (match-string-no-properties 1)))
		    '(class def))
	    (setq name (match-string-no-properties 2))))
      (cond (name
	      ;; Now make sure it's really for the enclosing class.
	      (beginning-of-line)
	      (python-forward-sexp 1)
	      (and (> (point) start-point)
		   name))))))

(defun rgr-python-def-name ()
  ;; [not complete.  -- rgr, 17-Mar-09.]
  "Return the name for the current definition.  The following formats
are supported:

    Class.method    class method (if inside a def with that syntax).
    Class#method    instance method (if inside an unqualifed def).
    class Class     if inside the class but not a def.
    method          if inside method but not a class.
"
  (let ((start-point (point)) (name nil) (class nil) (type nil))
    (save-excursion
      (cond ((not (re-search-backward rgr-python-def-name-re nil t))
	      nil)
	    ((eq (setq type (intern (downcase (match-string 1)))) 'class)
	      ;; Class body.
	      (concat (match-string 1) " " (match-string 2)))
	    ((eq type 'def)
	      ;; Def body.
	      (match-string 2))
	    ((< (save-excursion
		  (save-match-data
		    (beginning-of-line)
		    (python-forward-sexp 1)
		    (point)))
		start-point)
	      ;; This def doesn't contain the starting point, so we must be at
	      ;; top level (i.e. between defs) in the class.
	      (rgr-python-enclosing-class-or-module-name))
	    ((string-match "\\." (setq name (match-string 2)))
	      ;; Class method.
	      name)
	    (t
	      ;; Instance method.
	      (let ((class-name (rgr-python-enclosing-class-or-module-name)))
		(if class-name
		    ;; use the PickAxe convention for class methods.
		    (concat class-name "#" name)
		    name)))))))

(put 'python-mode 'mode-definition-name 'rgr-python-def-name)

;;;###autoload
(defun rgr-python-mode-hook ()
  (define-key python-mode-map "\r" 'newline-and-indent)
  (define-key python-mode-map "\n" 'newline)
  (define-key python-mode-map "\C-\M-f" 'python-forward-sexp)
  (define-key python-mode-map "\M-q" 'rgr-fill-script-comment)
  ;; Try to avoid shifting.  -- rgr, 20-Dec-96.
  (define-key python-mode-map "-" 'rgr-c-electric-dash))

(add-hook 'python-mode-hook 'rgr-python-mode-hook)

(provide 'rgr-python-hacks)
