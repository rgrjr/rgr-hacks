;;; tcl mode hacking.
;;;
;;;    Modification history:
;;;
;;; created.  -- rgr, 9-Aug-99.
;;; set up hookage properly for autoloading.  -- rgr, 11-Aug-99.
;;; conditionally defined rgr-c-electric-dash.  -- rgr, 12-Aug-99.
;;; gratuitous change to test discus.  -- rgr, 8-Sep-99.
;;; don't assume rgr-abbrev-major-modes-that-have-definitions is bound.
;;;	-- rgr, 3-Oct-99.
;;; rgr-tcl-mode-hook: bind rgr-c-electric-dash always.  -- rgr, 9-Nov-99.
;;;

(if (and (boundp 'rgr-abbrev-major-modes-that-have-definitions)
	 (not (member 'tcl-mode rgr-abbrev-major-modes-that-have-definitions)))
    (setq rgr-abbrev-major-modes-that-have-definitions
	  (cons 'tcl-mode rgr-abbrev-major-modes-that-have-definitions)))

;;;###autoload
(defun rgr-tcl-mode-hook ()
  (interactive) ;; debugging
  ;; commenting and filling
  (setq fill-column 80)
  (setq comment-start "#")
  (set (make-local-variable 'lisp-comment-fill-column) 80)
  (set (make-local-variable 'rgr-definition-line-regexp)
       "^[ \t]*\\(ad_\\)?proc\\(_doc\\)?")
  (define-key tcl-mode-map "\M-q" 'rgr-fill-comment)
  ;; other commands
  (define-key tcl-mode-map "\r" 'newline-and-indent)
  (setq tcl-electric-hash-style nil)
  (define-key tcl-mode-map "-" 'rgr-c-electric-dash))

