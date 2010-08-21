;;;****************************************************************************
;;;
;;;    GNU emacs mouse hackery.
;;;
;;; $Id$

(defun rgr-print-table (table)
  "Formatting hack, for documenting mouse commands."
  (let ((min-widths (make-list (length (car table)) 0))
	(table-tail table))
    (while table-tail
      (let ((line-tail (car table-tail)) (width-tail min-widths))
	(while (and line-tail width-tail)
	  (setcar width-tail (max (car width-tail)
				  (length (car line-tail))))
	  (setq width-tail (cdr width-tail) line-tail (cdr line-tail))))
      (setq table-tail (cdr table-tail)))
    (setq table-tail table)
    (while table-tail
      (let ((line-tail (car table-tail)) (width-tail min-widths))
	(while line-tail
	  (let* ((item (car line-tail)) (min-width (or (car width-tail) 0))
		 (len (length item)))
	    (setq width-tail (cdr width-tail) line-tail (cdr line-tail))
	    (if item
		(princ item))
	    (cond (line-tail
		    (while (< len min-width)
		      (write-char ?\ )
		      (setq len (1+ len)))
		    (princ "  "))))))
      (setq table-tail (cdr table-tail))
      (terpri))))

(defun rgr-print-map-internal (keymap prefix)
  (let ((tail (cdr keymap)))
    (if (arrayp (car tail))
	(setq tail (cdr tail)))
    (while tail
      (let ((entry (car tail)))
	(if (consp entry)
	    (let* ((key (car entry)) (binding (cdr entry))
		   (sequence (vconcat prefix (vector key)))
		   (key-string
		     ;; (if (numberp key) (make-string 1 key) (format "%s" key))
		     (key-description sequence)))
	      (cond ((atom binding)
		      (insert "\n" key-string
			      (format ": %S" binding)))
		    ((eq (car binding) 'keymap)
		      (rgr-print-map-internal binding sequence))
		    ((and (consp (cdr binding))
			  (eq (car (cdr binding)) 'keymap))
		      ;; menu
		      (insert (format "\n*** begin %S ***" (car binding)))
		      (rgr-print-map-internal (cdr binding) sequence)
		      (insert (format "\n*** end %S ***" (car binding))))
		    (t
		      ;; [don't know what to make of this.  -- rgr, 24-Jan-95.]
		      (insert "\n" key-string 
			      (format ": %S" binding)))))))
      (setq tail (cdr tail)))))

(defun rgr-print-command-table (cmds &optional sort-predicate)
  (if sort-predicate
      (setq cmds (sort cmds sort-predicate)))
  (with-output-to-temp-buffer "*Help*"
    (with-current-buffer "*Help*"
      (make-local-variable 'truncate-lines)
      (setq truncate-lines t))
    (princ "Mouse commands:\n\n")
    (rgr-print-table (cons '("Gesture" "Command name" "Documentation") cmds))))

(defun rgr-find-mouse-commands (map)
  ;; Helper for rgr-document-mouse-commands, below.  Needless to say, this can
  ;; afford to skip the key vector, if (arrayp (car (cdr map))).
  (let ((cmds nil) (tail (cdr map)))
    (if (arrayp (car tail))
	(setq tail (cdr tail)))
    (while tail
      (let ((entry (car tail)))
	(if (consp entry)
	    (let ((key (car entry)) (cmd (cdr entry)))
	      (if (and (symbolp key) cmd (symbolp cmd)
		       (string-match "mouse" (symbol-name key)))
		  (setq cmds
			(cons (list (concat (key-description (vector key)) ":")
				    (symbol-name cmd)
				    (let ((doc (documentation cmd)))
				      (if (and doc (string-match "\n" doc))
					  (substring doc 0 (match-beginning 0))
					  doc)))
			      cmds))))))
      (setq tail (cdr tail)))
    cmds))

;;;###autoload
(defun rgr-document-mouse-commands ()
  "Print the list of all currently-bound mouse commands and their gestures."
  (interactive)
  (let ((cmds (nconc (rgr-find-mouse-commands (current-global-map))
		     (rgr-find-mouse-commands (current-local-map))))) 
    (rgr-print-command-table cmds
			     (function (lambda (a b)
			       (string< (car a) (car b)))))))

(provide 'rgr-mouse-doc)
