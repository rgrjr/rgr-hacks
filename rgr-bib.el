;;;; Hacking bibliographies.
;;;
;;;    See, for example, the ~/projects/papers/bibliography.bib file.
;;;
;;;    Modification history:
;;;
;;; created.  -- rgr, 16-Sep-99.
;;;

(defun rgr-bib-fix-one-author (author)
  ;; Just fix names with commas.
  (cond ((not (string-match "[ \t\n]*,[ \t\n]*" author))
	  author)
	(t
	  (let ((first-names (substring author (match-end 0)))
		(last-name (substring author 0 (match-beginning 0))))
	    (if (string-match "^[^{].* .*[^}]$" last-name)
		(setq last-name (concat "{" last-name "}")))
	    (concat first-names " " last-name)))))

(defun rgr-bib-fix-author-names (string)
  ;; Given a string of author (or editor) names, canonicalize them.
  (let ((and-regexp "[ \t\n]+and[ \t\n]+"))
    (if (string-match and-regexp string)
	(let ((pos (match-end 0))
	      (result (rgr-bib-fix-one-author
		        (substring string 0 (match-beginning 0)))))
	  (while (string-match and-regexp string pos)
	    (let ((author (substring string pos (match-beginning 0))))
	      (setq pos (match-end 0))
	      (setq result (concat result " and "
				   (rgr-bib-fix-one-author author)))))
	  (concat result
		  " and "
		  (rgr-bib-fix-one-author (substring string pos))))
	;; single author
	(rgr-bib-fix-one-author string))))

(defun rgr-fill-between (text indentation fill-col)
  ;; Return the text filled between the indentation and the fill column,
  ;; omitting the initial indentation on the first line and the trailing
  ;; newline.
  (save-excursion
    (set-buffer (get-buffer-create "*indent*"))
    (erase-buffer)
    (insert text)
    (let ((adaptive-fill-mode nil)
	  (fill-prefix nil)
	  (fill-column (- fill-col indentation)))
      (fill-region-as-paragraph (point-min) (point-max))
      (indent-rigidly (point-min) (point-max) indentation))
    (goto-char (point-min))
    (skip-chars-forward " \t")
    (buffer-substring (point) (1- (point-max)))))

;;;###autoload
(defun rgr-bib-fix-next-author ()
  "Find and normalize the next author or editor entry."
  (interactive)
  (or (re-search-forward "\\(author\\|editor\\)[ \t\n]*=[ \t\n]*" nil t)
      (error "No more author or editor fields."))
  (goto-char (match-end 0))
  (let* ((start (if (eq (char-after (point)) ?\{)
		    (1+ (point))
		    (point)))
	 (end (save-excursion
		(forward-sexp)
		(if (eq (char-after (1- (point))) ?\})
		    (1- (point))
		    (point))))
	 (old-authors (buffer-substring start end))
	 (new-authors (rgr-fill-between (rgr-bib-fix-author-names old-authors)
					(progn (goto-char start)
					       (current-column))
					fill-column)))
    (cond ((equal old-authors new-authors)
	    (message "(no change)"))
	  (t 
	    (delete-region start end)
	    (insert new-authors)))))

