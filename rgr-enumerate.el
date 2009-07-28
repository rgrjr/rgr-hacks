;;;; Hacks for creating and manipulating numbered paragraphs.
;;;
;;; [created.  -- rgr, 23-Feb-04.]
;;;
;;; $Id$

(defun rgr-renumber-explode-dots (string)
  (let ((result nil) (start 0))
    (save-match-data
      (while (string-match "\\([0-9]+\\)\\.?" string start)
	(setq result (cons (string-to-number (match-string 1 string)) result))
	(setq start (match-end 0))))
    (nreverse result)))

;; (rgr-renumber-explode-dots "3.5.7")

;;;###autoload
(defun rgr-renumber-region-paragraphs (start end)
  "If the region contains a series of numbered paragraphs, renumber them
sequentially, beginning with the first.  Numbers must have a trailing
dot, and may consist of two or more dot-separated subcomponents, which
are incremented lexicographically."
  (interactive "r")
  (let ((last nil))
    (save-excursion
      (goto-char start)
      (while (re-search-forward "^[ \t]+\\(\\([0-9.]+\\.\\)?[0-9]+\\)\\. "
				end t)
	(let ((dots (rgr-renumber-explode-dots (match-string 1))))
	  (cond ((not dots))
		((not last)
		  (setq last dots))
		(t
		  (let ((ll (length last)) (ld (length dots)))
		    (cond ((> ld ll)
			    ;; down to a lower level; take the initial value of
			    ;; the new dotted component(s) as given.
			    (setcdr (nthcdr (1- ll) last)
				    (nthcdr (1- ll) dots)))
			  (t
			    (if (> ll ld)
				;; up to a higher level
				(setcdr (nthcdr (1- ld) last) nil))
			    (setcar (nthcdr (1- ld) last)
				    (1+ (car (nthcdr (1- ld) last))))))
		    (replace-match (mapconcat (function (lambda (x)
						(format "%d" x)))
					      last ".")
				   t t nil 1)))))))))

;;;###autoload
(defun rgr-enumerate-region-sentences (start end)
  "Turn the sentences in the region into a series of numbered paragraphs.
The paragraphs are numbered from 1; if you wish to change this, use the
\\[rgr-renumber-region-paragraphs] command afterwards."
  (interactive "r")
  (save-excursion
    (let ((adaptive-fill-mode nil)
	  (index 0)
	  (end-of-sentence-re
	    (concat "\\([.?!][]\"')}]*\\)\\($\\| $\\|\t\\|  \\)\\([ \t\n]*\\)"
		    ;; this prevents us from numbering an empty sentence at the
		    ;; end of the region.
		    "\\([^ \t\n]\\)"))
	  (end (set-marker (make-marker) end)))
      (goto-char start)
      (while (looking-at "^[ \t]*$")
	(forward-line))
      (if (looking-at "^  +")
	  (replace-match (format "   %d.  " (setq index (1+ index)))))
      (while (re-search-forward end-of-sentence-re end t)
	(replace-match (concat (match-string 1)
			       (format "\n\n   %d.  "
				       (setq index (1+ index)))
			       (match-string 4))
		       t t)
	(save-excursion
	  (backward-paragraph 1)
	  (fill-paragraph nil))))))

(provide 'rgr-enumerate)

