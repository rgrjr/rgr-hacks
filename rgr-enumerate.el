;;;; Hacks for creating and manipulating numbered paragraphs.
;;;
;;; [created.  -- rgr, 23-Feb-04.]
;;;
;;; $Id$

;;;###autoload
(defun rgr-renumber-region-paragraphs (start end)
  "If the region contains a series of numbered paragraphs, renumber them
sequentially, beginning with the first."
  (interactive "r")
  (let ((index nil))
    (save-excursion
      (goto-char start)
      (while (re-search-forward "^[ \t]+\\([0-9]+\\)\\. " end t)
	(if index
	    (replace-match (format "%d" (setq index (1+ index)))
			   t t nil 1)
	    (setq index (string-to-int (match-string 1))))))))

;;;###autoload
(defun rgr-enumerate-region-sentences (start end)
  "Turn the sentences in the region into a series of numbered paragraphs.
The paragraphs are numbered from 1; if you wish to change this, use the
\\[rgr-renumber-region-paragraphs] command afterwards."
  (interactive "r")
  (save-excursion
    (let ((index 0)
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
	  (backward-paragraph 2)
	  (fill-paragraph nil))))))

(provide 'rgr-enumerate)

