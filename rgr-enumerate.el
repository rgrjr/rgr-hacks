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
      (while (re-search-forward "^ +\\([0-9]+\\)\\. " end t)
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
  (let ((index 0))
    (save-excursion
      (goto-char start)
      (if (looking-at "^  +")
	  (replace-match (format "\n   %d.  " (setq index (1+ index)))))
      (while (re-search-forward
	       "\\([.?!][]\"')}]*\\)\\($\\| $\\|\t\\|  \\)\\([ \t\n]*\\)"
	       end t)
	(replace-match (concat (match-string 1)
			       (format "\n\n   %d.  "
				       (setq index (1+ index)))))
	(save-excursion
	  (backward-paragraph 2)
	  (fill-paragraph nil))))))

(provide 'rgr-enumerate)
