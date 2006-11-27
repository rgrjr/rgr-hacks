;;;; Changing heading nesting depth.
;;;
;;;    Modification history:
;;;
;;; . . .
;;; heuristicating TOC anchor names (not fully impl).  -- rgr, 25-Jul-96.
;;; rgr-html-add-heading-anchors: split out of TOC stuff.  -- rgr, 6-Jul-97.
;;; rgr-html-number-headings, make toc without numbers.  -- rgr, 9-Oct-97.
;;; rgr-html-increment-region-heading-nesting: new.  -- rgr, 18-Jan-98.
;;; rgr-html-increment-buffer-heading-nesting: bug fixing & reorg, don't let
;;;	rgr-collect-headings make trivial top-level lists.  -- rgr, 19-Jan-98.
;;; split out of ./rgr-html-hacks.el file.  -- rgr, 23-Mar-98.
;;; rgr-html-add-heading-anchors: flush rgr-anchor-definition-re var.  -- rgr,
;;;	16-Apr-98.
;;;

(require 'rgr-html-hacks)

(defconst rgr-heading-markup-re
    (let ((whitespace "[ \t]*"))
      (concat "\\(<" whitespace "/?" whitespace "h\\)\\([0-9]\\)\\>"))
  "Constant that mactches heading markup (as opposed to the heading
itself; see rgr-heading-re for comparison).  The first subexpression is
everything before the number, and the second subexpression is just the
number; together they make up the entire match string, important for
replace-match.")

;;;###autoload
(defun rgr-html-increment-buffer-heading-nesting
       (delta-nest &optional start end)
  "Increment or decrement the nesting level of headings in the buffer,
e.g. changing <h1> to <h2>, <h2> to <h3>, etc.  By default, the depth is
increased by one, but this can be changed with a prefix argument, which
can be negative to decrease the nesting depth.  HTML tag nesting errors
are not detected."
  (interactive "p")
  ;; (message "Start %s end %s" start end) (sit-for 2)
  (setq start (or start (point-min)))
  (setq end (or end (point-max)))
  (let ((n-opens 0) (n-closes 0)
	(delta-nest (or delta-nest 1))
	(start (min start end)) (end (max start end)))
    (save-excursion
      (goto-char start)
      (while (re-search-forward rgr-heading-markup-re end t)
	(let ((prefix (match-string 1))
	      (depth (+ delta-nest (string-to-number (match-string 2)))))
	  (cond ((>= depth 1)
		  (replace-match (format "%s%d" prefix depth) t t)
		  (if (string-match "/" prefix)
		      (setq n-closes (1+ n-closes))
		      (setq n-opens (1+ n-opens)))))))
      (cond ((not (= n-closes n-opens))
	      (message "Warning:  Changed %d opens and %d closes!"
		       n-opens n-closes)
	      (sit-for 2))
	    (t
	      (message "Incremented %d headings by %d."
		       n-opens delta-nest))))))

;;;###autoload
(defun rgr-html-increment-region-heading-nesting (delta-nest)
  "Increment or decrement the nesting level of headings in the buffer,
e.g. changing <h1> to <h2>, <h2> to <h3>, etc.  By default, the depth is
increased by one, but this can be changed with a prefix argument, which
can be negative to decrease the nesting depth.  HTML tag nesting errors
are not detected."
  (interactive "p")
  (rgr-html-increment-buffer-heading-nesting delta-nest (point) (mark)))

(defun rgr-section-number-string (indices)
  ;; helper for rgr-html-number-headings, which gives us the section numbers as
  ;; a list in reverse order.  -- rgr, 9-Oct-97.
  (cond ((null indices) "")
	((null (cdr indices)) (format "%d" (car indices)))
	(t
	  (concat (rgr-section-number-string (cdr indices))
		  (format ".%d" (car indices))))))

;;;###autoload
(defun rgr-html-number-headings ()
  "Scan the buffer for headings, putting section numbers on them."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((level-stack nil) (index-stack nil) (last-level -1))
      (while (re-search-forward rgr-heading-re nil t)
	(let ((level (string-to-number (buffer-substring-no-properties
					 (match-beginning 1)
					 (match-end 1))))
	      (heading (buffer-substring-no-properties (match-beginning 2)
						       (match-end 2))))
	  (cond ((> level last-level)
		  ;; increase in nesting level.
		  (setq level-stack (cons last-level level-stack))
		  (setq index-stack (cons 1 index-stack))
		  (setq last-level level))
		((and level-stack (< level last-level))
		  ;; increase in nesting level.
		  (while (and level-stack (< level last-level))
		    (setq index-stack (cdr index-stack))
		    (setq last-level (car level-stack))
		    (setq level-stack (cdr level-stack)))
		  (setcar index-stack (1+ (car index-stack))))
		(t
		  ;; Same level.
		  (setcar index-stack (1+ (car index-stack)))))
	  ;; Finally, do the item.
	  (let ((section-number-string (rgr-section-number-string index-stack)))
	    (goto-char (match-beginning 0))
	    (forward-sexp)
	    (if (looking-at "[ \t]*[0-9.]+[ \t]+")
		(replace-match ""))
	    (insert section-number-string ".  ")
	    (message "%s %s" section-number-string heading)
	    (sit-for 2))
	  )))))

;;;; Adding anchors.

(defun rgr-heuristicate-anchor-name (title)
  ;; From a title string, try to make a suitable anchor name.
  (let ((result nil) (start 0) (end (length title)))
    (while (string-match "[a-zA-Z0-9]+" title start)
      (setq result (cons (downcase (substring title (match-beginning 0)
					      (match-end 0)))
			 (and result (cons "-" result))))
      (setq start (match-end 0)))
    (setq result (nreverse result))
    ;; Flush an initial number.
    (and result (cdr (cdr result))
	 (string-match "^[0-9]+$" (car result))
	 (setq result (cdr (cdr result))))
    (apply (function concat) result)))

;;;###autoload
(defun rgr-html-add-heading-anchors (max-depth)
  "Add anchors to all headings that do not already have them.
A numeric arg means only go down to that depth."
  (interactive "p")
  (let ((n-new-anchors 0))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward rgr-heading-re nil t)
	(let ((level (string-to-number (buffer-substring-no-properties
					 (match-beginning 1)
					 (match-end 1)))))
	  (if (<= level max-depth)
	      ;; Make an anchor if needed.
	      (let ((heading (buffer-substring-no-properties (match-beginning 2)
							     (match-end 2)))
		    (last-point (point)))
		(forward-line -1)
		(if (or (not (looking-at "<a\\>"))
			;; it's an anchor; make sure it's not a name def'n.
			(let ((options (save-excursion
					 (goto-char (match-end 0))
					 (rgr-html-parse-tag-attributes))))
			  (not (stringp (cdr (assq 'name options))))))
		    ;; Not present; add one.
		    (let ((anchor-name (rgr-heuristicate-anchor-name heading)))
		      (forward-line)
		      (insert "<a name=\"" anchor-name "\">\n")
		      (setq n-new-anchors (1+ n-new-anchors))))
		;; Resume search from where it left off.
		(goto-char last-point)))))
      (message "Made %d new anchor%s down to level %d."
	       n-new-anchors
	       (if (= n-new-anchors 1) "" "s")
	       max-depth))))

(provide 'rgr-html-head)
