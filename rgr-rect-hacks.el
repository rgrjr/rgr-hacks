;;;; Additional rectangle commands.
;;;
;;;    Modification history:
;;;
;;; created.  -- rgr, 15-Jan-03.
;;;
;;; $Id$

(require 'rect)

(defvar box-rectangle-characters "+-| "
  "Characters to put around a box:  the first character is for the
corners, the second is for the top and bottom, the third is for the
side, and the fourth is for the interior.  For a box that spans at least
three lines, the first two characters form the start/end and middle of
the first and last box line, and the third and fourth characters form
the start/end and middle of all other lines.")

(defvar box-rectangle-state nil
  "Kludge to tell the line operator whether to generate a first/last line.")
(defvar box-rectangle-last nil
  "Kludge to tell the line operator whether to generate a first/last line.")

(defun rgr-box-apply-on-rectangle (line-processor start end &rest args)
  (let ((box-rectangle-state 'first)
	(box-rectangle-last (save-excursion
			      (goto-char end)
			      (beginning-of-line)
			      (set-marker (make-marker) (point)))))
    (apply (function apply-on-rectangle) line-processor start end args))
  (goto-char start))

(defun rgr-open-box-rectangle-line (startcol endcol delete-p)
  ;; this is much like the string-rectangle-line fn.
  (let ((last-p (= (point) box-rectangle-last)))
    (move-to-column startcol t)
    (if delete-p
	(delete-rectangle-line startcol endcol nil))
    (if (not (= startcol endcol))
	(let* ((index (if (or (eq box-rectangle-state 'first) last-p) 0 2))
	       (outside-char (aref box-rectangle-characters index))
	       (interior-char (aref box-rectangle-characters (1+ index)))
	       (interior-width (- (- endcol startcol) 2)))
	  (insert outside-char)
	  ;; [this may not do the right thing for multicolumn characters?  --
	  ;; rgr, 15-Jan-03.]
	  (cond ((>= interior-width 0)
		  (insert-char interior-char interior-width)
		  (insert outside-char)))
	  (setq box-rectangle-state nil)))))

;;;###autoload
(defun box-open-rectangle (start end)
  "Blank out the region-rectangle, add a surrounding box, & shift text right.

The text previously in the region is not overwritten by the blanks,
but instead winds up to the right of the rectangle.

When called from a program the rectangle's corners are START and END."
  (interactive "*r")
  (rgr-box-apply-on-rectangle 'rgr-open-box-rectangle-line start end nil))

;;;###autoload
(defun box-replace-rectangle (start end)
  "Blank out the region-rectangle, add a surrounding box, deleting text."
  (interactive "*r")
  (rgr-box-apply-on-rectangle 'rgr-open-box-rectangle-line start end t))

(defun rgr-box-surround-rectangle-line (startcol endcol)
  ;; this is based on delete-extract-rectangle-line, which i don't fully
  ;; understand.  -- rgr, 15-Jan-03.
  (let* ((last-p (= (point) box-rectangle-last))
	 (first-or-last-p (or (eq box-rectangle-state 'first) last-p)))
    (cond ((= startcol endcol))
	  ((or first-or-last-p
	       (< (move-to-column startcol t) startcol))
	    (rgr-open-box-rectangle-line startcol endcol t))
	  (t
	    (let* ((pt (point))
		   (index (if first-or-last-p 0 2))
		   (outside-char (aref box-rectangle-characters index))
		   (interior-width (- (- endcol startcol) 2)))
	      ;; [this is wrong.  -- rgr, 15-Jan-03.]
	      (or (member (char-after) '(?\n ?\t))
		  (delete-char 1))
	      (insert outside-char)
	      ;; [this may not do the right thing for multicolumn characters?
	      ;; -- rgr, 15-Jan-03.]
	      (cond ((>= interior-width 0)
		      (move-to-column (1- endcol) t)
		      ;; [this is also wrong.  -- rgr, 15-Jan-03.]
		      (or (member (char-after) '(?\n ?\t))
			  (delete-char 1))
		      (insert outside-char)))
	      (setq box-rectangle-state nil))))))

;;;###autoload
(defun box-surround-rectangle (start end)
  "Put a box around the region-rectangle, preserving interior text."
  (interactive "*r")
  (rgr-box-apply-on-rectangle 'rgr-box-surround-rectangle-line start end))

(global-set-key "\C-crb" 'box-open-rectangle)
(global-set-key "\C-crx" 'box-replace-rectangle)
(global-set-key "\C-crs" 'box-surround-rectangle)

;;; Duplicating a rectangle.

(defun duplicate-rectangle (start end)
  ;; [this doesn't work.  -- rgr, 30-Jul-03.]
  (interactive "*r")
  (insert-rectangle (setq killed-rectangle (extract-rectangle start end))))

(global-set-key "\C-crd" 'duplicate-rectangle)

(provide 'rgr-rect-hacks)
