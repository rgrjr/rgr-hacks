;;;****************************************************************************
;;;
;;;    Very random hackery.  Most of this is oriented toward doing emacs system
;;; maintenance tasks.
;;;
;;;    Do (setq debug-on-error t) to debug this stuff.  Use
;;; (read-key-sequence "Testing? ") to discover the arcane encodings used for
;;; non-ASCII character in FSF emacs 19.  Also, doing
;;;
;;;	(let ((e (read-event))) (cons (event-modifiers e) (event-basic-type e)))
;;;
;;; will give you the raw events and their symbolic decoding.  There is
;;; apparently no way provided to encode these symbolic characters into what
;;; read-event returns (but see ./encode-key.el).  Use (e.g.) [?\C-\M-%] in
;;; define-key calls.
;;;

(eval-when-compile
  (require 'cl-lib)
  (require 'ffap))

(defvar rgr-emacs)

;;;; Autoload-generating commands.

(defun rgr-update-directory-autoloads (directory &optional file)
  ;; internal grinder for the rgr-update-autoloads and
  ;; rgr-batch-update-autoloads fns.  -- rgr, 21-Dec-00.
  (require 'autoload)
  (let* ((dir (expand-file-name directory))
	 (generated-autoload-file
	   (expand-file-name (or file "loaddefs.el") dir)))
    (update-directory-autoloads dir)))

;;;###autoload
(defun rgr-update-autoloads ()
  "Update the rgr-hacks-autoloads.el file with the latest autoloads
from this directory."
  (interactive)
  (rgr-update-directory-autoloads rgr-emacs "rgr-hacks-autoloads.el")
  (let ((imported (expand-file-name "../imported" rgr-emacs)))
    (if (file-directory-p imported)
	(rgr-update-directory-autoloads imported))))

;;;###autoload
(defun rgr-batch-update-autoloads ()
  "Calls rgr-update-directory-autoloads on command-line-args-left."
  (let ((delete-old-versions t)	;; force backup deletion without query.
	(kept-old-versions 0))
    (apply 'rgr-update-directory-autoloads command-line-args-left))
  (setq command-line-args-left nil))

;;;; Byte compilation.

;;;###autoload
(defun rgr-list-files-needing-compile (directory &optional arg)
  ;; based on byte-recompile-directory
  "Recompile every `.el' file in DIRECTORY that needs recompilation.
This is if a `.elc' file exists but is older than the `.el' file.
Files in subdirectories of DIRECTORY are processed also.

If the `.elc' file does not exist, normally the `.el' file is *not*
listed as needing to be compiled.  But a prefix argument (optional
second arg) means list it anyway.

A nonzero prefix argument also means ask about each subdirectory."
  (interactive "DList files needing recompile in directory: \nP")
  (if arg
      (setq arg (prefix-numeric-value arg)))
  (if noninteractive
      nil
    (save-some-buffers)
    (force-mode-line-update))
  (let ((directories (list (expand-file-name directory)))
	(file-count 0)
	(dir-count 0)
	last-dir)
    (with-output-to-temp-buffer "*elisp files*"
     (while directories
       (setq directory (car directories))
       (message "Checking %s..." directory)
       (let ((files (directory-files directory))
	     source dest)
	 (while files
	   (setq source (expand-file-name (car files) directory))
	   (cond ((member (car files) '("." ".." "RCS" "CVS")))
		 ((and (file-directory-p source)
		       (not (file-symlink-p source)))
		   ;; This file is a subdirectory.  Handle them differently.
		   (when (or (null arg)
			     (eq 0 arg)
			     (y-or-n-p (concat "Check " source "? ")))
		     (setq directories
			   (nconc directories (list source)))))
		 ;; It is an ordinary file.  Decide whether to compile it.
		 ((not (and (string-match emacs-lisp-file-regexp source)
			    (not (auto-save-file-name-p source))))
		   ;; not an emacs lisp file.
		   )
		 ((progn (setq dest (byte-compile-dest-file source))
			 (not (file-exists-p dest)))
		   (if arg
		       (princ (format "%s is not compiled.\n" source))))
		 ;; File was already compiled.
		 ((file-newer-than-file-p source dest)
		   (princ (format "%s needs to be compiled.\n" source))
		   (setq file-count (1+ file-count))
		   (if (not (eq last-dir directory))
		       (setq last-dir directory
			     dir-count (1+ dir-count)))))
	   (setq files (cdr files))))
       (setq directories (cdr directories))))
    (message "Done (total of %d file%s need to be compiled%s)."
	     file-count (if (= file-count 1) "" "s")
	     (if (> dir-count 1) (format " in %d directories" dir-count) ""))))

;;;; Hacking meals.

;;;###AUTOLOAD
(defun rgr-region-meals (start end)
  (interactive "r")
  (shell-command-on-region start end
			   "../meals.pl --recipe-file ../recipes.text --det"))

;;;###AUTOLOAD
(defun rgr-dup-line (&optional count)
  "Duplicate the current line before the marked line.
If the first non-whitespace char is a '#', remove that and all whitespace
that comes immediately afterward.  With a numeric arg (default 1), do that
many lines.  The mark is moved after the added line(s), point is moved to
the next line."
  (interactive "p")
  (dotimes (i count)
    (save-excursion
      (let ((line (buffer-substring-no-properties
		   (progn (beginning-of-line) (point))
		   (progn (end-of-line) (point)))))
	(goto-char (or (mark) (error "The mark is not set.")))
	(insert-before-markers line)
	(save-excursion
	  (beginning-of-line)
	  (skip-chars-forward " \t\n")
	  (if (looking-at "#[ \t\n]*")
	      (replace-match "")))
	(insert-before-markers "\n")))
    (forward-line 1)))

;;;; Sudoku statistics.

(defconst rgr-sudoku-statistics-line
  "^.*[ \t]\\([0-9]+\\.[0-9]\\)[ \t]+\\([0-9]+:[0-9][0-9]\\) *\\(.*\\)$"
  "Regexp that recognizes the difficulty, time, and optional game type comment
string in sudoku game statistics files.")
(defconst rgr-suduku-date-regexp "\\([0-9][0-9]?\\)-\\(...\\)-\\([0-9][0-9]\\)"
  "Regexp that recognizes dates that come after sudoku results.")

(defun rgr-sudoku--time-secs ()
  ;; Get time time in seconds from (match-string 2).
  (let ((time (match-string 2)))
    (+ (* 60 (cl-parse-integer (substring time 0 -3)))
       (cl-parse-integer (substring time -2)))))

(defun rgr-sudoku--next-date ()
  ;; Parse DD-Mon-YY time in the 21st century and return a (day mon year) date
  ;; triple with values compatible with decode-time, i.e. day is between 1 and
  ;; 31 and month is between 1 and 12.  Dies if month is not the usual 3-letter
  ;; English abbreviation.
  (and (re-search-forward rgr-suduku-date-regexp nil t)
       (let ((day (cl-parse-integer (match-string 1)))
	     (numeric-month
	       (or (cl-position (match-string 2)
				'("Jan" "Feb" "Mar" "Apr" "May" "Jun"
				  "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")
				:test #'string-equal)
		   (error "Bad month in %S" (match-string 0))))
	     (year (+ 2000 (cl-parse-integer (match-string 3)))))
	 (list day (1+ numeric-month) year))))

(defun rgr-sudoku--date-less? (date1 date2)
  ;; Given two dates as returned by rgr-sudoku--next-date, return true if the
  ;; first is strictly less than the second.
  (cond ((< (caddr date1) (caddr date2)) t)
	((> (caddr date1) (caddr date2)) nil)
	((< (cadr date1) (cadr date2)) t)
	((> (cadr date1) (cadr date2)) nil)
	(t (< (car date1) (car date2)))))

(defun rgr-sudoku--find-next-date-after (target-date)
  ;; Use binary search between here and the beginning of the file to find the
  ;; first date after the indicated date.
  (let ((min (point-min))
	(max (point))
	(last-date t) (mid-date nil))
    (while (not (or (equal mid-date target-date)
		    (equal mid-date last-date)))
      (goto-char (/ (+ min max) 2))
      (beginning-of-line)	;; in case we land in the middle of a date.
      (setq last-date mid-date)
      (setq mid-date (rgr-sudoku--next-date))
      (if (rgr-sudoku--date-less? mid-date target-date)
	  (setq min (point))
	(setq max (point))))
    (point)))

;;;###autoload
(defun rgr-sudoku-statistics ()
  "Find similar sudoku results from the previous year and report a percentile.
If on a line that matches rgr-sudoku-statistics-line, e.g. the first of

    1  179  18.3  70:21 (Sohei)
    -- rgr, 14-Mar-21.

though not necessarily with leading indentation, this command searches backward
for all similar difficultly levels and presents a percentile score.  The first
two numbers are ignored, the third is the difficulty level, and the third is
the time in minutes and seconds.  'Similar difficulty levels' are those within
plus or minus one of the given level.  The next date that matches
rgr-suduku-date-regexp is taken as the ending date, and all scores that match
the tag (in this example it is '(Sohei)') that are on lines before the date on
or after the same date in the year before are counted.  Times above and below
the time on the current line are tallied separately, and the percentile is
given as the times above divided by the total, in a report such as this:

    4221 seconds is 23.4 percentile over the last year (n=184)."
  (interactive)
  (require 'parse-time)
  (save-excursion
    (beginning-of-line)
    (or (looking-at rgr-sudoku-statistics-line)
	(error "Not on a sudoku statistics line."))
    (let ((target-difficulty (car (read-from-string (match-string 1))))
	  (target-time (rgr-sudoku--time-secs))
	  (target-comment (match-string 3)))
      ;; (message "Got %S" (list target-difficulty target-time target-comment))
      (forward-line)	;; Count the current line in the statistics.
      (let* ((search-end (point))
	     (end-date (save-excursion
			 (or (rgr-sudoku--next-date)
			     (error "This line is undated."))))
	     (start-date (list (car end-date) (cadr end-date)
			       (1- (caddr end-date))))
	     (count-above 0)
	     (count-below 0))
	;; Search backward a year.
	(rgr-sudoku--find-next-date-after start-date)
	;; Go to the previous date.
	(re-search-backward rgr-suduku-date-regexp nil t)
	(forward-line)
	;; Count similar puzzles on statistics lines as we walk forward.
	(while (< (point) search-end)
	  (when (looking-at rgr-sudoku-statistics-line)
	    (let ((difficulty (car (read-from-string (match-string 1))))
		  (time-secs (rgr-sudoku--time-secs))
		  (comment (match-string 3)))
	      (when (and (< (abs (- target-difficulty difficulty)) 1)
			 (string-equal target-comment comment))
		;; Count this guy.
		(if (> time-secs target-time)
		    (cl-incf count-above)
		  (cl-incf count-below)))))
	  (forward-line))
	;; Report the result.
	(let ((n (+ count-above count-below)))
	  (message "%S seconds is %.1f percentile over the last year (n=%d)."
		   target-time (/ (* 100.0 count-above) n) n))))))

;;;; Done.

(provide 'rgr-random-hacks)
