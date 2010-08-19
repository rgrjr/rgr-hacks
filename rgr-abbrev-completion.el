;;;*****************************************************************************
;;;
;;;    Self-training completion
;;;
;;;   Really, this is just a different notion of abbrevs.  It is written to be
;;; easily gnu-ified.
;;;
;;;    As you are typing along (or even just moving the cursor), it learns the
;;; words you type.  When you type a prefix and follow it with f4, it supplies
;;; as much of the completion as it can.  If it finishes the whole word, it
;;; increments the completion count on that word so it can make a better guess
;;; when completing abbreviations.  [oops -- what if a word and an abbreviation
;;; are the same?  -- rgr, 5-May-94.]  If you type f4 after an obvious
;;; abbreviation (such as "mvb" for "multiple-value-bind"), it inserts the "most
;;; popular" completion, as determined by completion counts, incrementing it as
;;; well.  If you type f4 again immediately, it cycles through the possibilities
;;; in descending order of popularity, updating the counts as it goes.  At any
;;; point, [(meta f4)] gives a menu of possibilities.  This means the commands
;;; must remember a fair amount of state from one keystroke to the next, though
;;; they try not to cons unless they absolutely have to.
;;;
;;;    Bugs/to do:
;;;
;;;    1.  need an undo boundary before insertion.  [this seems to work OK in
;;; GNU emacs.  -- rgr, 22-Aug-99.]
;;;
;;;    2.  It would be nice to get rid of trailing "." and "]" (and probably
;;; others) in words.  Also, "words" that are entirely nonalphabetic should
;;; probably be omitted.  The problem is finding a reasonable choke-point for
;;; this.  -- rgr, 27-Jul-95.
;;;
;;;    4.  Should replace fast-string-hash with a simple intern of the first
;;; three letters.  Probably faster, since it will run in C rather than byte
;;; code.
;;;
;;;    5.  The sets of (string from to) args that were used for efficiency in
;;; Zmacs could now be replaced with a single "word" or "prefix" argument.
;;; [done.  -- rgr, 1-Feb-98.]
;;;
;;;    6.  Flush text attributes in completion text.  -- rgr, 12-Dec-96.
;;; [done.  -- rgr, 22-Aug-99.]
;;;
;;;    7.  ***bug***:  new version considers it OK that a string is a completion
;;; of itself.
;;;
;;;    Modification history:
;;;
;;; started rgr-insert-symbol-abbreviation implementation.  -- rgr, 28-Apr-94.
;;; more work on completions, inserting abbreviations.  -- rgr, 26-May-94.
;;; abbreviation fixery, rgr-clean-symbol-prefixes, etc.  -- rgr, 10-Jun-94.
;;; rgr-kill-last-symbol-abbreviation, insert case-replace.  -- rgr, 13-Jun-94.
;;; rgr-enumerate-completion-possibilities instead of menu, fix some
;;;	rgr-kill-last-symbol-abbreviation bugs.  -- rgr, 13-Jun-94.
;;; com-describe-symbol-at-point, split out of #p"MW:zmacs.lisp"
;;;	file.  -- rgr, 23-Jun-94.
;;; some rgr-kill-last-symbol-abbreviation improvement.  -- rgr, 20-Jul-94.
;;; file i/o, more kill improvement, abbrev re-completion.  -- rgr, 21-Jul-94.
;;; record # of completions written, make rgr-kill-last-symbol-abbreviation
;;;	inform if no completions to kill.  -- rgr, 25-Jul-94.
;;; (flavor:method make-instance section-node :after): new.  -- rgr, 3-Aug-94.
;;; downcase when interning, made rgr-maybe-learn-new-symbol more conservative,
;;;	other tweaks.  -- rgr, 11-Aug-94.
;;; rgr-string-case, really downcase when interning (bug fix), other bug
;;;	recovery, some gnu tweaking.  -- rgr, 15-Aug-94.
;;; rgr-relearn-buffer-definition-names: new, better clean.  -- rgr, 26-Aug-94.
;;; while -> #p"mw:zmacs.lisp".  -- rgr, 1-Sep-94.
;;; rgr-maybe-insert-abbreviation: no poss. count if =1.  -- rgr, 28-Nov-94.
;;; nothing much.  -- rgr, 5-Dec-94.
;;; rgr-hash-string-potential-symbols: do terminating word.  -- rgr, 19-Dec-94.
;;; complete in minibuffer.  -- rgr, 21-Mar-95.
;;; started some gnuification.  -- rgr, 26-May-95.
;;; *** started GNU version (emacs 19.31).  -- rgr, 28-Nov-96.
;;; rgr-abbrev-find-file-hook, other new hooks.  -- rgr, 29-Nov-96.
;;; fix rgr-read-completion-file-internal eof bug.  -- rgr, 29-Nov-96.
;;; rgr-abbrev-completion-save-file, conditional save.  -- rgr, 2-Dec-96.
;;; rgr-install-weekly-completion-cycle: new, don't dncase.  -- rgr, 10-Dec-96.
;;; rgr-abbrev-maybe-insert-abbreviation: make re-compl. work, msg.  -- rgr, 10-Dec-96.
;;; rgr-write-completion-file: don't overwrite recent files.  -- rgr, 12-Dec-96.
;;; rgr-abbrev-find-file-hook: no psa-transaction files.  -- rgr, 24-Mar-97.
;;; flush (line start end) args, new UI start.  -- rgr, 1-Feb-98.
;;; fix repeat-single-possibility problem, general cleanup.  -- rgr, 3-Feb-98.
;;; rgr-insert-symbol-abbreviation: don't impose word limit if repeating, make
;;;	word-start optional rgr-abbrev-completion-state fn.  -- rgr, 10-Feb-98.
;;; don't uselessly trash "(New file)" message.  -- rgr, 2-Mar-98
;;; rgr-abbrev-more-message, new min length.  -- rgr, 10-Mar-99.
;;; clean up comments, delete dead code.  -- rgr, 22-Aug-99.
;;; add fi:common-lisp-mode to major modes with definitions.  -- rgr, 13-Feb-00.
;;; rgr-list-abbreviations: new.  -- rgr, 23-May-01.
;;; rgr-abbrev-make-simple-abbreviation case changes, e.g.
;;;	"reallyStupidNamingConvention" -> "rsnc".  -- rgr, 26-Oct-01.
;;; rgr-completion-start-auto-save feature.  -- rgr, 1-Jul-02.
;;; rgr-abbrev-after-save-hook, to update after autosave.  -- rgr, 2-Jul-02.
;;; rgr-write-completion-file: made this a command.  -- rgr, 2-Sep-02.
;;;

(defvar rgr-completion-min-entry-length 6
  ;; [used to be 8; we are now trying a new value, which is in fact the
  ;; "historic" LSP value.  -- rgr, 10-Mar-99.]
  "*Don't bother remembering words shorter than this.")
(defvar rgr-abbrev-completion-save-directory
	(expand-file-name "~/emacs/completions"))
(defvar rgr-abbrev-completion-save-file
	(and rgr-abbrev-completion-save-directory
	     (file-writable-p rgr-abbrev-completion-save-directory)
	     (expand-file-name "completions.text"
			       rgr-abbrev-completion-save-directory))
  "*Default file into which to save completions between sessions, nil to
disable.")
(defvar rgr-completion-auto-save-timer nil
  "A timer object if auto-save is enabled (see rgr-completion-start-auto-save),
else nil.")
(defvar rgr-abbrev-after-save-hook nil
  "*Hook run after saving a completion file.  Mostly this is used as
communication between rgr-install-weekly-completion-cycle and
rgr-completion-do-auto-save so that the file name gets updated to the
next day when both features are in effect.")
(defvar rgr-abbrev-completion-append-to-file-p
	'rgr-abbrev-completion-file-newer-than-one-day-p
  "t to always append to an existing file, nil to never append, and the
name of a function that takes the filename as an argument and returns a
boolean otherwise.")

(defconst rgr-string-table-size 311)
(defvar rgr-string-table (make-vector rgr-string-table-size nil))

(defvar rgr-abbreviation-scratch-string (make-string 100 32)
  "See the rgr-abbrev-make-simple-abbreviation function.")

(defvar rgr-abbrev-completion-state nil
  "Completion state, cons of (word-start word-end orig-word new-word),
and (current-index . possibilities).")

;; [attempt to dekludgify.  -- rgr, 10-Feb-98.]
(defsubst rgr-abbrev-completion-state-insertion-start (&optional state)
  (car (car (or state rgr-abbrev-completion-state))))
(defsubst rgr-abbrev-completion-state-original-string (&optional state)
  (nth 2 (car (or state rgr-abbrev-completion-state))))
(defsubst rgr-abbrev-completion-state-replacement-string (&optional state)
  (nth 3 (car (or state rgr-abbrev-completion-state))))

;;;; Low-level utility definitions.

(defsubst rgr-alphanumericp (char)
  ;; Curiously, this also includes digits in both Lisp and C sytaxes.  -- rgr,
  ;; 29-Nov-96.
  (eq (char-syntax char) ?w))

(defsubst rgr-lower-case-p (char)
  ;; This hack doesn't work in EBCDIC, where the alphabetics aren't dense,
  ;; except that rgr-abbrev-make-simple-abbreviation arranges to give it only
  ;; alphabetics, so it's OK.  (I used to have to care about such things.)
  (and (<= ?a char) (<= char ?z)))

(defsubst rgr-identifier-char-p (char)
  "Return T if legal in an identifier.  Characters that are true for
this test but are not alphabetic are used as break characters when
identifiers up into words."
  ;; Unfortunately, this is not quite isomorphic between systems.
  (let ((code (char-syntax char)))
    (or (eq code ?w)
        (eq code ?_))))

(defun rgr-buffer-word-start (end)
  "Look backward for the beginning of a word, and return the point (but
leave the current point where it is)."
  ;; [formerly a grinder for the with-rgr-word macro, but now used by itself.
  ;; -- rgr, 1-Feb-98.]
  (save-excursion
    (goto-char end)
    (while (and (not (bolp))
                (rgr-identifier-char-p (char-after (1- (point)))))
      (forward-char -1))
    (point)))

(defun rgr-string-case (string)
  "Return a keyword symbol in (:upper :lower :mixed) depending upon the
case of alphabetic characters in string, or NIL if the string has no
alphabetic characters."
  (let ((upper-p nil)
        (lower-p nil))
    (let ((<limit> (length string))
          (i 0))
      (while (< i <limit>)
        (let ((code (aref string i)))
          (cond ((< code 65))
                ((<= code 90) (setq upper-p t))
                ((< code 97))
                ((<= code 122) (setq lower-p t))))
        (setq i (1+ i))))
    (cond ((and upper-p lower-p) ':mixed)
          (upper-p ':upper)
          (lower-p ':lower)
          (t nil))))

(defun rgr-fast-string-hash (string)
  (let ((len (length string)))
    (cond ((eq len '0) 0)
          ((eq len '1) (upcase (aref string 0)))
          ((eq len '2)
           (+ (* (upcase (aref string 0)) 29)
              (upcase (aref string 1))))
          (t (+ (upcase (aref string 2))
                (* (+ (* (upcase (aref string 0)) 29)
                      (upcase (aref string 1)))
                   29))))))

(defun rgr-abbrev-intern-string (string initial-value)
  (let ((hash (mod (rgr-fast-string-hash string) rgr-string-table-size)))
    (or (let ((<tail> (aref rgr-string-table hash)))
          (while (and <tail> (not (equal string (car (car <tail>)))))
            (setq <tail> (cdr <tail>)))
          (car <tail>))
        (let ((len (length string)))
	  ;; [don't downcase all-upper things; now that I'm hacking C (sigh),
	  ;; case matters.  -- rgr, 10-Dec-96.]
          '(if (eq (rgr-string-case string) ':upper)
              (let ((i 0))
                (while (< i len)
                  (aset string i (downcase (aref string i)))
                  (setq i (1+ i)))))
          (car (aset rgr-string-table hash
		     (cons (cons string initial-value)
			   (aref rgr-string-table hash))))))))

(defun rgr-abbrev-prefix-equal (prefix string)
  ;; Case-insensitive prefix comparison without consing.
  (let ((end2 (length prefix)))
    (and (>= (length string) end2)
         (let ((i 0)
               (equal-p t))
           (while (and equal-p (< i end2))
             (setq equal-p
                    (= (upcase (aref prefix i))
                       (upcase (aref string i))))
             (setq i (+ i 1)))
           equal-p))))

(defun rgr-abbrev-map-possibilities (function string &optional abbrevs-too-p)
  ;; Applies function to each entry that is a possible completion or
  ;; abbreviation expansion of string, with the second arg detailing which: t
  ;; for prefix or nil for abbrev.  [was the rgr-map-completions function, now
  ;; subsumes rgr-map-abbreviations as well.  -- rgr, 1-Feb-98.]
  (let ((<tail>
         (aref rgr-string-table
               (mod (rgr-fast-string-hash string) rgr-string-table-size)))
        (entry nil))
    (while <tail>
      (setq entry (car <tail>))
      (if (rgr-abbrev-prefix-equal string (car entry))
          (if (listp (cdr entry))
	      ;; Abbreviations
	      (and abbrevs-too-p
		   (let ((<tail> (cdr entry)))
		     (while <tail>
		       (funcall function (car <tail>) nil)
		       (setq <tail> (cdr <tail>)))))
	      ;; Prefixes
	      (and (not (eq abbrevs-too-p ':abbrevs-only))
		   (funcall function entry t))))
      (setq <tail> (cdr <tail>)))))

(defun rgr-abbrev-make-simple-abbreviation (string)
  ;; Pick out the first letters of words delimited by non-alphanumerics, or
  ;; uppercase letters that come immediately after lower case, as in
  ;; "reallyStupidNamingConvention" -> "rsnc".
  (let ((last-alpha nil) (last-lc nil)
        (abbrev rgr-abbreviation-scratch-string)
        (abbrev-len 0)
	(to (length string))
	(i 0))
    (while (< i to)
      (let* ((char (aref string i))
	     (this-alpha (rgr-alphanumericp char))
	     (this-lc (and this-alpha (rgr-lower-case-p char))))
        (cond ((and this-alpha
		    (or (not last-alpha)
			(and last-lc (not this-lc))))
	        ;; take this char (alphabetic after nonalphabetic, or uppercase
	        ;; after lowercase).
	        (aset abbrev abbrev-len (downcase char))
	        (setq abbrev-len (+ abbrev-len 1))))
	(setq last-alpha this-alpha)
	(setq last-lc this-lc)
	(setq i (1+ i))))
    (substring abbrev 0 abbrev-len)))

(defun rgr-undefine-entry (entry)
  (let* ((string (car entry))
         (length (length string))
         (hash (mod (rgr-fast-string-hash string) rgr-string-table-size))
         (abbreviation (rgr-abbrev-make-simple-abbreviation string)))
    (aset rgr-string-table hash
	  (delete entry (aref rgr-string-table hash)))
    (if (>= (length abbreviation) 3)
        (let ((abbrev-entry (rgr-abbrev-intern-string abbreviation nil)))
          (if (listp (cdr abbrev-entry))
              (rplacd abbrev-entry (delete entry (cdr abbrev-entry))))))))

(defun rgr-abbrev-hash-name (string)
  ;; "Learns" the name, interning it both as a string and as an abbreviation.
  (if (>= (length string) rgr-completion-min-entry-length)
      (let* ((entry (rgr-abbrev-intern-string string 0))
             (abbreviation (rgr-abbrev-make-simple-abbreviation string))
             (len (length abbreviation)))
        (if (>= len 3)
            (let ((abbrev-entry (rgr-abbrev-intern-string abbreviation nil)))
              (if (listp (cdr abbrev-entry))
                  (if (not (member entry (cdr abbrev-entry)))
                      (rplacd abbrev-entry (cons entry (cdr abbrev-entry)))))))
        entry)))

(defun rgr-hash-string-potential-symbols (string)
  ;; Define all names that appear in the string.  The string is expected to
  ;; constitute a complete line, so we can assume that initial and terminal
  ;; words are complete.
  (let ((length (length string))
        (word-start nil))
    (let ((i 0))
      (while (< i length)
        (let ((char (aref string i)))
          (cond ((not (rgr-identifier-char-p char))
		  (and word-start
		       (rgr-abbrev-hash-name (substring string word-start i)))
		  (setq word-start nil))
                ((not word-start)
		  (setq word-start i))))
        (setq i (1+ i))))
    (if (and word-start
             (rgr-identifier-char-p (aref string (- length 1))))
	;; Look at the terminating word.
        (rgr-abbrev-hash-name (substring string word-start length)))))

(defun rgr-prefix-length (string1 string2 &optional max)
  ;; Return the length of the common prefix of string1 and string2, up to a
  ;; maximum of max, or 0 if they have no common prefix.
  (let* ((lens (min (length string1) (length string2)))
         (max (if max (min max lens) lens))
         (i 0))
    (while (and (< i max)
                (char-equal (aref string1 i) (aref string2 i)))
      (setq i (+ i 1)))
    i))

(defun rgr-prefix-exists-p (string entries)
  ;; Returns T iff string is a proper prefix of any of the entries.
  (let ((len (length string))
        (tail entries)
        (result nil))
    (while tail
      (let ((entry (car (car tail))))
        (if (and (= (rgr-prefix-length string entry) len)
		 (> (length entry) len))
	    (setq result t tail nil)
	    (setq tail (cdr tail)))))
    result))

;;; Learning by watching the user type.

(defun rgr-maybe-learn-new-symbol ()
  (let ((point (point)))
    (if (and (> (- point (point-min)) rgr-completion-min-entry-length)
	     (not (rgr-identifier-char-p (char-after (1- point))))
	     (rgr-identifier-char-p (char-after (- point 2))))
	;; with-rgr-word (line start end) (- point 1)
        (let* ((word (buffer-substring-no-properties
		       (rgr-buffer-word-start (- point 1))
		       (- point 1)))
	       (len (length word)))
	  (if (and (>= len rgr-completion-min-entry-length)
		   (not (let ((prefix-p nil))
			  (rgr-abbrev-map-possibilities
			    (function (lambda (entry ignore)
			      (if (not (= (length (car entry)) len))
				  (setq prefix-p entry))))
			    word)
			  prefix-p)))
	      (rgr-abbrev-hash-name word))))))

;;; Learning definition names.

(defvar rgr-definition-line-regexp "^[(a-zA-Z]"
  "This works well enough for both C and Lisp.  Really, the variable
ought to be made buffer-local for anything fancier.")

(defun rgr-relearn-buffer-definition-names (&optional silent-p)
  "Relearns all names on all definition lines of the current buffer.
Definitions are defined by matching the value of the rgr-definition-line-regexp
variable."
  (interactive)
  (let ((count 0))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward rgr-definition-line-regexp nil t)
	(let ((line (buffer-substring-no-properties
		      (point)
		      ;; This lumps multiple candidate lines into one, as well
		      ;; as moving past the set of consecutive matching lines.
		      (progn
			(beginning-of-line)
			(while (and (not (eobp))
				    (looking-at rgr-definition-line-regexp))
			  (setq count (1+ count))
			  (forward-line))
			(point)))))
	  (rgr-hash-string-potential-symbols line))))
    (or silent-p
	(message "Done -- %d definition lines." count))
    count))

(defvar rgr-abbrev-major-modes-that-have-definitions
	'(emacs-lisp-mode lisp-mode fi:common-lisp-mode makefile-mode c-mode)
  "These modes get rgr-relearn-buffer-definition-names done automatically
when the file is found.")

(defun rgr-abbrev-find-file-hook ()
  ;; Conditionally call rgr-relearn-buffer-definition-names.
  (if (and (memq major-mode rgr-abbrev-major-modes-that-have-definitions)
	   ;; [don't uselessly trash "(New file)" message.  -- rgr, 2-Mar-98]
	   (not (= (point-min) (point-max))))
      (if (memq major-mode '(emacs-lisp-mode lisp-mode))
	  ;; [***kludge***: don't treat psa transactions as abbrevs.  -- rgr,
	  ;; 24-Mar-97.]
	  (or (string-match "psa-transactions.*\\.log$" (buffer-file-name))
	      (rgr-relearn-buffer-definition-names))
	  ;; Normal case.
	  (rgr-relearn-buffer-definition-names))))

;;;; Machinery for inserting and deleting completions.

(defun rgr-abbrev-insert-word-tail
       (original-string word-entry &optional length)
  (let* ((new-word (car word-entry))
	 (new-string (if length (substring new-word 0 length) new-word))
         (length (or length (length new-word)))
	 (word-start (- (point) (length original-string))))
    ;; [old state.  these are obsolete.  -- rgr, 3-Feb-98.]
    ;; (setq *rgr-completion-original-string* original-string)
    ;; (setq *rgr-completion-insertion-entry* word-entry)
    ;; (setq *rgr-completion-insertion-length* length)
    ;; (setq rgr-completion-other-abbrevs nil)
    ;; no case-replace in gnu emacs?  -- rgr, 1-Aug-95.  [probably don't want it
    ;; anyway.  -- rgr, 29-Nov-96.]  [there is a case-replace, but it's a
    ;; variable that affects what's passed to replace-match.  but in any (ah)
    ;; case, we'd need to use replace-match, but we didn't use RE matching in
    ;; the first place.  -- rgr, 1-Feb-98.]
    (delete-region word-start (point))
    (insert new-string)
    ;; [new state.  -- rgr, 1-Feb-98.]
    (or rgr-abbrev-completion-state
	(setq rgr-abbrev-completion-state (cons nil nil)))
    (rplaca rgr-abbrev-completion-state
	    (list word-start (point) original-string new-string word-entry))
    ;; update use count.
    (if (= length (length new-word))
        (setcdr word-entry (+ (cdr word-entry) 1)))
    t))

(defun rgr-abbrev-completion-state (&optional word-start)
  ;; Use the variable with the same name to decide whether we are continuing or
  ;; not, and if so, how.  point is assumed to be at the word end.  [word-start
  ;; may soon go away.  -- rgr, 10-Feb-98.]
  (let* ((state (car rgr-abbrev-completion-state))
	 (insertion-start (car state))
	 (insertion-end (car (cdr state)))
	 (replacement-string (rgr-abbrev-completion-state-replacement-string)))
    '(message "point %s insertion-start %s insertion-end %s word-start %s"
	     (point) insertion-start insertion-end word-start)
    '(sit-for 1)
    (cond ((null state) nil)
	  ((and word-start
		(or (not (eq insertion-start word-start))
		    (< (point) insertion-end)
		    (not (equal (buffer-substring-no-properties insertion-start
								insertion-end)
				replacement-string))))
	    ;; [usually not done -- loses if the syntax rules change.  -- rgr,
	    ;; 10-Feb-98.]
	    ;; no completion, completing in a different place, or the initial
	    ;; portion has changed.
	    nil)
	  ((and (not word-start)
		;; The rules are slightly different here.  (point) must be at
		;; insertion-end, and we can't check the start.
		(or (not (= (point) insertion-end))
		    (not (equal (buffer-substring-no-properties insertion-start
								insertion-end)
				replacement-string))))
	    ;; [usually not done -- loses if the syntax rules change.  -- rgr,
	    ;; 10-Feb-98.]
	    ;; no completion, completing in a different place, or the initial
	    ;; portion has changed.
	    nil)
	  ((= (point) insertion-end)
	    ;; repeated invocation at the same point.
	    ':repeat)
	  (t
	    ;; must have supplied a few more characters.
	    ':continue))))

(defun rgr-abbrev-undo-last-insertion-internal (&optional query-p)
  ;; If we're still in the place of the last insertion, undo it & return the
  ;; word entry.  [We used to do this even if the user had typed a few more
  ;; characters.  Probably wasn't a good idea.  -- rgr, 10-Feb-98.]
  (if (rgr-abbrev-completion-state)
      (let* ((state (car rgr-abbrev-completion-state))
	     (word-start (car state))
	     (original-string (nth 2 state))
             (word-entry (nth 4 state))
             (word-length (- (nth 1 state) word-start))) 
	(if (eq query-p t)
	    (setq query-p "Undo"))
	(cond ((or (not query-p)
		   (y-or-n-p (format "%s \"%s\"?" query-p (car word-entry))))
		(delete-region word-start (point))
		(insert original-string)
		(if (= word-length (length (car word-entry)))
		    (setcdr word-entry (- (cdr word-entry) 1)))
		word-entry)))))

;;; Getting rid of unwanted possibilities.
;;; This is a problem with "watching-the-user-type" learning.

(defun rgr-kill-last-symbol-abbreviation ()
  "Undefine the last abbreviation inserted.
Also undoes the effect if it was done by the immediately preceding command."
  (interactive)
  (let ((chosen-entry (rgr-abbrev-undo-last-insertion-internal "Remove")))
    (or chosen-entry
	;; with-rgr-word (line start end) (point)
	(let ((word (buffer-substring-no-properties
		      (rgr-buffer-word-start (point))
		      (point)))
	      (nothing-found t))
	  (rgr-abbrev-map-possibilities
	   (function (lambda (entry ignore)
	     (setq nothing-found nil)
	     (if (not chosen-entry)
		 (if (y-or-n-p (format "Remove \"%s\"? " (car entry)))
		     (setq chosen-entry entry)))))
	   word t)
	  (if nothing-found
	      (error "No completions for \"%s\"." word))))
    (cond (chosen-entry
	    (message "Undefined \"%s\"." (car chosen-entry))
	    (rplacd chosen-entry -1)
	    (rgr-undefine-entry chosen-entry)))))

(defun rgr-abbrev-show-possibilities (possibilities)
  ;; [this would be better replaced by an ispell-style menu.  -- rgr, 1-Feb-98.]
  (cond ((null possibilities) (message "No possibilities."))
	((null (cdr possibilities))
	  (message "The only possibility is %s." (car possibilities)))
	(t
	  (let* ((size 78)
		 (room (- size
			  (length ", and ~D others")
			  (length (car (nth 0 possibilities)))
			  2)))
	    (princ (format "%s" (car (nth 0 possibilities))))
	    (setq possibilities (cdr possibilities))
	    (while (and possibilities
			(>= room
			    (+ 4 (length (car (car possibilities))))))
	      (let ((string (car (nth 0 possibilities)))
		    (separator (if (cdr possibilities) ", " ", and ")))
		(princ (format "%s%s" separator string))
		(setq room
		      (- room
			 (+ (length separator) (length string)))))
	      (setq possibilities (cdr possibilities)))
	    (if possibilities
		(princ (format ", and %d others" (length possibilities))))
	    (princ ".")))))

(defun rgr-better-match-p (entry1 entry2)
  (if (= (cdr entry1) (cdr entry2))
      (> (length (car entry1)) (length (car entry2)))
      (> (cdr entry1) (cdr entry2))))

;;;; Commands for inserting completions.
;; [revised design.  -- rgr, 1-Feb-98.]

(defun rgr-abbrev-more-message (n total)
  ;; print a message describing the position of this possibility in the set.  n
  ;; is the 0-based index of the possibility we are inserting now; total is the
  ;; count of all possibilities.
  (let ((more (- total (1+ n)))
	(redo (substitute-command-keys
	        "type \\[rgr-insert-symbol-abbreviation]")))
    (cond ((= total 1)
	    ;; only one possibility; don't bother saying anything.
	    )
	  ((= n 0)
	    ;; first of multiple hits; explain fully.
	    (message "%d more possibilit%s; %s to cycle through them."
		     more
		     (if (= more 1) "y exists" "ies exist")
		     redo))
	  ((> more 0)
	    ;; neither first nor last of more than one (and therefore must be at
	    ;; least three).  just give the numbers.
	    (message "%d more possibilit%s."
		     more
		     (if (= more 1) "y" "ies")))
	  (t
	    ;; last of multiple; remind the user how to get them again.
	    (message "Last of %d possibilities; %s to restart."
		     total redo)))))

(defun rgr-abbrev-insert-next-possibility (word n)
  (let ((possibilities (cdr rgr-abbrev-completion-state))
	(last-entry (nth 4 (car rgr-abbrev-completion-state))))
    (cond ((null (cdr possibilities))
	    (error "No completions for '%s'." word))
          (t
	    (if last-entry
		(setcdr last-entry (- (cdr last-entry) 1)))
	    (rgr-abbrev-more-message n (length (cdr possibilities)))
	    (rgr-abbrev-insert-word-tail word (nth n (cdr possibilities)))
	    (setcar possibilities n)))))

(defun rgr-abbrev-expand-initial-guess (word-start word
					&optional no-abbrev-p no-expand-p)
  ;; no-expand-p means just compute the possibilities.
  (let ((len (length word))
	(possibilities nil) (best-match nil) (best-len nil))
    (rgr-abbrev-map-possibilities
      (function (lambda (entry prefix-p)
	(let* ((string (car entry))
	       (entry-len (length string)))
	  ;; Keep track of possibilities.
	  (or no-abbrev-p
	      (setq possibilities (cons entry possibilities)))
	  ;; Additionally, find the best prefix.
	  (cond ((or (not prefix-p)
		     (= entry-len len)))
		((null best-match)
		  (setq best-match entry best-len entry-len))
		(t
		  (let* ((best (car best-match))
			 (new-len (rgr-prefix-length string best best-len)))
		    (setq best-len new-len)
		    (setq best-match entry)))))))
      word t)
    (setq possibilities (sort possibilities (function rgr-better-match-p)))
    (setq rgr-abbrev-completion-state
	  (cons nil (cons 0 possibilities)))
    (cond (no-expand-p nil)
	  ((null best-match)
	    (rgr-abbrev-insert-next-possibility word 0))
          ((> best-len len)
	    (rgr-abbrev-insert-word-tail word best-match best-len))
          (t
	    (rgr-abbrev-show-possibilities possibilities)))))

(defun rgr-complete-word-start (&optional no-abbrev-p no-expand-p)
  ;; Start a new completion.
  (let* ((word-start (rgr-buffer-word-start (point)))
	 (word (buffer-substring-no-properties word-start (point))))
    (if (< (length word) 3)
	;; Fixed limitation of the hash/search algorithm.
	(error "Must have at least three characters."))
    (rgr-abbrev-expand-initial-guess word-start word
				     no-abbrev-p no-expand-p)))

;;;###autoload
(defun rgr-insert-symbol-abbreviation ()
  ;; New UI rgr-insert-symbol-abbreviation pass.  -- rgr, 1-Feb-98.
  "Expand the quasi-abbrev before point."
  (interactive)
  (let ((state (rgr-abbrev-completion-state)))
    (if (not state)
	;; Starting a new completion.
	(rgr-complete-word-start)
	;; Continuing a previous completion attempt.
	(let* ((replacement-string
		 (rgr-abbrev-completion-state-replacement-string))
	       (n-poss (length (cdr (cdr rgr-abbrev-completion-state))))
	       (next-possibility
		 (mod (1+ (car (cdr rgr-abbrev-completion-state))) n-poss)))
	  (cond ((eq state ':continue)
		  ;; adding characters works only if we're doing prefix
		  ;; completion.
		  (rgr-abbrev-expand-initial-guess
		    (rgr-abbrev-completion-state-insertion-start)
		    replacement-string t))
		;; so (eq state ':repeat) must be true.
		((= n-poss 1)
		  (message "Sole completion."))
		(t
		  (rgr-abbrev-insert-next-possibility replacement-string
						      next-possibility)))))
    ;; [debugging.  -- rgr, 1-Feb-98.]
    '(message "Old state is %s, new state is %s"
	     state (rgr-abbrev-completion-state))
    '(sit-for 1)))

(defun rgr-list-abbreviations ()
  "Lists possibilities for the quasi-abbrev before point."
  (interactive)
  (or (rgr-abbrev-completion-state)
      (rgr-complete-word-start nil t))
  (let* ((tail (cdr (cdr rgr-abbrev-completion-state)))
	 (ptr (car (cdr rgr-abbrev-completion-state)))
	 (i 0)
	 (n-poss (length tail)))
    (with-output-to-temp-buffer "*Completion Possibilities*"
      (princ (if (= n-poss 1)
		 "There is exactly one possibility:\n\n"
		 (format "There are %d possibilities:\n\n" n-poss)))
      (while tail
	(princ (if (= i ptr) " * " "   "))
	(princ (car (car tail)))
	(princ "\n")
	(setq tail (cdr tail))
	(setq i (1+ i)))
      (princ "\nCurrent (or next) is marked with a \"*\".\n"))))

;;;; Other completion utilities.

(defun rgr-clean-symbol-prefixes (verbose-p)
  "Removes all unused 'symbols' that are prefixes of others."
  (interactive "P")
  (let ((count 0))
    (let ((i 0))
      (while (< i rgr-string-table-size)
	(let ((entries (aref rgr-string-table i)))
	  (let ((<tail> entries)
		(pair nil))
	    (while <tail>
	      (setq pair (car <tail>))
	      (let ((string (car pair)))
		(cond ((eq (rgr-string-case string) ':upper)
			(message "Found uppercase entry %s; fixing." pair)
			(let ((<limit> (length string))
			      (i 0))
			  (while (< i <limit>)
			    (aset string i (downcase (aref string i)))
			    (setq i (1+ i))))))
		(if (numberp (cdr pair))
		    (cond ((or (< (length string)
				  rgr-completion-min-entry-length)
			       (let ((<length> (length string))
				     (<index> 0)
				     (<found> nil))
				 (while (and (< <index> <length>)
					     (not <found>))
				   (if (= 32 (elt string <index>))
				       (setq <found> <index>))
				   (setq <index> (1+ <index>)))
				 <found>)
			       (and (= (cdr pair) 0)
				    (rgr-prefix-exists-p string entries)))
			    (if verbose-p
				(message "Flushing %s." pair))
			    (setq count (+ count 1))
			    (rplacd pair -1)
			    (setq entries (delq pair entries))))))
	      (setq <tail> (cdr <tail>))))
	  (aset rgr-string-table i entries))
	nil
	(setq i (1+ i))))
    (let ((i 0))
      (while (< i rgr-string-table-size)
	(let ((<tail> (aref rgr-string-table i))
	      (pair nil))
	  (while <tail>
	    (setq pair (car <tail>))
	    (cond ((listp (cdr pair))
		    (let ((<tail> (cdr pair))
			  (word nil))
		      (while <tail>
			(setq word (car <tail>))
			(if (= (cdr word) -1)
			    (rplacd pair (delq word (cdr pair))))
			(setq <tail> (cdr <tail>))))
		    (cond ((null (cdr pair))
                            (if verbose-p
				(message "Flushing %s." pair))
                            (aset rgr-string-table i
				  (delq pair (aref rgr-string-table i)))))))
	    (setq <tail> (cdr <tail>))))
	(setq i (1+ i))))
    (message "Removed %d unused prefix%s."
	     count (if (= count 1) "" "es"))))

;;;; Completion I/O

(defmacro with-rgr-temp-file-buffer (buffer-and-filename &rest body)
  "Puts us in the buffer, finding the file if necessary, and restoring
the original state if the file isn't already in a buffer."
  (let ((buffer (car buffer-and-filename))
	(filename (car (cdr buffer-and-filename))))
    (` (let (((, buffer) (get-file-buffer (, filename)))
	     (<delete-p> nil))
	 ;; I'm hoping at least some of this is correct.
	 (unwind-protect
	      (save-excursion
		(cond ((not (, buffer))
			(setq (, buffer) (find-file-noselect (, filename)))
			(setq <delete-p> t)))
		(set-buffer (, buffer))
		(,@ body))
	   (and (, buffer) <delete-p>
		(kill-buffer (, buffer))))))))

(defun rgr-write-completions-internal (stream threshold atrophy-percent)
  ;; just outputs the current completion state to the stream.
  (princ (format ";; Completions written %s\n\n" (current-time-string)) stream)
  (let ((total 0) (written 0)
	(i 0))
    (while (< i rgr-string-table-size)
      (let* ((entries (aref rgr-string-table i))
	     (<tail> entries)
	     (entry nil))
	(while <tail>
	  (setq entry (car <tail>))
	  (if (numberp (cdr entry))
	      (let ((string (car entry))
		    (uses (cdr entry)))
		(setq total (+ total 1))
		;; (message "%S" entry)  (sit-for 1)
		(cond ((>= uses threshold)
			(setq written (+ written 1))
			(princ (format "(%S . %S)\n"
				       string
				       (/ (* uses atrophy-percent) 100))
			       stream)))))
	  (setq <tail> (cdr <tail>))))
      (setq i (1+ i)))
    (princ (format "\n;; %d out of %d completions written.\n"
		   written total)
           stream)))

(defun rgr-abbrev-completion-file-newer-than-one-day-p (filename)
  ;; Return non-nil if the file is less than a day old.
  (let ((then (nth 5 (file-attributes filename))))
    (or (null then)	;; missing file
	(let* ((now (current-time))
	       (delta-high (- (car now) (car then)))
	       (delta-low (- (car (cdr now)) (car (cdr then))))
	       (delta-secs (+ (* delta-high 65536) delta-low)))
	  (and (< delta-high 10)	;; guard against integer overflow!
	       (< delta-secs (* 24 60 60)))))))

(defun rgr-write-completion-file (filename &optional threshold atrophy-percent)
  ;; apply threshold first, atrophy-percent afterwards, and write whatever is
  ;; left over to the file.
  (interactive
    (list (let ((default (or rgr-abbrev-completion-save-file
			      (buffer-file-name)
			      (expand-file-name "completions.text"))))
	    (read-file-name (format "Save into (default %s)? "
				    (abbreviate-file-name default))
			    (file-name-directory default)
			    default))))
  ;; default the numeric args.
  (or threshold
      (setq threshold 1))
  (or atrophy-percent
      (setq atrophy-percent 80))
  (with-rgr-temp-file-buffer (buffer filename)
    (cond ((if (memq rgr-abbrev-completion-append-to-file-p '(nil t))
	       rgr-abbrev-completion-append-to-file-p
	       (funcall rgr-abbrev-completion-append-to-file-p filename))
	    ;; Appending; go ahead and save edited changes.
	    (goto-char (point-max)))
	  (t
	    ;; Flushing old completions, so don't screw the user if the buffer
	    ;; has been edited.  -- rgr, 12-Dec-96.
	    (if (buffer-modified-p)
		(error "Buffer %s has been edited; %s."
		       (buffer-name) "can't save completions into it"))
	    (erase-buffer)))
    (rgr-write-completions-internal buffer threshold atrophy-percent)
    (let ((delete-old-versions t))
      (save-buffer))
    (run-hooks 'rgr-abbrev-after-save-hook)))

(defun rgr-completion-do-auto-save ()
  ;; Run indirectly via the rgr-completion-start-auto-save feature.
  (and rgr-abbrev-completion-save-file
       (rgr-write-completion-file rgr-abbrev-completion-save-file)))

(defun rgr-completion-stop-auto-save (&optional no-message-p)
  (cond ((null rgr-completion-auto-save-timer)
	  (or no-message-p
	      (message "Completion auto-save is not enabled."))
	  nil)
	(t
	  (cancel-timer rgr-completion-auto-save-timer)
	  (setq rgr-completion-auto-save-timer nil)
	  (or no-message-p
	      (message "Completion auto-save is now disabled."))
	  t)))

(defun rgr-completion-start-auto-save ()
  "Start auto-save for the abbrev-completion feature.  Filtered
completions will be saved normally to the file named by
rgr-abbrev-completion-save-file at a random time between 01:00 and
02:00."
  (interactive)
  (require 'timer)
  (if rgr-completion-auto-save-timer
      (rgr-completion-stop-auto-save t))
  (or rgr-abbrev-completion-save-file
      (error "No save file in %S; set this first."
	     'rgr-abbrev-completion-save-file))
  (let* ((desired-hour 1)
	 (now (current-time))
	 (tomorrow (decode-time (timer-relative-time now (* 24 60 60))))
	 (this-hour (nth 2 tomorrow))
	 (time (append (list (random 60) (random 60) desired-hour)
		       (nthcdr 3 (if (< this-hour desired-hour)
				     ;; do it later today.
				     (decode-time now)
				     tomorrow))))
	 (timer (timer-create)))
    (timer-set-time timer (apply 'encode-time time))
    (timer-set-function timer 'rgr-completion-do-auto-save)
    (timer-activate timer)
    (setq rgr-completion-auto-save-timer timer)
    (message "Auto-save set for 0%d:%2d."
	     (car (cdr (cdr time))) (car (cdr time)))
    timer))

(defun rgr-read-completion-file-internal (stream)
  ;; Read and merge with the existing database.  If an entry exists and the
  ;; lengths differ, we set it to the max of the two.
  (let ((file-entry nil))
    (while (setq file-entry 
		 (condition-case error
		     (read (current-buffer))
		   (end-of-file nil)))
      (let ((count (cdr file-entry))
	    (entry (rgr-abbrev-hash-name (car file-entry))))
        (if (and entry (< (cdr entry) count))
	    (rplacd entry count))))))

;;;###autoload
(defun rgr-read-completion-file (filename)
  (with-rgr-temp-file-buffer (buffer filename)
    (goto-char (point-min))
    (rgr-read-completion-file-internal buffer)))

(defun rgr-save-completion-hack ()
  ;; Run at C-x C-c time.  Should really have a nicer notion of what the right
  ;; file is, whether it has been modified, etc.  -- rgr, 29-Nov-96.
  (and rgr-abbrev-completion-save-file
       (rgr-write-completion-file rgr-abbrev-completion-save-file))
  t)

;;;###autoload
(defun rgr-abbrev-generate-weekly-save-file ()
  "Helper function for rgr-install-weekly-completion-cycle -- generates a file
name with the day of the week in it."
  (setq rgr-abbrev-completion-save-file
	(expand-file-name (concat "completions-"
				  (substring (current-time-string) 0 3)
				  ".text")
			  rgr-abbrev-completion-save-directory)))

;;;###autoload
(defun rgr-install-weekly-completion-cycle ()
  (let ((files (directory-files rgr-abbrev-completion-save-directory
				t "^completions.*\\.text$")))
    (while files
      (rgr-read-completion-file (car files))
      (setq files (cdr files)))
    (rgr-abbrev-generate-weekly-save-file)
    (add-hook 'rgr-abbrev-after-save-hook
	      'rgr-abbrev-generate-weekly-save-file)))

;;;###autoload
(defun rgr-install-abbrev-completion ()
  "Install hooks for abbrev completion.
This is meant for calling from a .emacs file, but is also a command so
it can be re-initialized if any of the hooks get stepped on."
  (interactive)
  (add-hook 'find-file-hooks 'rgr-abbrev-find-file-hook)
  (add-hook 'kill-emacs-query-functions 'rgr-save-completion-hack)
  (add-hook 'post-command-hook 'rgr-maybe-learn-new-symbol))

(provide 'rgr-abbrev-completion)

;;; Debugging

; escape value (though emacs makes it go away if it gets an error).
;(setq post-command-hook nil)
;(rgr-read-completion-file "~rogers/emacs/completions.text")
;(rgr-write-completion-file "~rogers/emacs/completions.text")
