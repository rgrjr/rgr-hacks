;;;*****************************************************************************
;;;
;;;    The bagels game.
;;;
;;;   This is based on the ancient computer game by the People's Computer
;;; Company (but I don't have the "My Computer Loves Me When I Speak In BASIC"
;;; book to cite it properly).  The current version allows repeated symbols,
;;; which the original does not (but it would not be hard to support both).
;;;
;;;    emacs lisp is a hacker's dream.  The original implementation of this was
;;; 180 lines (including comments), and took less than two hours.  -- rgr,
;;; 5-Mar-96.
;;;
;;;    Modification history:
;;;
;;; created.  -- rgr, 5-Mar-96.
;;; bagels-position: eql -> eq.  -- rgr, 1-May-00.
;;;

(defvar bagels-available-symbols "!@$%&*+=;~"
  "Set of symbols from which to construct a code word to guess.
Don't change this unless you know which ones are special to the code.")
(defvar bagels-number-of-symbols 7
  "*Number of symbols to choose from when making a code word.")
(defvar bagels-symbols nil
  "Subset of bagels-available-symbols of length bagels-number-of-symbols
-- the symbols that the user can use in a guess.")
(defvar bagels-code-word-length 4
  "*Length of the code word to make up.")
(defvar bagels-code-word nil
  "The code word itself.")
(defvar bagels-game-in-progress-p nil)
(defvar bagels-guess-number 1)

(defvar bagels-mode-map (make-sparse-keymap))

(defun bagels-start-new-game ()
  "Make up a code word, and tell the user about it."
  (interactive)
  (let ((i 0))
    (setq bagels-code-word (make-string bagels-code-word-length 0))
    (setq bagels-symbols
	  (substring bagels-available-symbols 0 bagels-number-of-symbols))
    (while (< i bagels-code-word-length)
      (aset bagels-code-word i
	    (aref bagels-available-symbols (random bagels-number-of-symbols)))
      (setq i (1+ i))))
  (goto-char (point-max))
  (if (not (bobp))
      (insert "\n\n"))
  (insert "I am thinking of a `word' "
	  (prin1-to-string bagels-code-word-length)
	  " characters long, using symbols from the\n"
	  "string \"" bagels-symbols "\", possibly with repeats.  Type "
	  (substitute-command-keys "\\[bagels-make-guess]")
	  " to guess it,\n"
	  (substitute-command-keys "\\[bagels-give-up]")
	  " to give up.  `fermi' means right symbol, right place;\n"
	  "`pico' means right symbol, wrong place; "
	  "`bagels' means nothing right."
	  "\n\n1: ")
  (setq bagels-game-in-progress-p t
	bagels-guess-number 1))

(defun bagels-mode ()
  ;; Installs the mode.
  (kill-all-local-variables)
  (use-local-map bagels-mode-map)
  (setq major-mode 'bagels-mode)
  (setq mode-name "Bagels")
  (mapcar 'make-local-variable '(bagels-number-of-symbols bagels-symbols
				 bagels-code-word-length bagels-code-word
				 bagels-game-in-progress-p bagels-guess-number))
  (bagels-start-new-game))

;;; The core of the game itself.

(defun bagels-position (elt vector)
  (let ((len (length vector)) (i 0) (result nil))
    (while (< i len)
      (if (eq elt (aref vector i))
	  (setq result i i len)
	  (setq i (1+ i))))
    result))

(defun bagels-score-guess (guess)
  ;; Score a guess that's not completely correct.
  (insert "  -> ")
  (let ((n-right-exactly 0) (n-right-in-wrong-place 0)
	(space ?\ )
	;; Scratch copy.
	(code-word (concat "" bagels-code-word)))
    (let ((i 0))
      (while (< i bagels-code-word-length)
	(cond ((eq (aref guess i) (aref code-word i))
		(setq n-right-exactly (1+ n-right-exactly))
	        (insert " fermi")
	        (aset code-word i space)
	        (aset guess i space)))
	(setq i (1+ i))))
    (let ((i 0))
      (while (< i bagels-code-word-length)
	(let* ((guess-char (aref guess i))
	       (pos (bagels-position guess-char code-word)))
	  (cond ((and pos
		      (not (eq guess-char space)))
		  (setq n-right-in-wrong-place (1+ n-right-in-wrong-place))
		  (insert " pico")
		  (aset code-word pos space)
		  (aset guess i space))))
	(setq i (1+ i))))
    (if (and (zerop n-right-exactly) (zerop n-right-in-wrong-place))
	(insert " bagels"))))

(defun bagels-find-guess ()
  ;; Helper function for bagels-make-guess, below.
  (end-of-line)
  (if (not (eobp))
      (error "Make your guess on the last line."))
  (beginning-of-line)
  (if (not (looking-at "^[0-9]+:[ \t]*"))
      (error "Last line screwed up?  Try undoing & re-guessing . . ."))
  (goto-char (match-end 0))
  (let ((guess-chars nil) (guess-start (point))
	(count 0) (char (char-after (point))))
    (while char
      (cond ((null (bagels-position char bagels-symbols))
	      (error "%c is not one of the symbols; use \"%s\" instead."
		     char bagels-symbols))
	    (t
	      (setq guess-chars (cons char guess-chars))
	      (setq char (cond ((not (eobp))
				 (forward-char)
				 (char-after (point))))))))
    (if (looking-at "[ \t\n]+")
	(replace-match ""))
    (setq guess-chars (concat (nreverse guess-chars)))
    (cond ((< (length guess-chars) bagels-code-word-length)
	    (error "Guess is too short -- it should be %d long."
		   bagels-code-word-length))
	  ((> (length guess-chars) bagels-code-word-length)
	    (error "Guess is too long -- it should be %d long."
		   bagels-code-word-length)))
    guess-chars))

(defun bagels-make-guess ()
  (interactive)
  (cond (bagels-game-in-progress-p
	  (let ((guess (bagels-find-guess)))
	    ;; (message "Guess is `%s'." guess)
	    (cond ((equal guess bagels-code-word)
		    (insert "  YESSS!!!")
		    (bagels-start-new-game))
		  (t
		    (bagels-score-guess guess)
		    (setq bagels-guess-number (1+ bagels-guess-number))
		    (insert "\n" (prin1-to-string bagels-guess-number) ": ")))))
	((y-or-n-p "No game in progress; start a new one? ")
	  (bagels-start-new-game))))

(defun bagels-give-up ()
  (interactive)
  (insert "\n  The answer was \"" bagels-code-word "\".\n")
  (setq bagels-game-in-progress-p nil))

(define-key bagels-mode-map "\n" 'bagels-make-guess)
(define-key bagels-mode-map "\r" 'bagels-make-guess)

;;; Obligatory command to start a game.

;;;###autoload
(defun bagels ()
  "Play bagels in a buffer of our own."
  (interactive)
  (switch-to-buffer (get-buffer-create "*bagels*"))
  (or (eq major-mode 'bagels-mode)
      (bagels-mode))
  (or bagels-game-in-progress-p
      (bagels-start-new-game)))

;; In case anyone wants to "require" this.

(provide 'bagels)

