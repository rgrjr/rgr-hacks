;;;*****************************************************************************
;;;
;;;; Hacking perl mode.
;;;
;;;    Modification history:
;;;
;;; created with rgr-perl-quick-arglist hack.  -- rgr, 26-Jul-96.
;;; rgr-get-man-buffer: now does the unspeakable.  -- rgr, 2-Aug-96.
;;; rgr-get-man-buffer: fix (require 'man) bug.  -- rgr, 7-Aug-96.
;;; rgr-perl-show-documentation: new.  -- rgr, 8-Aug-96.
;;; rgr-reinstall-scripts: new hack.  -- rgr, 12-Aug-96.
;;; rgr-add-to-perl-modification-history: new hack.  -- rgr, 13-Aug-96.
;;; rgr-perl-show-arglist: msg instead of showing doc buf.  -- rgr, 23-Oct-96.
;;; rgr-reinstall-scripts: add verbose-p arg.  -- rgr, 2-Dec-96.
;;; rgr-perl-mode-hook: add rgr-c-electric-dash.  -- rgr, 20-Dec-96.
;;; rgr-perl-mode-hook: (executable-set-magic "perl").  -- rgr, 29-Apr-97.
;;; rgr-perl-mode-hook: no electric-perl-terminator on ':'.  -- rgr, 16-May-97.
;;; rgr-perl-get-name-around-point: made much smarter.  -- rgr, 13-Aug-97.
;;; rgr-perl-definition-name: new.  -- rgr, 14-Oct-97.
;;; rgr-perl-get-name-around-point: allow (e.g.) -T.  -- rgr, 27-Jan-98.
;;; rgr-perl-function-documentation-prefix: OSF1 hack.  -- rgr, 29-Apr-98.
;;; rgr-perl-mode-hook: executable-set-magic path kludge.  -- rgr, 22-May-98.
;;; rgr-get-man-buffer: err if magic line not found.  -- rgr, 11-Sep-98.
;;; rgr-perl-mode-hook: Learn subroutine names.  -- rgr, 14-Dec-98.
;;; rgr-perl-mode-hook: rgr-comment-region-lisp.  -- rgr, 7-Sep-99.
;;; rgr-get-man-buffer: try to make more robust.  -- rgr, 13-Sep-99.
;;; rgr-perl-mode-hook: linux executable-set-magic version.  -- rgr, 16-Apr-00.
;;; rgr-perldoc: new hack.  -- rgr, 9-May-00.
;;; rgr-perldoc: must require man.  -- rgr, 20-Jun-00.
;;; rgr-perl-function-documentation-prefix: linux case.  -- rgr, 2-Sep-00.
;;; rgr-perl-mode-hook: prefer /usr/bin/perl for #! magic.  -- rgr, 22-Oct-02.
;;; rgr-perl-function-documentation-prefix: add SuSE support.  -- rgr, 4-Nov-02.
;;; rgr-perl-newline-and-maybe-indent: new POD hack.  -- rgr, 8-Nov-02.
;;; rgr-perl-mode-hook: use "/usr/bin/perl -w" magic.  -- rgr, 10-Jan-03.
;;;

;;;; Installation hacks.

;; This is here for lack of a better place.  Pretty hackish in any case.  --
;; rgr, 12-Aug-96.

(defvar rgr-install-directory (expand-file-name "~thread/code/bin/"))

;;;###autoload
(defun rgr-reinstall-scripts (&optional verbose-p)
  "Check scripts in this directory vs the rgr-install-directory variable.
A numeric arg comments on the up-to-dateness of all files in both dirs."
  (interactive "P")
  (let ((tail
	  ;; The cdring skips "." and "..".
	  (cdr (cdr (directory-files rgr-install-directory))))
	(count 0))
    (while tail
      (let* ((source (car tail))
	     (dest (concat rgr-install-directory source)))
	(cond ((not (and (file-readable-p source) (file-readable-p dest))))
	      ((file-newer-than-file-p source dest)
		(message "%s needs installing." source)
	        (sit-for 1)
	        (setq count (1+ count)))
	      (verbose-p
		(message "%s is up to date in %s." source rgr-install-directory)
	        (sit-for 1))))
      (setq tail (cdr tail)))
    (if (zerop count)
	(message "%s is up to date wrt %s."
		 rgr-install-directory default-directory)
	(message "%d file%s installed in %s."
		 count (if (= count 1) "" "s") rgr-install-directory))))

;;;; general stuff.

(defun rgr-perl-definition-name ()
  (if (re-search-backward "^sub[ \t\n]+\\([a-zA-Z_]+\\)" nil t)
      (match-string 1)))

(defun rgr-add-to-perl-modification-history (&optional insert-definition-name-p)
  ;; [syntax-independent version.  -- rgr, 13-Aug-96.]
  "Add to a modification history near the top of the file.
Sets the mark before moving there, and starts a new line before the end
of the perl comment.  If no history exists (which it determines by
searching for the string in the rgr-modification-history-herald
variable), then you are asked about starting one.  (If you are asked
this when there already is one, then somebody probably inserted extra
crud at the beginning of the file.)  If given a numeric argument,
inserts the definition name (if it can find one) [presently nonworking
for perl syntax]."
  (interactive "P")
  (rgr-add-to-modification-history-internal
    "#    " "^# *" "#" "^#* *$"
    (if insert-definition-name-p
	(rgr-perl-definition-name))))

;;;; perl documentation support.

(defvar rgr-perl-function-documentation-prefix
        (cond ((string-match "osf" system-configuration)
		;; only two spaces in front of function names in the man page.
		;; [I hope that this really depends on OSF1 . . .  -- rgr,
		;; 29-Apr-98.]
		"^  ")
	      ((string-match "linux" system-configuration)
	        ;; "*-linux-gnu" and "*-suse-linux" tested.
		"^       ")
	      (t
		;; Default (works on SunOS 4.1.4 at least).
		"^     "))
  "Regular expression that matches the precise indentation in front of
the function name in the perl documentation.  This appears to be
somewhat system-dependent.")

(defvar rgr-manpage-done-p nil)
(defvar rgr-perl-manpage-alphabetical-listing nil
  "Start of alphabetical function listing.")

(defun rgr-manpage-done () (setq rgr-manpage-done-p t))

(defun rgr-get-man-buffer (man-args)
  ;; Really rude hack to get the manpage we want.  [should pick a less general
  ;; name, especially since we always set rgr-perl-manpage-alphabetical-listing
  ;; -- and err out if it's not found.  -- rgr, 13-Sep-99.]
  (let ((buffer-name (concat "*Man " man-args "*")))
    (require 'man) ;; needed to force definition of Man-mode-hook
    (let ((buffer
	    (or (get-buffer buffer-name)
		(let ((Man-notify-method 'meek)
		      (rgr-manpage-done-p nil)
		      (Man-mode-hook (cons 'rgr-manpage-done Man-mode-hook)))
		  (man man-args)
		  (while (not rgr-manpage-done-p)
		    (message "Waiting for %s man page . . ." man-args)
		    (sit-for 1))
		  (get-buffer buffer-name))
		(error "%S hack didn't work for %S."
		       'rgr-get-man-buffer man-args))))
      ;; make sure that rgr-perl-manpage-alphabetical-listing is initialized.
      (or rgr-perl-manpage-alphabetical-listing
	  (save-excursion
	    (set-buffer buffer)
	    (goto-char (point-min))
	    ;; Hack to find the actual function listings.
	    (search-forward "Alphabetical Listing of Perl Functions")
	    (forward-line)
	    (setq rgr-perl-manpage-alphabetical-listing (point))))
      buffer)))

(defun rgr-perl-get-name-around-point ()
  (let ((identifier-chars "a-zA-Z0-9_&:"))
    (save-excursion
      ;; This may not move us anywhere, but at least it ensures that we are next
      ;; to an identifier.
      (if (not (looking-at (concat "[" identifier-chars "]")))
	  (skip-chars-backward (concat "^" identifier-chars)))
      (let ((start (save-excursion
		     (skip-chars-backward identifier-chars)
		     (point)))
	    (end (save-excursion
		   (skip-chars-forward identifier-chars)
		   (point))))
	;; [this allows -r and -T (for example), but we don't know how to find
	;; their documentation yet.  -- rgr, 27-Jan-98.]
	(if (and (= (- end start) 1)
		 (= (char-after (1- start)) ?-))
	    (setq start (1- start)))
	(buffer-substring start end)))))

;;;###autoload
(defun rgr-perl-show-documentation (name)
  (interactive
    (list (rgr-perl-prompt-for-name "Find documentation for perl function")))
  (let ((regexp (concat rgr-perl-function-documentation-prefix
			(regexp-quote name)
			" "))
	(man-buffer (rgr-get-man-buffer "perlfunc")))
    (switch-to-buffer-other-window man-buffer)
    (push-mark)
    (goto-char rgr-perl-manpage-alphabetical-listing)
    (cond ((not (re-search-forward regexp nil t))
	    (pop-mark)
	    (error "Can't find %s documentation." name)))
    (goto-char (+ (match-beginning 0) 5))
    (recenter 0)
    (other-window -1)))

;;;###autoload
(defun rgr-perl-quick-documentation ()
  "Only works for builtins documented in the perlfunc man page."
  (interactive)
  (rgr-perl-show-documentation (rgr-perl-get-name-around-point)))

;;;###autoload
(defun rgr-perl-show-arglist (name)
  (interactive "sShow arglist for perl function: ")
  (let* (;; [this only works on SunOS; the documentation is indented by three
	 ;; less on the alphas.  -- rgr, 29-Apr-98.]
	 ;; (indent "     ") (long-indent "             ")
	 ;; (regexp (concat "^" indent name " "))
	 (regexp (concat rgr-perl-function-documentation-prefix
			 (regexp-quote name)
			 " "))
	 (man-buffer (rgr-get-man-buffer "perlfunc")))
    (or (save-excursion
	  (set-buffer man-buffer)
	  (goto-char rgr-perl-manpage-alphabetical-listing)
	  (cond ((not (re-search-forward regexp nil t))
		 (error "Can't find %s documentation." name)))
	  ;; In case it fits on one line, print the arglist Lisp style (approx).
	  (message "%s" (buffer-substring
			  (progn (goto-char (match-beginning 0))
				 (skip-chars-forward " \t")
				 (point))
			  (progn (end-of-line)
				 (point))))
	  (forward-line)
	  ;; We were successful if the next line is the indented paragraph
	  ;; describing the function.  If not, it will be a blank line
	  ;; separating the next argument pattern.
	  (looking-at rgr-perl-function-documentation-prefix))
	;; Multiline; have to display the buffer.
	(rgr-perl-show-documentation name))))

;;;###autoload
(defun rgr-perl-quick-arglist ()
  "Only works for builtins documented in the perlfunc man page."
  (interactive)
  (rgr-perl-show-arglist (rgr-perl-get-name-around-point)))

;;; perldoc interface.

(defun rgr-perl-prompt-for-name (prompt-string-start)
  (let* ((default (rgr-perl-get-name-around-point))
	 (result (read-string
		  (format "%s%s: "
			  prompt-string-start
			  (if default
			      (concat " (default '" default "')")
			      ""))
		  nil nil default)))
    result))

(defvar rgr-perldoc-program "perldoc")
(defvar rgr-perldoc-args "")

;;;###autoload
(defun rgr-perldoc (name)
  "Find perldoc documentation, e.g. for a perl module."
  (interactive
    (list (rgr-perl-prompt-for-name "Find perlpod documentation for")))
  ;; Need to require this first, since if man autoloads while we have these
  ;; variables bound, they will be left unbound after we're done, to the
  ;; detriment of M-x man.  -- rgr, 20-Jun-00.
  (require 'man)
  (let ((manual-program rgr-perldoc-program)
	(Man-switches rgr-perldoc-args))
    (man name)))

;;; Indentation.

(defvar rgr-perl-function-indent 0)

(defun rgr-perl-indent-command ()
  "Indent current line as Perl code.  [Simplified version.  -- rgr, 4-Aug-97.]"
  (interactive)
  (let ((fn-start 0) (rgr-perl-function-indent 0))
    (save-excursion
      (setq fn-start (perl-beginning-of-function))
      (beginning-of-line)
      (skip-chars-forward " \t\f")
      (setq rgr-perl-function-indent (current-column)))
    (perl-indent-line nil fn-start)))

;; [this backfires, because perl-beginning-of-function only goes to the previous
;; "sub" form, not the *matching* sub.  so after the nested subroutine, lines
;; get indented by 0, because it thinks we're outside a top-level form.  -- rgr,
;; 4-Aug-97.]
'(defadvice calculate-perl-indent (around rgr-perl-function-indent activate)
  (let ((result ad-do-it))
    (if (numberp result)
	(+ result rgr-perl-function-indent)
	result)))

(defun rgr-perl-newline-and-maybe-indent (arg)
  (interactive "p")
  (newline arg)
  (if (save-excursion
	(or (not (re-search-backward "^=" nil t))
	    (looking-at "^=cut")))
      (rgr-perl-indent-command)))

;;;###autoload
(defun rgr-perl-mode-hook ()
  ;; Swap newline and return character bindings (to get indentation by default).
  ;; See also the rgr-define-lisp-mode-commands comment.  -- rgr, 13-Jun-96.
  ;; Also, use a simplified indent.  -- rgr, 4-Aug-97.
  (define-key perl-mode-map "\r" 'rgr-perl-newline-and-maybe-indent)
  (define-key perl-mode-map "\n" 'newline)
  (define-key perl-mode-map "\t" 'rgr-perl-indent-command)
  ;; Don't do indenting on ";" -- ${$foo} loses.  -- rgr, 16-May-97.
  (define-key perl-mode-map ";" 'self-insert-command)
  ;; Try to avoid shifting.  -- rgr, 20-Dec-96.
  (define-key perl-mode-map "-" 'rgr-c-electric-dash)
  ;; Learn subroutine names.  -- rgr, 14-Dec-98.
  (make-local-variable 'rgr-definition-line-regexp)
  (setq rgr-definition-line-regexp "^ *sub +")
  (rgr-relearn-buffer-definition-names)
  ;; Function arguments and documentation from the man page.  -- rgr, 26-Jul-96.
  (define-key perl-mode-map "\C-za" 'rgr-perl-quick-arglist)
  (define-key perl-mode-map "\C-zd" 'rgr-perl-quick-documentation)
  ;; Standard modification history.
  (define-key perl-mode-map "\M-*" 'rgr-add-to-perl-modification-history)
  ;; This is also inherited from the global map, but define it here for sure.
  (define-key perl-mode-map "\M-q" 'rgr-fill-script-comment)
  ;; Indent comments, even at BOL (by dropping ";?#\\|" from the start of this
  ;; regular expression).  It is also necessary to use rgr-perl-indent-command,
  ;; since perl-indent-command overrides the regexp.  -- rgr, 4-Aug-97.
  (setq perl-nochange "\f\\|\\s(\\|\\(\\w\\|\\s_\\)+:")
  ;; Put in interpreter magic.  -- rgr, 29-Apr-97.  [but not in library modules.
  ;; -- rgr, 16-May-97.]  [kludge:  have to hardwire the /usr/local/bin/perl
  ;; path, because it is the only possibility that exists on all three machine
  ;; types [at BMERC].  the alphas prefer /usr/bin/perl, and the solaris
  ;; machines prefer /bin/perl, both based on the $PATH setting.  -- rgr,
  ;; 22-May-98.]  [but /usr/local/bin/perl doesn't exist in standard RH Linux,
  ;; so use /usr/bin/perl instead.  -- rgr, 16-Apr-00.]
  (cond ((not (string-match "\.pr?l$" (buffer-file-name))))
	;; [while the "real" perl is now in /usr/local/bin/, /usr/bin/perl is
	;; standard, so we need a link from there anyway.  -- rgr, 22-Oct-02.]
	((file-executable-p "/usr/bin/perl")
	  (require 'executable)
	  (executable-set-magic "/usr/bin/perl -w"))
	((file-executable-p "/usr/local/bin/perl")
	  (require 'executable)
	  (executable-set-magic "/usr/local/bin/perl")))
  ;; Version dependent stuff.
  (rgr-emacs-major-version-case
   ((19 20 lucid19)
     ;; Shadow global comment-region-lisp binding.  [but this loads ilisp, which
     ;; is a bother . . .  -- rgr, 26-Jul-96.]  [replaced with my own hack.  --
     ;; rgr, 7-Sep-99.]
     (define-key perl-mode-map [?\C-x ?\C-\;] 'rgr-comment-region-lisp))))

;; (setq debug-on-error nil)
