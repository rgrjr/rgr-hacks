;;;*****************************************************************************
;;;
;;;; Hacking perl mode.
;;;
;;; Copyright (C) 1996-2003  Robert G. Rogers Jr
;;;
;;; This file is not part of GNU Emacs, but it customizes perl-mode, which is.
;;; Accordingly, it is distributed under the same terms as GNU Emacs:
;;;
;;;	GNU Emacs is free software; you can redistribute it and/or modify
;;;	it under the terms of the GNU General Public License as published by
;;;	the Free Software Foundation; either version 2, or (at your option)
;;;	any later version.
;;;
;;;	GNU Emacs is distributed in the hope that it will be useful,
;;;	but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;	GNU General Public License for more details.
;;;
;;;	You should have received a copy of the GNU General Public License
;;;	along with GNU Emacs; see the file COPYING.  If not, write to the
;;;	Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;;	Boston, MA 02111-1307, USA.
;;;
;;; Commentary:
;;;
;;;    To install the documentation lookup commands, put the following in your
;;; .emacs file:
;;;
;;;	(autoload 'rgr-perl-mode-install-documentation-hacks "rgr-perl-hacks")
;;;	(add-hook 'perl-mode-hook 'rgr-perl-mode-install-documentation-hacks)
;;;
;;; You should be aware that the use of "C-z" as a prefix violates the emacs
;;; key-binding standard.  "C-z" was chosen for compatibility with the
;;; "traditional" ilisp bindings for the corresponding functionality for Lisp
;;; development; these in turn were based on the MIT Lisp Machine Zmacs
;;; implementation.  In order to avoid stepping on other bindings, a user could
;;; choose to bind them to "C-c a", "C-c d", etc., instead.  [And even ilisp
;;; will soon change to conform to the emacs key binding standard, though the
;;; exact bindings are still being decided.  So stay tuned.  -- rgr, 30-Apr-03.]
;;;
;;;    You should also be aware that documentation lookup relies on the
;;; indentation of the "perlfunc" man page, which may vary from system to
;;; system.  If rgr-perl-show-documentation can't even find the documentation
;;; for "print", for example, you may need to tweak the value of the
;;; rgr-perl-function-documentation-prefix variable so that it exactly matches
;;; the whitespace that appears before the names of documented functions, and no
;;; more.
;;;
;;;    You might also want to look at rgr-perl-mode-fix-indentation and
;;; rgr-perl-mode-install-extra-hacks, but be aware that the latter includes
;;; commands that depend on other rgr-hacks functions not defined in this file.
;;;
;;;    Modification history:
;;;
;;; created with rgr-perl-quick-arglist hack.  -- rgr, 26-Jul-96.
;;; rgr-get-man-buffer: now does the unspeakable.  -- rgr, 2-Aug-96.
;;; rgr-get-man-buffer: fix (require 'man) bug.  -- rgr, 7-Aug-96.
;;; rgr-perl-show-documentation: new.  -- rgr, 8-Aug-96.
;;; rgr-add-to-perl-modification-history: new hack.  -- rgr, 13-Aug-96.
;;; rgr-perl-show-arglist: msg instead of showing doc buf.  -- rgr, 23-Oct-96.
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
;;;
;;; $Id$

;;;; variables.

(defvar rgr-perldoc-program "perldoc")
(defvar rgr-perldoc-args "")

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

(defvar rgr-perl-function-indent 0)

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

;;;###autoload
(defun rgr-perl-show-documentation (name)
  "Find the documentation for builtin functions on the perlfunc man page.
Prompts for the name of a perl function; the default is a name extracted
from the next near point.  The \"man perlfunc\" documentation page is
then shown in another window, scrolled down to the start of the
specified function.  If the documentation can't be found, the other
window is positioned at the start of the functional descriptions and
left selected, to make it easier to search the page manually."
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
    (goto-char (match-beginning 0))
    (skip-chars-forward " \t")
    (recenter 0)
    (other-window -1)))

;;;###autoload
(defun rgr-perl-quick-documentation ()
  "Find the documentation for builtins functions on the perlfunc man page.
See the rgr-perl-show-documentation command."
  (interactive)
  (rgr-perl-show-documentation (rgr-perl-get-name-around-point)))

;;;###autoload
(defun rgr-perl-show-arglist (name)
  "Find the arguments for builtins documented on the perlfunc man page.
Prompts for the name of a perl function; the default is a name extracted
from the next near point.  If only one variant of the function is
documented, then that line is shown in the message area at the bottom of
the frame.  If multiple variants are shown, then the documentation page
is shown in another window, as by the rgr-perl-show-documentation
command, except that if the function is not documented, a message is
printed and nothing is shown."
  (interactive (list (rgr-perl-prompt-for-name "Show arguments for")))
  (let* ((regexp (concat rgr-perl-function-documentation-prefix
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
  "Find the arguments for builtins documented on the perlfunc man page.
See the rgr-perl-show-arglist command."
  (interactive)
  (rgr-perl-show-arglist (rgr-perl-get-name-around-point)))

;;; perldoc interface.

;;;###autoload
(defun rgr-perldoc (name)
  "Find perldoc documentation, e.g. for a perl module.
This is like \\[man], but calls `perldoc' instead of `man' to request
the page."
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

(defun rgr-perl-indent-command ()
  "Indent current line as Perl code."
  ;; [simplified version.  -- rgr, 4-Aug-97.]  [may no longer be necessary, as
  ;; the perl-tab-to-comment default is now nil.  see the comments there,
  ;; especially the one by rms.  -- rgr, 30-Apr-03.]
  (interactive)
  (let ((fn-start 0) (rgr-perl-function-indent 0))
    (save-excursion
      (setq fn-start (perl-beginning-of-function))
      (beginning-of-line)
      (skip-chars-forward " \t\f")
      (setq rgr-perl-function-indent (current-column)))
    (perl-indent-line nil fn-start)))

(defun rgr-perl-newline-and-maybe-indent (arg)
  "Insert a newline and indent if not within POD."
  (interactive "p")
  (newline arg)
  (if (save-excursion
	(or (not (re-search-backward "^=" nil t))
	    (looking-at "^=cut")))
      (rgr-perl-indent-command)))

;;; Installing these commands.

;;;###autoload
(defun rgr-perl-mode-install-documentation-hacks ()
  ;; perl-mode-hook function that installs commands to get function arguments
  ;; and documentation from the man page.  See also the rgr-perldoc command.  --
  ;; rgr, 26-Jul-96.  [note that "C-z a" and "C-z d" are nonstandard bindings; i
  ;; chose them for compatibility with ilisp, but even ilisp will soon change.
  ;; a user could choose to bind them to "C-c a" and "C-c d", though.  -- rgr,
  ;; 30-Apr-03.]
  (define-key perl-mode-map "\C-za" 'rgr-perl-quick-arglist)
  (define-key perl-mode-map "\C-z\C-a" 'rgr-perl-show-arglist)
  (define-key perl-mode-map "\C-zd" 'rgr-perl-quick-documentation)
  (define-key perl-mode-map "\C-z\C-d" 'rgr-perl-quick-documentation))

;;;###autoload
(defun rgr-perl-mode-fix-indentation ()
  ;; perl-mode-hook function that installs a simplified version of the
  ;; indentation code.  This is necessary because the indenter often gets it
  ;; wrong.  -- rgr, 4-Aug-97.
  ;; Start by swapping newline and return character bindings (to get indentation
  ;; by default).
  (define-key perl-mode-map "\r" 'rgr-perl-newline-and-maybe-indent)
  (define-key perl-mode-map "\n" 'newline)
  (define-key perl-mode-map "\t" 'rgr-perl-indent-command)
  ;; Don't do indenting on ";" -- ${$foo} loses.  -- rgr, 16-May-97.
  (define-key perl-mode-map ";" 'self-insert-command)
  ;; Indent comments, even at BOL (by dropping ";?#\\|" from the start of this
  ;; regular expression).  It is also necessary to use rgr-perl-indent-command,
  ;; since perl-indent-command overrides the regexp.  -- rgr, 4-Aug-97.
  (setq perl-nochange "\f\\|\\s(\\|\\(\\w\\|\\s_\\)+:"))

;;;###autoload
(defun rgr-perl-mode-install-extra-hacks ()
  ;; These all require code defined in other rgr-*-hacks.el files.
  ;; Try to avoid shifting.  -- rgr, 20-Dec-96.
  (define-key perl-mode-map "-" 'rgr-c-electric-dash)
  ;; Learn subroutine names.  -- rgr, 14-Dec-98.
  (make-local-variable 'rgr-definition-line-regexp)
  (setq rgr-definition-line-regexp "^ *sub +")
  (rgr-relearn-buffer-definition-names)
  ;; Standard modification history.
  (define-key perl-mode-map "\M-*" 'rgr-add-to-perl-modification-history)
  ;; This is also inherited from the global map, but define it here for sure.
  (define-key perl-mode-map "\M-q" 'rgr-fill-script-comment)
  ;; Put in interpreter magic.  -- rgr, 29-Apr-97.  [but not in library modules.
  ;; -- rgr, 16-May-97.]  [/usr/bin/perl is more standard, so prefer that.  --
  ;; rgr, 22-Oct-02.]
  (cond ((not (string-match "\.pr?l$" (buffer-file-name))))
	((file-executable-p "/usr/bin/perl")
	  (require 'executable)
	  (executable-set-magic "/usr/bin/perl -w"))
	((file-executable-p "/usr/local/bin/perl")
	  (require 'executable)
	  (executable-set-magic "/usr/local/bin/perl -w")))
  (if (member rgr-emacs-major-version '(19 20 lucid19))
      ;; Shadow global comment-region-lisp binding.  [but this loads ilisp,
      ;; which is a bother . . .  -- rgr, 26-Jul-96.]  [replaced with my own
      ;; hack.  -- rgr, 7-Sep-99.]
      (define-key perl-mode-map [?\C-x ?\C-\;] 'rgr-comment-region-lisp)))

;;;###autoload
(defun rgr-perl-mode-hook ()
  ;; Take the whole enchilada.
  (rgr-perl-mode-fix-indentation)
  (rgr-perl-mode-install-documentation-hacks)
  (rgr-perl-mode-install-extra-hacks))

(provide 'rgr-perl-mode)

;; End of rgr-perl-hacks.el
