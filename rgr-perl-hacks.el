;;;*****************************************************************************
;;;
;;;; Hacking perl mode.
;;;
;;; Copyright (C) 1996-2011  Robert G. Rogers Jr
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
;;;    You should be aware that documentation lookup relies on the
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
;;; $Id$

(eval-when-compile
  (require 'cl)
  (require 'man)
  (require 'perl-mode)
  (require 'cperl-mode))

;;;; variables.

(defvar rgr-perldoc-program "perldoc")
(defvar rgr-perldoc-args "")

(defvar rgr-perl-function-documentation-prefix "^       "
  "Regular expression that matches the precise indentation in front of
the function name in the perl documentation.  This appears to be
somewhat system-dependent; the default is only known to work with
GNU/Linux (openSUSE).")

(defvar rgr-manpage-done-p nil)
(defvar rgr-perl-manpage-alphabetical-listing nil
  "Start of alphabetical function listing.")

;;;; general stuff.

(defun rgr-perl-this-line-definition-name ()
  ;; Get the definition name on the current line, without moving point,
  ;; returning nil if we can't find one.
  (let ((perl-name "\\([$:a-zA-Z0-9_]+\\)"))
    (save-excursion
      (beginning-of-line)
      (cond ((looking-at (concat "^[ \t]*sub[ \t]+" perl-name))
	      (match-string-no-properties 1))
	    ((looking-at (concat "^[ \t]*\\(my[ \t]*\\)?" perl-name
				 "[ \t]*|*=[ \t]*sub[ \t]*{"))
	      ;; [note that the "my" will not be present if put in a separate
	      ;; statement so that the sub can recur.  -- rgr, 28-Nov-08.]
	      (match-string-no-properties 2))))))

(defun rgr-perl-def-name-tail (suffix)
  ;; Given any inner names in suffix (which may be nil), return it if there are
  ;; no more outer names, else concatenate the new outer name and recur.  Does
  ;; not preserve point.
  (condition-case err
      (let* ((outer-name (progn
			   (backward-up-list)
			   (rgr-perl-this-line-definition-name)))
	     (full-name (if (and outer-name suffix)
			    (concat outer-name " " suffix)
			    outer-name)))
	(cond ((null outer-name)
		(rgr-perl-def-name-tail suffix))
	      ((not (= (aref outer-name 0) ?$))
		full-name)
	      (t
		(rgr-perl-def-name-tail full-name))))
    (error
      ;; This happens when backward-up-list runs out of list.
      suffix)))

(defun rgr-perl-definition-name ()
  "Snarf the name of the Perl sub containing point.
If point is inside a named lexical (i.e. \"my $inner = sub { ... }\"),
then the returned name will be a series of names in the form

    \"outermost $inner $innermost\".

The first line of a sub is always considered inside it, even if point
is not inside the curly braces."
  (let ((inner-name (rgr-perl-this-line-definition-name)))
    (cond (inner-name
	    (beginning-of-line)
	    (rgr-perl-def-name-tail inner-name))
	  (t
	    (rgr-perl-def-name-tail nil)))))

(put 'perl-mode 'mode-definition-name 'rgr-perl-definition-name)
(put 'cperl-mode 'mode-definition-name 'rgr-perl-definition-name)

(defun rgr-perl-toggle-internal (string from to)
  "In Perl, this would be '$string =~ s/$from/$to/g;'."
  (save-match-data
    (while (string-match from string)
      (setq string (replace-match to t t string)))
    string))

;;;###autoload
(defun rgr-perl-toggle-module-syntax ()
  "Toggle a module name between the Unix filename and Perl syntax."
  (interactive)
  (save-excursion
    (skip-chars-backward "a-zA-Z0-9_:./")
    (skip-chars-forward ":./")
    (cond ((looking-at "\\([a-zA-Z0-9_]+/[a-zA-Z0-9_/]+\\)\\(\\.pm\\)?")
	    (replace-match
	      (rgr-perl-toggle-internal (match-string 1) "/" "::")
	      t t)
	    (skip-chars-backward "a-zA-Z0-9_:/")
	    (if (looking-at "/trunk::")
		;; Special hack for cleaning up "svn log" output.
		(replace-match "")))
	  ((looking-at "[a-zA-Z0-9_]+::[a-zA-Z0-9_:]+")
	    (replace-match
	      (concat (rgr-perl-toggle-internal (match-string 0) "::" "/")
		      ".pm")
	      t t))
	  (t
	    (error "Unrecognized module syntax before point.")))))

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
	  (with-current-buffer buffer
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
			"\\>"))
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
    (or (with-current-buffer man-buffer
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

;;; Hacking POD.

(defvar rgr-perl-footnote-regexp "\\[\\([0-9][0-9a-zA-Z]*\\)\\]"
  "For matching footnotes in Perl POD documentation.")

;;;###autoload
(defun rgr-perl-renumber-pod-footnotes ()
  "Renumber footnotes consecutively in POD.

A footnote is labelled with a '[#]' string, where '#' is a digit string
with an optional alphanumeric suffix.  The footnote is considered to be
defined where this string appears on a line starting with '=item'; each
footnote must have exactly one definition which should appear after all
references \[though the occurrence of later references is not checked\].

This command renumbers the footnotes consecutively from the first one,
leaving the value of the first one intact if it is numeric, else
renumbering from 1.  Definitions are renumbered to match, but no attempt
is made to reorder the definitions to correspond to the new numbering.
\[We should try to identify such cases and at least print a message.  --
rgr, 8-Nov-06.\]"
  (interactive)
  (let ((start (point))
	;; entries are of the form (orig-label new-label use-count def-p).
	(label-to-entry nil)
	(entries-to-renumber nil))
    ;; Go through the whole document looking for labels.  This must be done in
    ;; two nested loops, an outer loop to find the POD and an inner loop to find
    ;; the labels within the POD, so that we don't renumber constant subscripts.
    (goto-char (point-min))
    (let ((current-index nil))
      (while (re-search-forward "^=" nil t)
	(let ((end (save-excursion
		     (cond ((re-search-forward "^=cut" nil t)
			     (forward-line)
			     (point))))))
	  (while (re-search-forward rgr-perl-footnote-regexp end t)
	    (let* ((label (match-string-no-properties 1))
		   (entry (assoc label label-to-entry))
		   (definition-p (save-excursion
				   (beginning-of-line)
				   (looking-at "^=item"))))
	      (cond ((and (not entry) definition-p)
		      (message "Footnote %S is unused." label)
		      (sit-for 1))
		    ((not entry)
		      (setq current-index 
			    (cond (current-index (1+ current-index))
				  ((string-match "^[0-9]+$" label)
				    (string-to-number label))
				  (t 1)))
		      (setq entry
			    (list label (format "%d" current-index) 1 nil))
		      (setq label-to-entry (cons entry label-to-entry)))
		    (definition-p
		      (setcar (cdr (cdr (cdr entry))) t))
		    (t
		      ;; subsequent reference for a known label.
		      (setcar (cdr (cdr entry)) (1+ (nth 2 entry)))))))
	  (goto-char end)))
      '(message "[entries %S]" label-to-entry))
    ;; Check for footnotes that are referenced by not defined.  [Maybe we don't
    ;; want to renumber any in this case?  -- rgr, 8-Nov-06.]
    (let ((tail label-to-entry))
      (while tail
	(let* ((entry (car tail))
	       (old-label (car entry))
	       (new-label (nth 1 entry)))
	  (if (not (string-equal old-label new-label))
	      (setq entries-to-renumber (cons entry entries-to-renumber)))
	  (cond ((not (nth 3 entry))
		  (message "Footnote %s (renumbered to %s) is undefined."
			   old-label new-label)
		  (sit-for 2)))
	  (setq tail (cdr tail)))))
    ;; Now renumber.  This must also be done in two nested loops, and all in one
    ;; pass so that we don't get tripped up by overlapping renumberings.
    (cond ((null entries-to-renumber)
	    (message "All footnotes are numbered consecutively."))
	  (t
	    (goto-char (point-min))
	    (while (re-search-forward "^=" nil t)
	      (let ((end (save-excursion
			   (cond ((re-search-forward "^=cut" nil t)
				   (forward-line)
				   (point))))))
		(while (re-search-forward rgr-perl-footnote-regexp end t)
		  (let* ((label (match-string-no-properties 1))
			 (entry (assoc label entries-to-renumber))
			 (old-label (car entry))
			 (new-label (nth 1 entry)))
		    (cond ((and entry
				(not (string-equal old-label new-label)))
			    '(message "Changing %S to %S." old-label new-label)
			    (replace-match new-label t t nil 1)))))
		(goto-char end)))
	    (message "Renumbered %d out of %d footnotes."
		     (length entries-to-renumber) (length label-to-entry))))
    ;; Done.
    (goto-char start)))

;;;###autoload
(defun rgr-perl-renumber-region-pod-items (start end)
  "If the region contains a series of numbered \"=item\" lines, renumber them
sequentially, beginning with the number of the first.  Numbers must have a
trailing dot, and may consist of two or more dot-separated subcomponents,
which are incremented lexicographically."
  (interactive "r")
  (require 'rgr-enumerate)	;; for rgr-renumber-explode-dots
  (let ((last nil))
    (save-excursion
      (goto-char start)
      (while (re-search-forward "^=item[ \t]+\\(\\([0-9.]+\\.\\)?[0-9]+\\)\\."
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

;;; Building documentation templates for Perl classes.

(defun rgr-perl-find-quoted-names ()
  ;; Returns a list of strings, and moves point.
  (let ((words nil))
    (skip-chars-forward " \t\n")
    (while (looking-at "qw(")
      (goto-char (match-end 0))
      (skip-chars-forward " \t\n")
      (while (not (or (eobp) (looking-at ")")))
	(let ((word-start (point)))
	  (skip-chars-forward "^ \t\n(){}")
	  (setq words
		(cons (buffer-substring-no-properties word-start (point))
		      words)))
	(skip-chars-forward " \t\n"))
      (when (looking-at ")")
	(forward-char)
	(skip-chars-forward " \t\n")
	(when (looking-at ",")
	  (forward-char)
	  (skip-chars-forward " \t\n"))
	(while (looking-at "#")
	  (forward-line)
	  (skip-chars-forward " \t\n"))))
    words))

(defun rgr-perl-forward-doc-paragraphs ()
  ;; Do just that.
  (while (not (or (eobp) (looking-at "\n=\\(head\\|cut\\)")))
    (forward-paragraph)))

(defvar rgr-perl-sub-names-to-ignore
  '(home_page_url insert local_display_fields post_web_update
    pretty_name primary_key search_page_name table_audited_p
    table_name type_pretty_name web_search web_update)
  "List of symbols naming subs that do not need to be documented.
Presumably this is because they implement a well-known API
defined by a superclass.  (I'd really prefer a more elegant
solution that used inheritance, but I'm too lazy to write it now,
and too impatient to wait.  -- rgr, 1-Jul-13.]")

(defun rgr-adjoin (elt set)
  (if (member elt set) set (cons elt set)))

(defun rgr-union (set1 set2)
  ;; Compute the union of two sets of strings.
  (cond ((null set1) set2)
	((null set2) set1)
	(t
	  (rgr-union (cdr set1) (rgr-adjoin (car set1) set2)))))

(defun rgr-perl-update-method-documentation ()
  "In a Perl class, add '=head3' items for undocumented methods."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let* ((defined-names nil) (autoloaded-names nil)
	   (code-end (save-excursion
		       (or (re-search-forward "^1;$" nil t)
			   (re-search-forward "^__END__$" nil t))))
	   (doc-start (or code-end (point-min))))
      ;; First get accessor method names.
      (while (re-search-forward
	       "->build_\\(field\\|fetch\\|set\\)_\\|->define_class_\\(slots\\)"
	       code-end t)
	(let ((what (or (match-string-no-properties 2)
			(match-string-no-properties 1))))
	  (if (not (string= what "slots"))
	      (forward-sexp))
	  ;; (message "found %S at %d" what (point))
	  (cond ((string= what "field")
		  ;; build_field_accessors
		  (skip-chars-forward " \t\n([")
		  (setq defined-names
			(rgr-union (rgr-perl-find-quoted-names)
				   defined-names)))
		((string= what "fetch")
		  ;; build_fetch_accessor
		  (skip-chars-forward " \t\n")
		  (if (looking-at "(qw(\\([a-zA-Z0-9_]+\\)")
		      (setq defined-names
			    (rgr-adjoin (match-string-no-properties 1)
					defined-names))))
		((string= what "set")
		  ;; build_set_fetch_accessor
		  (skip-chars-forward " \t\n(")
		  (if (looking-at "'\\([a-zA-Z0-9_]+\\)'")
		      (setq defined-names
			    (rgr-adjoin (match-string-no-properties 1)
					defined-names))))
		((string= what "slots")
		  ;; define_class_slots
		  (skip-chars-forward " \t\n(")
		  (setq defined-names
			(rgr-union (rgr-perl-find-quoted-names)
				   defined-names))))))
      ;; Now get non-internal sub names.
      (goto-char (point-min))
      (while (re-search-forward "^sub +\\([a-zA-Z][a-zA-Z0-9_]*\\)"
				code-end t)
	(let* ((name (match-string-no-properties 1))
	       (symbol (intern name)))
	  (if (not (member symbol rgr-perl-sub-names-to-ignore))
	      (setq defined-names (rgr-adjoin name defined-names)))))
      ;; And autoloaded sub names.
      (goto-char (point-min))
      (while (re-search-forward "^ *sub +\\([a-zA-Z][a-zA-Z0-9_]*\\);"
				code-end t)
	(let ((name (match-string-no-properties 1)))
	  (setq autoloaded-names (rgr-adjoin name autoloaded-names))
	  (setq defined-names (rgr-adjoin name defined-names))))
      ;; (message "got %S" defined-names)
      (setq defined-names (sort defined-names #'string-lessp))
      ;; Look for undefined ones.
      (goto-char doc-start)
      (or (re-search-forward "^=head2 \\(Method\\|Accessor\\)" nil t)
	  (error "No 'Accessors and methods' section below %S." doc-start))
      (forward-line)
      (rgr-perl-forward-doc-paragraphs)
      (let ((tail defined-names)
	    (n-updated 0))
	(while (and tail
		    (not (eobp)))
	  (let* ((name (car tail))
		 (name-regexp (format "\n=head[0-9] %s\n" name)))
	    (cond ((looking-at name-regexp)
		    (forward-paragraph 1)	;; skip the =head3 line.
		    (rgr-perl-forward-doc-paragraphs))
		  (t
		    (while (and (looking-at "\n=head[0-9] \\(.*\\)\n")
				(string-lessp (match-string 1) name))
		      (or (member name rgr-perl-sub-names-to-ignore)
			  (message "%s is no longer implemented, %s"
				   (match-string 1) "or out of sequence"))
		      (sit-for 1)
		      (forward-paragraph)
		      (rgr-perl-forward-doc-paragraphs))
		    (when (save-excursion
			    (re-search-forward name-regexp nil t))
		      (message "Documentation for %s is out of sequence."
			       name)
		      (sit-for 1))
		    (insert "\n=head3 " name "\n")
		    (if (member name autoloaded-names)
			(insert "\nAutoloaded.\n"))
		    (setq n-updated (1+ n-updated)))))
	  (rgr-perl-forward-doc-paragraphs)
	  (setq tail (cdr tail)))
	(message "%d updated." n-updated)))))

(defun rgr-perl-update-documentation ()
  (interactive)
  (goto-char (point-min))
  (cond ((re-search-forward "^__END__" nil t)
	  (forward-paragraph 2))
	(t
	  (goto-char (point-max))
	  (insert "\n__END__\n\n")
	  (let ((module-name (save-excursion
			       (goto-char (point-min))
			       (and (re-search-forward
				      "^package \\([A-Aa-z_:]+\\);" nil t)
				    (match-string-no-properties 1)))))
	    (insert "=head1 " (or module-name "Module") "\n\n"))
	  (insert "=head2 Accessors and methods\n\n")
	  (if (save-excursion (re-search-backward "^ +sub [^{}]+;" nil t))
	      (insert "=head2 Autoloaded methods\n\n"))
	  (insert "=cut\n")))
  (rgr-perl-update-method-documentation))

;;; Adding CGI::Carp to Web scripts.

(defun rgr-perl-carpify ()
  "Add a \"use CGI::Carp\" line to the current Perl buffer.
This comes in a paragraph of its own after \"use warnings\"."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (or (re-search-forward "^use warnings;" nil t)
	(error "Buffer has no 'use warnings' line."))
    (skip-chars-forward " \t\n")
    (if (looking-at "use CGI::Carp")
	(error "Already using 'CGI::Carp' here."))
    (insert "use CGI::Carp qw(fatalsToBrowser);\n\n")
    (message "Done.")))

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

(defun rgr-perl-newline-and-maybe-indent (arg)
  "Insert a newline and indent if not within POD."
  (interactive "p")
  (newline arg)
  (if (save-excursion
	(or (not (re-search-backward "^=" nil t))
	    (looking-at "^=cut")))
      (perl-indent-command)))

;;; Scope hax.

(defun perl-id-char-p (char)
  (or (and (<= ?a char) (<= char ?z))
      (and (<= ?A char) (<= char ?Z))
      (and (<= ?0 char) (<= char ?9))
      (= char ?_)))

(defun rgr-perl-show-scope-free-references ()
  (interactive)
  (let ((nest 0) (ids nil))
    (save-excursion
      (while (and (not (eobp)) (>= nest 0))
	(skip-chars-forward " \t\n")
	(let ((char (char-after)))
	  (cond ((null char))
		((= char ?#)
		  (forward-line))
		((member char '(?\( ?\{))
		  (setq nest (1+ nest))
		  (forward-char))
		((member char '(?\) ?\}))
		  (setq nest (1- nest))
		  (forward-char))
		((member char '(?\' ?\"))
		  (forward-char)
		  (skip-chars-forward (string ?^ char))
		  (forward-char))
		((member char '(?$ ?% ?& ?@))
		  (let* ((name (buffer-substring
				 (progn (forward-char) (point))
				 (progn (skip-chars-forward "a-zA-Z0-9_")
					(point))))
			 (sigil (cond ((not (= char ?$)) char)
				      ((= (char-after) ?\{) ?%)
				      ((= (char-after) ?\[) ?@)
				      (t char)))
			 (id (concat (string sigil) name)))
		    ; name is "" after "&" or "&&" operators.
		    (if (and (length name) (not (member id ids)))
			(setq ids (cons id ids)))))
		((member char '(?= ?> ?- ?, ?\; ?: ?!))
		  ; These are non-problematic.
		  (forward-char))
		((perl-id-char-p char)
		  ; Bareword (but need to look for q, qq, m, s, ...).
		  (skip-chars-forward "a-zA-Z0-9_"))
		(t
		  (message "have char %c" char)
		  (forward-char)))))
      (message "End of scope, found %d ids." (length ids))
      (with-output-to-temp-buffer "*Perl IDs*"
	(let ((tail (sort ids (function string-lessp))))
	  (while tail
	    (princ (car tail))
	    (princ "\n")
	    (setq tail (cdr tail)))))
      (sit-for 1))
    ))

;; (setq debug-on-error t)

;;; Installing these commands.

;;;###autoload
(defun rgr-perl-mode-install-documentation-hacks (map)
  ;; perl-mode-hook function that installs commands to get function arguments
  ;; and documentation from the man page.  See also the rgr-perldoc command.  --
  ;; rgr, 26-Jul-96.
  (define-key map "\C-c\C-a" 'rgr-perl-quick-arglist)
  (define-key map "\C-c\C-d" 'rgr-perl-quick-documentation))

;;;###autoload
(defun rgr-perl-mode-fix-indentation ()
  ;; Swap newline and return to get indentation by default, with a hack that
  ;; won't indent within pod.
  (define-key perl-mode-map "\r" 'rgr-perl-newline-and-maybe-indent)
  (define-key perl-mode-map "\n" 'newline)
  ;; Don't do indenting on ";" -- ${$foo} loses.  -- rgr, 16-May-97.
  (define-key perl-mode-map ";" 'self-insert-command)
  ;; Indent comments, even at BOL (by dropping ";?#\\|" from the start of this
  ;; regular expression).  -- rgr, 4-Aug-97.
  (setq perl-nochange "\f\\|\\s(\\|\\(\\w\\|\\s_\\)+:"))

;;;###autoload
(defun rgr-perl-mode-install-extra-hacks (map)
  ;; These all require code defined in other rgr-*-hacks.el files.
  ;; Try to avoid shifting.  -- rgr, 20-Dec-96.
  (define-key map "-" 'rgr-c-electric-dash)
  ;; Learn subroutine names.  -- rgr, 14-Dec-98.  [insist on an open curly so
  ;; that we don't pick up forward decls.  -- rgr, 22-Apr-05.]
  (make-local-variable 'rgr-definition-line-regexp)
  (setq rgr-definition-line-regexp "^ *sub +.*{")
  (rgr-relearn-buffer-definition-names)
  ;; Standard modification history.
  (define-key map "\M-q" 'rgr-fill-script-comment)
  ;; Put in interpreter magic.  -- rgr, 29-Apr-97.  [but not in library modules.
  ;; -- rgr, 16-May-97.]  [/usr/bin/perl is more standard, so prefer that.  --
  ;; rgr, 22-Oct-02.]
  (cond ((not (string-match "\.pr?l$" (buffer-file-name))))
	((rgr-emacs-version-p 23)
	  ;; Not needed in perl-mode in Emacs 23.x.
	  )
	((file-executable-p "/usr/bin/perl")
	  (require 'executable)
	  (executable-set-magic "/usr/bin/perl -w"))
	((file-executable-p "/usr/local/bin/perl")
	  (require 'executable)
	  (executable-set-magic "/usr/local/bin/perl -w"))))

;;;###autoload
(defun rgr-perl-mode-hook ()
  ;; First, this helps deal with stupid editors (including perl-mode, alas) that
  ;; leave whitespace as indentation on otherwise empty lines.
  (setq paragraph-start "[ \t]*$\\|^")
  (setq paragraph-separate paragraph-start)
  ;; Bind rgr-perl-carpify.
  (define-key perl-mode-map "\C-c%" 'rgr-perl-carpify)
  ;; Take the whole enchilada.
  (rgr-perl-mode-fix-indentation)
  (rgr-perl-mode-install-documentation-hacks perl-mode-map)
  (rgr-perl-mode-install-extra-hacks perl-mode-map))

;;;###autoload
(defun rgr-cperl-mode-hook ()
  (define-key cperl-mode-map "\r" 'cperl-linefeed)
  (define-key cperl-mode-map "\n" 'newline)
  (setq cperl-indent-level 4)
  (setq cperl-continued-statement-offset 4)
  (setq cperl-close-paren-offset 0)
  ;; Bind rgr-perl-carpify.
  (define-key cperl-mode-map "\C-c%" 'rgr-perl-carpify)
  (rgr-perl-mode-install-documentation-hacks cperl-mode-map)
  (rgr-perl-mode-install-extra-hacks cperl-mode-map))

(provide 'rgr-perl-mode)

;; End of rgr-perl-hacks.el
