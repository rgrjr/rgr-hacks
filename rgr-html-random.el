;; Ephemeral hacks.  -- rgr, 26-Mar-98.

(defun rgr-html-fix-dl ()
  (interactive)
  (let* ((whitespace "[ \t\n]*")
	 (ul-to-dl (concat "</ul>" whitespace "<dl>" whitespace
			   "<dt>" whitespace "<dd>"))
	 (dl-to-ul (concat "</dl>" whitespace "<ul>"))
	 (either-transition (concat ul-to-dl "\\|" dl-to-ul)))
    (goto-char (point-min))
    (while (re-search-forward either-transition nil t)
      (replace-match "\n<p>"))
    (goto-char (point-min))
    (if (re-search-forward (concat whitespace "</dl>") nil t)
	(replace-match "\n</ul>"))))

;;; Navigation bars for <tt>needle</tt> tools.  -- rgr, 26-Nov-99.

(defvar rgr-html-nt-nav-bar
	"<p><a href=\"http://bmerc-www.bu.edu/\">BMERC</a> : <a
href=\"\"><tt>needle</tt> tools</a> : <a
href=\"file-formats.html\">File formats</a>")

(defun rgr-html-insert-nav-bar ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((title (if (re-search-forward "<title>\\(.*\\)</title>" nil t)
		     (match-string 1)
		     "? programs")))
      (cond ((re-search-forward "<body>" nil t)
	      (replace-match "<body bgcolor=white>"))
	    ((re-search-forward "bgcolor=\"#ffffff\"" nil t)
	      (replace-match "bgcolor=white")))
      (or (re-search-forward "^<h2>" nil t)
	  (error "Can't find <h2> tag."))
      (goto-char (match-beginning 0))
      (rgr-html-forward-markup)
      (skip-chars-forward " \t\n")
      (insert rgr-html-nt-nav-bar " : " title "\n<hr>\n\n<p>"))))

