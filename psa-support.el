;;;*****************************************************************************
;;;
;;;    Random psa support hacks.
;;;
;;;    Modification history:
;;;
;;; created.  -- rgr, 14-Nov-95.
;;; added server-mail-traffic hack.  -- rgr, 15-Nov-95.
;;; psa-retry: new hack.  -- rgr, 1-Dec-95.
;;; psa-fix-empty-sequence-bug: helper for psa-retry.  -- rgr, 17-Mar-97.
;;; add autoloads.  -- rgr, 18-Dec-98.
;;; psa-update-lost-requests-page: new hack.  -- rgr, 18-Dec-98.
;;; psa-update-lost-requests-page: add rgr-find-url hack.  -- rgr, 21-Dec-98.
;;; psa-update-lost-requests-page: allow non-window use.  -- rgr, 15-Jan-99.
;;; psa-update-lost-requests-page: update only if new data, change directory in
;;;	shell if needed.  -- rgr, 10-Feb-99.
;;;

(eval-when-compile
  (require 'rmail))

(defun psa-require-psa-server ()
  ;; Done at run time so that we don't need psa-request available for things
  ;; that don't have it.
  (let ((load-path (cons (expand-file-name "~psa/psa-test/bin") load-path)))
    (require 'psa-server)))

(defun server-mail-traffic (days hours requests runs)
  ;; in messages per month
  (* (/ (+ requests (* 4 runs))
	(+ days (/ hours 24.0)))
     30))

;; (server-mail-traffic 17 21 251 113)
;; (server-mail-traffic 18 22 262 116)
;; (server-mail-traffic 0 21 4 2)
;; (server-mail-traffic 4 23 53 25)
;; (server-mail-traffic 5 17 66 30)
;; (server-mail-traffic 3 10 79 41)

;;;###autoload
(defun rgr-delete-non-sequence-characters ()
  "Hack to convert the rest of the file to something resembling a
protein sequence."
  (interactive)
  (replace-regexp "[^ \t\nacdefghiklmnpqrstvwy]+" ""))

(defconst psa-retry-flame
    "   [The server rejected this because it was being too picky when parsing
the \"analysis-assumptions\" line.  I have fixed this problem, and am
resubmitting your request.  We apologize for the inconvenience.

					-- Bob Rogers
					   rogers@darwin.bu.edu
					   BMERC, 617-353-7123]
")

(defconst psa-empty-sequence-retry-flame
    "   [This is the format the mail server is expecting; a bug in the Web
interface allowed you to put the sequence in the comments field and
leave the sequence field blank.  We apologize for the inconvenience, and
hope you find our server useful.

					-- Bob Rogers
					   rogers@darwin.bu.edu
					   BMERC, 617-353-7123]
")

;;;###autoload
(defun psa-fix-empty-sequence-bug ()
  "The psa-empty-sequence-retry-flame says it all."
  (interactive) ;; for debugging
  (let* ((sequence-start (point))
	 (sequence-lines nil)
	 (psa-amino-acid-chars "A-Z") ;; "ACDEFGHIKLMNPQRSTVWY"
	 (aa-line-regexp
	   (concat "^;+[ \t]*\\([ \t"
		   psa-amino-acid-chars
		   (downcase psa-amino-acid-chars)
		   "]+\n\\)")))
    (message "sequence-start is %d, regexp is %s" sequence-start aa-line-regexp)
    (sit-for 2)
    (while (re-search-forward aa-line-regexp nil t)
      (setq sequence-lines (cons (buffer-substring (match-beginning 1)
						   (match-end 1))
				 sequence-lines))
      (replace-match ""))
    (goto-char sequence-start)
    (if (not (re-search-forward "^1$" nil t))
	(error "bug in psa-fix-empty-sequence-bug; can't find '1' line."))
    '(save-excursion
      (forward-line -2)
      (insert ";\n"))
    (beginning-of-line)
    (apply (function insert) (reverse sequence-lines))
    (delete-char -1))
  (goto-char (point-max))
  (insert psa-empty-sequence-retry-flame))

;;;###autoload
(defun psa-retry ()
  "In rmail, tries to redirect a mis-sent message to psa-request.
Heuristicates the message body, and leaves you in mail mode, as via
rmail-reply."
  (interactive)
  (psa-require-psa-server)
  (cond ((eq major-mode 'rmail-mode)
	 (rmail-reply t))
	((eq major-mode 'rmail-summary-mode)
	 (rmail-summary-reply t))
	(t
	 (error "Must be in rmail for this to work.")))
  (let ((*psa-bindings* nil) (request-id nil)
	(text-start nil) (text-end nil))
    ;; Get the headers and "interesting" part of the body.
    (save-excursion
      (set-buffer mail-reply-buffer)
      ;; from psa-parse-message
      (psa-parse-headers)
      ;; [this now initializes the task; use psa-parse-from-and-to-fields
      ;; instead.  -- rgr, 29-Sep-95.]
      ;; (psa-parse-address-fields)
      (psa-parse-from-and-to-fields)
      ;; find start of interesting text.  If we can't find the sender's original
      ;; message (perhaps because it's not a psa-request server error message),
      ;; then take the entire body and let the user sort it out later.  If the
      ;; sender missent to psa@darwin, then that's the right thing anyway.
      (if (search-forward "--- Original message ---" nil t)
	  (forward-line))
      ;; Go past the headers.
      (re-search-forward "^[ \t]*$" nil t)
      (setq text-start (point))
      (setq text-end (point-max)))
    (let ((to (or (psa-value 'to) ""))
	  (from (psa-value 'address)))
      (if (or (equal from "psa-request@darwin.bu.edu")
	      (equal from "mailer-daemon@darwin.bu.edu")
	      (equal from "psa@darwin.bu.edu"))
	  ;; Resubmitting from a server error response (rather than forwarding a
	  ;; message missent by the original user).
	  (setq from to))
      ;; Fixup reply headers.
      (goto-char (point-min))
      (replace-regexp "^To: " "To: psa-request@darwin.bu.edu\nCC: ")
      (cond ((eq from to)
	      (kill-line)
	      (insert from)))
      (forward-line)
      (insert "Reply-To: " from "\n")
      (search-forward "Subject: ")
      (if (not (eolp))
	  ;; delete original rmail-reply subject
	  (kill-line))
      (insert "forwarded PSA request")
      (if (psa-value 'subject)
	  (insert " [" (psa-value 'subject) "]"))
      ;; Insert the request part of the message.
      (goto-char (point-max))
      (save-excursion
	(insert-buffer-substring mail-reply-buffer text-start text-end)
	(cond ((re-search-backward "^--.*--$")
		;; MIME content boundary
		(delete-region (match-beginning 0) (1+ (match-end 0))))))
      ;; Heuristically determine the nature of the problem.
      (if (save-excursion
	    (re-search-forward "^1$" nil t))
	  (psa-fix-empty-sequence-bug)
	  (insert psa-retry-flame))
      ;; Debugging 
      '(psa-insert-bindings)
      (goto-char (point-min)))))

;;;; Automatically updating the lost.htm page.

;;; Must be run on huxley (or other alpha) in order to access the
;;; /usr/local/etc/httpd/htdocs/psa-new/ directory.

(defun psa-comint-command (input)
  (let ((proc (get-buffer-process (current-buffer))))
    (insert-before-markers input "\n")
    (funcall comint-input-sender proc input)
    (comint-output-filter proc "")))

;;;###autoload
(defun psa-update-lost-requests-page ()
  "Insert ~psa/lost.tbl at the end of the 'official' lost.tbl, remake
the lost.htm page, install it at 'http://bmerc-www.bu.edu/psa/lost.htm',
and bring this page up in a browser."
  (interactive)
  (psa-require-psa-server)
  (let* ((web-dir "/usr/local/etc/httpd/htdocs/psa-new/")
	 (web-lost-database (expand-file-name "lost.tbl" web-dir))
	 (psa-lost-database (expand-file-name "~psa/lost.tbl")))
    (switch-to-buffer (find-file-noselect web-lost-database))
    (cond ((and (file-readable-p psa-lost-database)
		(file-newer-than-file-p psa-lost-database web-lost-database))
	    ;; If we're not on an Alpha, we should die immediately.
	    (if (= (point-min) (point-max))
		(error "Can't update lost.tbl; wrong system?"))
	    (goto-char (point-max))
	    (insert-file psa-lost-database))
	  (t
	    (message "No new transactions from %s." psa-lost-database)))
    ;; allow for manual edits, as well as the automatic update above.
    (if (buffer-modified-p)
	(save-buffer))
    ;; get a new window
    (switch-to-buffer-other-window (current-buffer))
    (shell)
    ;; Ensure we are in the right directory, and install the updated web page.
    (cond ((not (equal list-buffers-directory web-dir))
	    (psa-comint-command (concat "cd " web-dir))
	    (shell-cd web-dir)))
    (psa-comint-command "make install-dir=../psa install-lost")
    ;; back to lost.tbl in the first window.
    (other-window 1)
    (cond (window-system
	    ;; bring up in a browser window.
	    (require 'rgr-mouse)
	    (rgr-find-url "http://bmerc-www.bu.edu/psa/lost.htm")
	    (message "Done.  (You may need to click 'Reload'.)"))
	  (t
	    (message "Done.")))))

(provide 'psa-support)

