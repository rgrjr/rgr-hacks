;;; Hacks for finding protein sequences in GenBank.
;;;
;;; [created.  -- rgr, 16-Sep-03.]
;;;
;;; $Id$

(defvar rgr-genbank-base-url
        ;; "http://www.ncbi.nlm.nih.gov/entrez/query.fcgi?CMD=search&DB=protein"
	(concat "http://www.ncbi.nlm.nih.gov/entrez/query.fcgi?"
		"SUBMIT=y&db=protein&cmd=Search&term="))
(defvar rgr-genbank-locus-regexp
        "\\(dbj\\|sp\\|gb\\|ref\\|emb\\|pir\\)|+\\([-_a-zA-Z0-9.]+\\)")

;;;###autoload
(defun rgr-genbank-find-locus (locus)
  "Prompts for a GenBank locus, and looks it up via the browse-url function."
  (interactive "sGenBank locus: ")
  (message "Finding %s" locus)
  (browse-url (concat rgr-genbank-base-url locus)))

;;;###autoload
(defun rgr-genbank-find-next-locus ()
  "Searches for the next GenBank locus, and looks it up via
\\[rgr-genbank-find-locus].  This is optimized for use in blast output
transcripts."
  (interactive)
  (or (re-search-forward rgr-genbank-locus-regexp nil t)
      (error "No more loci."))
  (rgr-genbank-find-locus (match-string 2)))

(global-set-key "\C-cg" 'rgr-genbank-find-next-locus)

(provide 'rgr-genbank)
