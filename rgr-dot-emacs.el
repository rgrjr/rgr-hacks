;; To set up rgr-hacks:
(setq rgr-site 'bootcamp)
(autoload 'tcl-mode "tcl" "Tcl mode." t)
(autoload 'inferior-tcl "tcl" "Run inferior Tcl process." t)
;; [seems to already be there, but just in case.  -- rgr, 28-Sep-99.]
(setq auto-mode-alist (append '(("\\.tcl$" . tcl-mode)) auto-mode-alist))

(load-file "/u/std13/emacs/emacs-init.el")

;; To make discus work:
(setq *discus-local-host-regexp* (regexp-quote (system-name)))
;; [fixed?  -- rgr, 9-Sep-99.]
'(setq ange-ftp-date-regexp
      (concat " \\(Jan\\|Feb\\|Mar\\|Apr\\|May\\|Jun\\|Jul\\|Aug\\|Sep\\|Oct"
              "\\|Nov\\|Dec\\) +[0-3]?[0-9] "))
