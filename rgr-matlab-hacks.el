;;;; Stuff that doesn't belong in the matlab-hacks.el file.
;;;
;;;    Modification history:
;;;
;;; created.  -- rgr, 25-Jul-00.
;;; rgr-matlab-compare-transcripts: new.  -- rgr, 27-Feb-01.
;;; "-nodesktop" on Solaris.  -- rgr, 19-Mar-01.
;;;

(require 'matlab)

(if (string-match "solaris" system-configuration)
    ;; This is necessary for Matlab 6.0, which wants to run its own silly GUI
    ;; unless you tell it not to.  -- rgr, 19-Mar-01.
    (setq matlab-shell-command-switches "-nodesktop"))

;;;###autoload
(defun rgr-matlab-compare-transcripts ()
  (interactive)
  (let* ((analysis-type "pdb")
	 (output-file (concat "gpsa4a." analysis-type ".trans.new"))
	 (orig-file (concat "gpsa4a." analysis-type ".trans.orig"))
	 (result-buffer "*diff matlab*"))
    (save-excursion
      (let ((end (point)))
	(or (re-search-backward "^>> gpsa" nil t)
	    (error "Can't find a gpsa4a invocation."))
	(write-region (point) end output-file)))
    ;; [based on the discus-quick-source-compare fn.  -- rgr, 27-Feb-01.]
    (with-output-to-temp-buffer result-buffer
      (call-process "diff" nil result-buffer t
		    "-c" orig-file output-file))))

;;;###autoload
(defun rgr-matlab-mode-hook ()
  ;; Try to avoid shifting.  -- rgr, 25-Jul-00.
  (define-key matlab-mode-map "-" 'rgr-c-electric-dash))

;;;###autoload
(defun rgr-matlab-shell-mode-hook ()
  ;; Try to avoid shifting.  -- rgr, 25-Jul-00.
  (define-key matlab-shell-mode-map "-" 'rgr-c-electric-dash)
  ;; Further matlab-mode-install-path bashing.
  (setq matlab-mode-install-path '("/home2/staff/stultz/modscale/"
				   "/home2/staff/stultz/matlab/"
				   "/usr/local/matlab/toolbox/")))

;; (add-hook 'matlab-mode-hook 'rgr-matlab-mode-hook)
;; (add-hook 'matlab-shell-mode-hook 'rgr-matlab-shell-mode-hook)
;; (setq matlab-shell-mode-hook (list (car (cdr matlab-shell-mode-hook)) (car matlab-shell-mode-hook)))
