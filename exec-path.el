
(defun rgr-show-executables (command)
  "Show the full pathname for each version of COMMAND that exists in $PATH."
  (interactive "sCommand name: ")
  (with-output-to-temp-buffer (concat "*" command " versions*")
    (let ((tail exec-path))
      (while tail
	(let ((path (expand-file-name command (car tail))))
	  (cond ((and (file-executable-p path)
		      (not (file-directory-p path)))
		 (princ path)
		 (terpri))))
	(setq tail (cdr tail))))))
