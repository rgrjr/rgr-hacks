
(defun rgr-ilisp-kill-definition ()
  (interactive)
  (let ((name (lisp-def-name)))
    (message "[got %S]" name)))
