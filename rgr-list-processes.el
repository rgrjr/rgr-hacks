;;;****************************************************************************
;;;
;;; Turn buffer names into buttons in "*Process List*".
;;;
;;; This is an a file of its own because it requires Emacs 24.0.
;;;
;;; [created.  -- rgr, 27-Nov-10.]
;;;
;;; $Id:$

(defun rgr-list-processes-hook ()
  (let ((buffer-read-only nil))
    (goto-char (point-min))
    (forward-line 2)
    ;; The buffer name is the third field
    (let* ((word "[^ \t\n]+") (ws "[ \t\n]+")
	   (match-word-three (concat word ws word ws "\\(" word "\\)")))
      (while (not (eobp))
	(when (looking-at match-word-three)
	  (let* ((buffer-name (match-string 1))
		 (buffer (get-buffer buffer-name)))
	    (when buffer
	      (make-button (match-beginning 1) (match-end 1)
			   'action 'rgr-button-select-buffer
			   'buffer-name buffer-name
			   'help-echo "mouse-2, RET: Select this buffer"))))
	(forward-line)))))

(defun rgr-button-select-buffer (button)
  ;; Select the buffer named by the buffer-name property of the button.
  (let* ((buffer-name (button-get button 'buffer-name))
	 (buffer (and buffer-name (get-buffer buffer-name))))
    (when buffer
      (switch-to-buffer buffer))))

(add-hook 'after-list-processes-hook 'rgr-list-processes-hook)
;; (run-hooks 'after-list-processes-hook)

(provide 'rgr-list-processes)
