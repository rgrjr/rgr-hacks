;;;*****************************************************************************
;;;
;;;; Hacking Lua mode.
;;;
;;; Copyright (C) 2008  Robert G. Rogers Jr
;;;
;;; $Id:$

(eval-when-compile
  (require 'lua-mode))

(defun rgr-fill-lua-comment (arg)
  "Fill the Lua comment around point.
Just does the regular M-q (fill-paragraph) if it can't find a comment."
  (interactive "P")
  (or (rgr-fill-prefix-comment "-- ")
      (fill-paragraph arg)))

;;;###autoload
(defun rgr-lua-mode-hook ()
  (define-key lua-mode-map "\r" 'newline-and-indent)
  (define-key lua-mode-map "\n" 'newline)
  ;; Try to avoid shifting.  -- rgr, 20-Dec-96.
  (define-key lua-mode-map "-" 'rgr-c-electric-dash)
  ;; Learn subroutine names.  -- rgr, 14-Dec-98.  [insist on an open curly so
  ;; that we don't pick up forward decls.  -- rgr, 22-Apr-05.]
  (make-local-variable 'rgr-definition-line-regexp)
  (setq rgr-definition-line-regexp "^ *function +")
  (rgr-relearn-buffer-definition-names)
  (define-key lua-mode-map "\M-q" 'rgr-fill-lua-comment))
