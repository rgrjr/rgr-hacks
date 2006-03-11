;;;; Patch to unquote '%' escapes in local URLs.
;;;
;;; [created.  -- rgr, 9-Aug-03.]
;;;
;;; $Id$

(require 'ffap)

(defun ffap-url-encode-hex (string)
  ;; Convert a hex string to an integer.  Breaks if given non-hex chars.
  (let ((hex-digit-alist '((?0 .  0)  (?1 .  1)  (?2 .  2)  (?3 .  3)
			   (?4 .  4)  (?5 .  5)  (?6 .  6)  (?7 .  7)
			   (?8 .  8)  (?9 .  9)  (?A . 10)  (?B . 11)
			   (?C . 12)  (?D . 13)  (?E . 14)  (?F . 15)))
	(result 0) (i 0))
    (while (< i (length string))
      (setq result (+ (* result 16)
		      (cdr (assq (aref string i) hex-digit-alist))))
      (setq i (1+ i)))
    result))

(defun ffap-url-decode-string (string)
  ;; [based on the vm-url-decode-buffer fn, which is GPL'ed with the following
  ;; notice: "Copyright (C) 1989-1997 Kyle E. Jones".  -- rgr, 9-Aug-03.]
  (let ((pos 0))
    (while (string-match "%[0-9a-fA-F][0-9a-fA-F]\\|\\+" string pos)
      (setq pos (1+ (match-beginning 0)))
      (setq string (replace-match
		     (if (= pos (match-end 0))
			 " "
			 (string (ffap-url-encode-hex
				   (substring string pos (match-end 0)))))
		     t t string)))
    string))

;; (equal (ffap-url-decode-string "foo%20bar+baz%25EFquux%23") "foo bar baz%EFquux#")

;; [was originally a defsubst.  patched to wrap ffap-url-decode-string around
;; the result.  -- rgr, 9-Aug-03.]
(defun ffap-url-unwrap-local (url)
  "Return URL as a local file, or nil.  Ignores `ffap-url-regexp'."
  (and (string-match "\\`\\(file\\|ftp\\):/?\\([^/]\\|\\'\\)" url)
       (ffap-url-decode-string (substring url (1+ (match-end 1))))))

;; [this must be part of the patch because of the foregoing defsubst; it has not
;; actually changed.  -- rgr, 9-Aug-03.]
(defun ffap-fixup-url (url)
  "Clean up URL and return it, maybe as a file name."
  (cond
   ((not (stringp url)) nil)
   ((and ffap-url-unwrap-local (ffap-url-unwrap-local url)))
   ((and ffap-url-unwrap-remote ffap-ftp-regexp
	 (ffap-url-unwrap-remote url)))
   ((fboundp 'url-normalize-url)	; may autoload url (part of w3)
    (url-normalize-url url))
   (url)))

(provide 'ffap-local-url-patch)

