;;;****************************************************************************
;;;
;;;   GNU Emacs init file.
;;;
;;;    Modification history:
;;;
;;; started mod hist.  -- rgr, 8-Apr-94.
;;; . . .
;;; modify-frame-parameters mouse & label hack.  -- rgr, 8-Nov-95.
;;; disable ffap under X11.  -- rgr, 15-Nov-95.
;;; install comment-region-lisp on C-x C-;.  -- rgr, 27-Nov-95.
;;; claris-works vt102 hack.  -- rgr, 8-Dec-95.
;;; general cleanup.  -- rgr, 11-Dec-95.
;;; html-helper-mode hacking.  -- rgr, 25-Jan-96.
;;; remove ctserv-request, html-helper-mode to default.el.  -- rgr, 30-Jan-96.
;;; rgr-html-helper-mode-hook: new.  -- rgr, 31-Jan-96.
;;; some hook cleanup, lisp indentation for emacs.  -- rgr, 8-Feb-96.
;;; fill-patch and sentence-end hacks.  -- rgr, 11-Feb-96.
;;; rgr-html-fill-paragraph.  -- rgr, 22-Feb-96.
;;; move autoloads to separate ./rgr-autoloads.el file.  -- rgr, 4-Mar-96.
;;; rgr-web-client-host init.  -- rgr, 14-Mar-96.
;;; Discus 5.0 and site-start.el related cleanups.  -- rgr, 19-Mar-96.
;;; 19.30 byte-compile-dynamic-docstrings, nuke fill-patch.  -- rgr, 21-Mar-96.
;;; fix frame label to use "official" functions.
;;; moved lots of stuff that wasn't add-hook or require to ./rgr-hacks.el file.
;;;	-- rgr, 29-Mar-96.
;;; add perl-mode-hook.  -- rgr, 13-Jun-96.
;;; add rgr-command-history-hook.  -- rgr, 12-Aug-96.
;;; (add-hook 'sh-mode-hook 'rgr-sh-mode-hook).  -- rgr, 16-Aug-96.
;;; compilation-mode-hook.  -- rgr, 27-Nov-96.
;;; installed abbrev-completion feature.  -- rgr, 29-Nov-96.
;;; makefile-mode-hook, add abb-comp "su" kludge, save file.  -- rgr, 2-Dec-96.
;;; rgr-install-weekly-completion-cycle feature.  -- rgr, 10-Dec-96.
;;; clean up change history.  -- rgr, 17-Apr-97.
;;; fix ffap version; emacs has reverted to 19.28.  -- rgr, 19-Jul-97.
;;; (rgr-install-hacks) simplification.  -- rgr, 16-Sep-97.
;;; set dired-listing-switches.  -- rgr, 19-Nov-97.
;;; rgr-maybe-rename-buffer find-file-hooks entry.  -- rgr, 17-Dec-97.
;;; ffap-other-window, version cleanup.  -- rgr, 15-Jan-98.
;;; bury-buffer on C-c b (easy to typo?), no find-file-lisp.  -- rgr, 6-Feb-98.
;;; setq rgr-web-client-host "mcclintock".  -- rgr, 5-Mar-98.
;;; rgr-thread-after-save-hook: new.  -- rgr, 10-Mar-98.
;;; rgr-web-client-host from "mcclintock" to "huxley".  -- rgr, 27-Apr-98.
;;; rgr-abbrev-hash-name hack.  -- rgr, 5-Jun-98.
;;; more rgr-abbrev-hash-name hacks.  -- rgr, 14-Oct-98.
;;; better rgr-web-client-host init, flush dupl. ~/emacs.  -- rgr, 19-Oct-98.
;;; fix dired-listing-switches for solaris.  -- rgr, 27-Oct-98.
;;; move stuff into site-start.el, hacks files.  -- rgr, 28-Oct-98.
;;; set rgr-web-client-host to sewall.  -- rgr, 4-Nov-98.
;;; rgr-dired-rename-file-and-versions hack.  -- rgr, 2-Feb-99.
;;; (require 'bmerc-efs), misc. cleanup.  -- rgr, 13-Feb-99.
;;; netscape changed locations again.  -- rgr, 3-Mar-99.
;;; completion cleanup.  -- rgr, 10-Mar-99.
;;; rgr-thread-after-save-hook: hack autodsm group as well.  -- rgr, 22-Mar-99.
;;; telnet-mode-hook, comint-mode-hook.  -- rgr, 24-Mar-99.
;;; matlab-shell-mode-hook.  -- rgr, 25-Mar-99.
;;; kludge rgr-web-client-name for acroread problem.  -- rgr, 29-Mar-99.
;;; undo same.  -- rgr, 30-Mar-99.
;;; move rgr-web-client-host again.  -- rgr, 1-Jul-99.
;;; add rgr-html-fix-regexps hook.  -- rgr, 26-Jul-99.
;;; fix html-mode-hook -- also run by html-helper-mode.  -- rgr, 13-Aug-99.
;;; matlab-mode-hook.  -- rgr, 21-Sep-99.
;;; moved matlab-mode-hook stuff to site-start.el file.  -- rgr, 21-Sep-99.
;;; sql-mode setup.  -- rgr, 30-Sep-99.
;;; add rgr-c-use-electric-dash-p.  -- rgr, 9-Nov-99.
;;; don't try to run netscape on mcclintock from home!  -- rgr, 9-Dec-99.
;;; rgr-thread-after-save-hook: no eql in 20.4.  -- rgr, 9-Dec-99.
;;; rgr-ilisp-load-hook: made this work.  -- rgr, 22-Dec-99.
;;; added rgr-w3-load-hook.  -- rgr, 16-Jan-00.
;;; added rgr-vm-mail-mode-hook.  -- rgr, 19-Jan-00.
;;; set user-mail-address at home.  -- rgr, 24-Jan-00.
;;; oops; move user-mail-address where rgr-site is defined.  -- rgr, 24-Jan-00.
;;; move i?lisp-mode-hook to the rgr-lisp-hacks.el file.  -- rgr, 28-Jan-00.
;;; bind C-x m to vm-mail globally.  -- rgr, 3-Feb-00.
;;; Turn off paging in subordinate shells.  -- rgr, 17-Feb-00.
;;; bind v+q-mbox-status to s-f1 globally.  -- rgr, 5-Mar-00.
;;; use bmerc version of rgr-thread-after-save-hook, install
;;;	rgr-mac-before-save-hook at home.  -- rgr, 17-Mar-00.
;;; only save completions in rgr-emacs if we can write it!  -- rgr, 1-Apr-00.
;;; set rgr-ssh-default-destination.  -- rgr, 12-Apr-00.
;;; rgr-ange-ftp-load-hook: new.  -- rgr, 13-Apr-00.
;;; matlab-mode-hook & matlab-shell-mode-hook.  -- rgr, 26-Jul-00.
;;; change rgr-ssh-default-destination from BU.  -- rgr, 7-Sep-00.
;;; Put ("\\.vm$" . vm-mode) on auto-mode-alist.  -- rgr, 27-Nov-00.
;;; rgr-elisp-find-tag-for-emacs-key binding.  -- rgr, 17-Dec-00.
;;; display and ssh checks for rgr-web-client-name setup.  -- rgr, 12-Jan-01.
;;; shorten sigler's frame.  -- rgr, 7-Mar-01.
;;; use feynman as rgr-web-client-host.  -- rgr, 8-Mar-01.
;;; remove sigler frame-height hackery.  -- rgr, 10-Apr-01.
;;; Mailcrypt stuff.  -- rgr, 3-May-01.
;;; flush transient-mark-mode, other cleanups.  -- rgr, 27-Jun-01.
;;; Added some xemacs stuff.  -- rgr, 26,27-Jul-01.
;;; flush bootcamp stuff, add term kludge for Tru64.  -- rgr, 20-Aug-01.
;;; update mc-gpg-user-id to new key.  -- rgr, 29-Jan-02.
;;; extended completion-ignored-extensions value.  -- rgr, 15-Mar-02.
;;; autoload wiki-remote-get.  -- rgr, 1-Apr-02.
;;; alternative v+q-mbox-status on f2.  -- rgr, 16-Jan-03.
;;; update mc-gpg-user-id to use new key.  -- rgr, 12-Feb-03.
;;; use mail-default-headers to BCC around spam filters.  -- rgr, 20-Mar-03.
;;; oops; mail-default-headers is bound by sendmail.el.  -- rgr, 24-Mar-03. 
;;; clear imenu-scanning-message.  -- rgr, 25-Mar-03.
;;;

(defvar rgr-emacs (expand-file-name
		    ;; use explicit user id so su works.
		    "~rogers/emacs"))
;; this is done now by site-start.el (the system init).  -- rgr, 19-Oct-98.
(or (member rgr-emacs load-path)
    (setq load-path (cons rgr-emacs load-path)))
(load (expand-file-name "rgr-hacks-autoloads.el" rgr-emacs))
;; For Discus.
(let ((discus (expand-file-name "discus" rgr-emacs)))
  (or (member discus load-path)
      (setq load-path (cons discus load-path))))
(load (expand-file-name "discus/loaddefs.el" rgr-emacs))

(setq rgr-space-means-execute-and-exit t)
(setq signature-login-name "rgr")
(rgr-install-hacks)

;; don't enable transient-mark-mode (seems to be on by default in the SuSe 20.7
;; version).  -- rgr, 27-Jun-01.
(setq transient-mark-mode nil)
;; turn off annoying "Scanning buffer for index" messages under ilisp.  -- rgr,
;; 25-Mar-03.
(setq imenu-scanning-message nil)
;; get command bindings in apropos.  -- rgr, 9-Apr-03.
(setq apropos-do-all t)

;; Some xemacs stuff . . .
(setq tags-build-completion-table nil)
(setq zmacs-regions nil)

;; Turn off paging in subordinate shells.  -- rgr, 17-Feb-00.
(setenv "PAGER" "cat")

;; set up ssh default.  -- rgr, 12-Apr-00.
(if (eq rgr-site 'home)
    (setq rgr-ssh-default-destination "rogers@huxley.bu.edu")
    (setq rgr-ssh-default-destination "rogers@rgrjr.dyndns.org"))

(defun rgr-ange-ftp-load-hook ()
  ;; [may need cleaning up, should probably go elsewhere.  -- rgr, 13-Apr-00.]
  (require 'ange-ftp-hacks)
  (and (string-match (regexp-quote "\\|\\.ps$")
		     ange-ftp-binary-file-name-regexp)
       ;; Don't treat PostScript as binary.
       (setq ange-ftp-binary-file-name-regexp
	     (replace-match "" nil nil ange-ftp-binary-file-name-regexp))))

(cond ((not (rgr-emacs-version-p 19))
	;; No ange-ftp in emacs 18.  -- rgr, 9-Aug-95.
	(require 'rgr-emacs-18-hacks))
      ((memq 'ange-ftp features)
	(rgr-ange-ftp-load-hook))
      (t
	;; note that eval-after-load initially appeared in emacs 19.0, but the
	;; semantics weren't changed to "evaluate immediately if already loaded"
	;; until 19.29.
	(eval-after-load "ange-ftp" '(rgr-ange-ftp-load-hook))))

;; This matters for certain threading applications.  -- rgr, 19-Nov-97.  [but
;; solaris screws it up; ONLY shows the group.  -- rgr, 27-Oct-98.]
(setq dired-listing-switches
      (if (string-match "solaris" system-configuration)
	  "-al"
	  "-alg"))
;; new hack  -- rgr, 2-Feb-99.
(defun rgr-dired-mode-hook ()
  (define-key dired-mode-map "\C-cr" 'rgr-dired-rename-file-and-versions))
(add-hook 'dired-load-hook 'rgr-dired-mode-hook)

;; Newer feature (ported from Lispm implementation).  -- rgr, 29-Nov-96.
;; [only save completions in rgr-emacs if we can write it!  -- rgr, 1-Apr-00.]
(and (file-writable-p rgr-emacs)
     (setq rgr-abbrev-completion-save-directory rgr-emacs))
(rgr-install-abbrev-completion)
(rgr-install-weekly-completion-cycle)

;; New feature!  -- rgr, 20-Jul-95.
(require 'ilisp-possibilities)

(rgr-make-tags-table-list-hook)
;; Zmacs binds this to C-M-., but that's find-tag-regexp in GNU emacs.  -- rgr,
;; 17-Dec-00.
(global-set-key "\C-c\M-." 'rgr-elisp-find-tag-for-emacs-key)

(setq completion-ignored-extensions
      (append '(;; ACL (and as modified).
		".fasl" ".aw86f" ".al86f"
		;; CMU Common Lisp.
		".x86f" ".axpf" ".sparcf" ".sparcf7"
		;; LispWorks?
		".afasl")
	      completion-ignored-extensions))

(add-hook 'rmail-mode-hook 'rgr-rmail-mode-hook)
(add-hook 'mail-mode-hook 'rgr-mail-mode-hook)
(add-hook 'vm-mail-mode-hook 'rgr-vm-mail-mode-hook)
(global-set-key "\C-xm" 'vm-mail)
(global-set-key (if (eq rgr-emacs-flavor 'fsf) [s-f1] [(super f1)])
		'v+q-mbox-status)
;; also try f2, which is easier to type at MGI.  -- rgr, 16-Jan-03.
(global-set-key [f2] 'v+q-mbox-status)
(if (eq rgr-site 'home)
    (setq mail-self-blind nil
	  mail-default-headers (concat (and (boundp 'mail-default-headers)
					    mail-default-headers)
				       "BCC: rogers-x@rgrjr.dyndns.org\n"))
    (setq mail-self-blind t))
;; Mailcrypt stuff.
(setq mc-default-scheme 'mc-scheme-gpg)
(setq mc-pgp-path "gpg")
(setq mc-gpg-user-id "Bob Rogers (main 2003) <rogers@rgrjr.dyndns.org>")
(setq mc-pgp-keydir (expand-file-name "~/.gnupg"))
(setq mc-passwd-timeout 600)	;; ten minutes.

(cond ((eq rgr-site 'home)
	(add-hook 'write-file-hooks 'rgr-mac-before-save-hook))
      ((eq rgr-site 'bmerc)
	(add-hook 'after-save-hook 'bmerc-thread-after-save-hook)))

(cond ((rgr-emacs-version-p 19 0)
	(add-hook 'comint-mode-hook 'rgr-comint-mode-hook)
	(add-hook 'shell-mode-hook 'rgr-shell-mode-hook)
        (add-hook 'telnet-mode-hook 'rgr-telnet-mode-hook)))
(add-hook 'sh-mode-hook 'rgr-sh-mode-hook)

(rgr-define-lisp-mode-commands emacs-lisp-mode-map)
(rgr-define-lisp-mode-commands lisp-interaction-mode-map)
(rgr-common-lisp-indentation)

(add-hook 'ilisp-mode-hook 'rgr-ilisp-mode-hook)
(add-hook 'lisp-mode-hook 'rgr-lisp-mode-hook)
;; [doing cmulisp seems to undo this.  -- rgr, 5-Apr-94.]  [actually, i think i
;; was taking ilisp-load-hook for ilisp-mode-hook.  -- rgr, 28-Jan-00.]
(add-hook 'cmulisp-hook 'rgr-lisp-mode-hook)
(add-hook 'emacs-lisp-mode-hook 'rgr-lisp-mode-hook)
(add-hook 'text-mode-hook 'rgr-text-mode-hook)
(add-hook 'c-mode-hook 'rgr-c-mode-hook)
(setq rgr-c-use-electric-dash-p t)
(add-hook 'compilation-mode-hook 'rgr-compilation-mode-hook)
(add-hook 'makefile-mode-hook 'rgr-makefile-mode-hook)
(add-hook 'perl-mode-hook 'rgr-perl-mode-hook)
(add-hook 'matlab-mode-hook 'rgr-matlab-mode-hook)
(add-hook 'matlab-shell-mode-hook 'rgr-matlab-shell-mode-hook)
(add-hook 'command-history-hook 'rgr-command-history-hook)
;; New language.  -- rgr, 11-Aug-99.
(add-hook 'tcl-mode-hook 'rgr-tcl-mode-hook)

;; Oops; vm doesn't do this.  -- rgr, 27-Nov-00.
(let ((entry '("[^/.]\\.vm$" . vm-mode)))
  (or (member entry auto-mode-alist)
      (setq auto-mode-alist (cons entry auto-mode-alist))))

;; Add my HTML code hacks.  (Only need to do this once.)
(add-hook 'html-helper-load-hook 'rgr-html-define-commands)
(add-hook 'html-helper-load-hook 'rgr-html-fix-regexps)
(add-hook 'rgr-html-tags-load-hook '(lambda () (load "rgr-html-servers")))
;; This has some per-buffer stuff.  -- rgr, 19-Mar-96.
(add-hook 'html-helper-mode-hook 'rgr-html-helper-mode-hook)
;; Ensure that we can fire up netscape.  We need to make special arrangements if
;; we are running under X11 on a non-Internet-enabled host at BMERC.
(let ((display (getenv "DISPLAY")))
  (if (and (eq rgr-site 'bmerc)
	   ;; running under X11
	   display
	   ;; not via SSH (that would mean some other "real" display).
	   (not (string-match ":[1-9][0-9]" display))
	   ;; the local host can't get to the Internet by itself.
	   (not (and (boundp 'bmerc-internet-host-regexp)
		     (string-match bmerc-internet-host-regexp (system-name)))))
      ;; We're not Internet-enabled; run netscape on somebody who is.  -- rgr,
      ;; 14-Mar-96.  [bmerc-internet-host-regexp is normally set by
      ;; /usr/local/share/emacs/site-lisp/site-start.el, but this probably won't
      ;; work for emacs 18.  -- rgr, 19-Mar-96.] . . .  [try sewall so we can
      ;; get matlab docs.  -- rgr, 4-Nov-98.]  [moved to mcclintock to simplify
      ;; xhost setup.  -- rgr, 1-Jul-99.]  [added display and ssh checks.  --
      ;; rgr, 12-Jan-01.]  [moved to feynman.  -- rgr, 8-Mar-01.]
      (setq rgr-web-client-host "feynman"
	    rgr-web-client-name "/usr/local/bin/netscape")))
;; Emacs/W3
(add-hook 'w3-load-hook 'rgr-w3-load-hook)
;; wiki-remote stuff.
(autoload 'wiki-remote-get "wiki-remote.el" "Get wiki PAGE from wiki NAME." t)

;; Change TERM=emacs into something that Tru64 "man" can deal with.  It refuses
;; to run if it can't recognize the terminal type, which is broken; it should
;; just assume "dumb" (though M-x manual-page could force this).  Change this
;; (almost) last so that other inits can see the original TERM value.  -- rgr,
;; 20-Aug-01.
(if (equal (getenv "TERM") "emacs")
    (setenv "TERM" "dumb"))

;;; Additional inits.
(cond ((and (eq rgr-site 'bmerc)
	    (not (equal (user-real-login-name) "rogers")))
	;; We are "su psa" (or thread, or somebody else) -- load that init too.
	;; [but disable the completion save.  -- rgr, 24-Mar-97.]
	(setq rgr-abbrev-completion-save-file nil)
	(load (concat "~" (user-real-login-name) "/.emacs") t)))
