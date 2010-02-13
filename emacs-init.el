;;;****************************************************************************
;;;
;;;;   GNU Emacs init file.
;;;
;;; [created.  -- rgr, 8-Apr-94.]
;;;
;;; $Id$

(defvar rgr-emacs (if load-file-name
		      (file-name-directory load-file-name)
		      ;; use explicit user id so su works.
		      (expand-file-name "~rogers/emacs/rgr-hacks"))
  "Where rgr-hacks files live.
This is a defvar so that it can be set globally by some prior .emacs file,
but it is usually sufficient to take the default.")
(or (member rgr-emacs load-path)
    (setq load-path (cons rgr-emacs load-path)))
(load (expand-file-name "rgr-hacks-autoloads.el" rgr-emacs))
;; For other packages.
(defvar rgr-imported-packages (expand-file-name "../imported" rgr-emacs)
  "Directory for other private emacs packages.")
(if (and (file-directory-p rgr-imported-packages)
	 (not (member rgr-imported-packages load-path)))
    (let ((autoloads (expand-file-name "loaddefs.el" rgr-imported-packages)))
      (setq load-path (cons rgr-imported-packages load-path))
      (if (file-readable-p autoloads)
	  (load autoloads))
      t))

(setq signature-login-name "rgr")
(rgr-install-hacks)

;; fix lame color scheme under KDE on SuSE 9.0.  -- rgr, 13-Mar-04.
;; [actually, let's make this the default.  -- rgr, 20-Mar-04.]
(set-background-color (if (zerop (user-real-uid))
			  ;; use something distinctive for root.  --
			  ;; rgr, 13-Mar-04.
			  "azure"
			  ;; use something more bland for normal users.
			  "linen"))
(set-foreground-color "black")

;; Disable transient-mark-mode (seems to be on by default in the SuSE 20.7
;; version).  -- rgr, 27-Jun-01.  [not to mention openSUSE 11.0.  -- rgr,
;; 9-Dec-08.]
(setq transient-mark-mode nil)
;; Disable "visual" C-n and C-p (Emacs 23), which screws up counting.  -- rgr,
;; 25-Dec-08.
(setq line-move-visual nil)
;; get command bindings in apropos.  -- rgr, 9-Apr-03.
(setq apropos-do-all t)
;; use unified diffs.  -- rgr, 6-Jul-03.
(setq diff-switches "-u")

;; Some xemacs stuff . . .
(setq tags-build-completion-table nil)
(setq zmacs-regions nil)

;; Initialize these to the coordinates of Boston.
(setq calendar-latitude   42.358)
(setq calendar-longitude -71.060)

;; Turn off the toolbar when working remotely, because it can be very slow over
;; ssh.  -- rgr, 5-May-03.  [actually, it's just as useless locally.  -- rgr,
;; 12-May-03.]
(cond ((and window-system
	    (fboundp 'display-graphic-p))
        (and (display-graphic-p)
	     (tool-bar-mode -1))
        (and (fboundp 'x-show-tip)
	     (tooltip-mode -1))))
;; Also turn off the splash graphic; it's not a problem when working locally,
;; but takes a few seconds to shovel over SSH.  -- rgr, 12-Jul-03.
(defun use-fancy-splash-screens-p () nil)

;; Turn off paging in subordinate shells.  -- rgr, 17-Feb-00.
(setenv "PAGER" "cat")

;; Set up /shared/emacs/site-lisp/ at home.
(cond ((and (eq rgr-site 'home)
	    ;; [lap always thinks it's home, but /shared/emacs/site-lisp is not
	    ;; always mounted.  -- rgr, 29-Jun-07.]
	    (file-directory-p "/shared/emacs/site-lisp")
	    (not (member "/shared/emacs/site-lisp" load-path)))
        (setq load-path (cons "/shared/emacs/site-lisp" load-path))
        (load "/shared/emacs/site-lisp/site-start.el")))

(add-hook 'dired-load-hook 'rgr-dired-load-hook)

;; Newer feature (ported from Lispm implementation).  -- rgr, 29-Nov-96.
;; [only save completions in rgr-emacs if we can write it!  -- rgr, 1-Apr-00.]
;; [now prefer to save in ~/emacs/completions instead.  -- rgr, 23-Apr-03.]
(setq rgr-abbrev-completion-save-directory
      (cond ((file-writable-p (expand-file-name "~/emacs/completions"))
	      (expand-file-name "~/emacs/completions"))
	    ((zerop (user-uid))
	      ;; [turn this off when root, because emacs thinks root can write
	      ;; anything, but it can't always when via nfs, and it's a pain to
	      ;; determine whether this is the case.  -- rgr, 12-Dec-02.]
	      nil)
	    ((file-writable-p rgr-emacs)
	      rgr-emacs)))
(rgr-install-abbrev-completion)
(and rgr-abbrev-completion-save-directory
     (rgr-install-weekly-completion-cycle))

;; New feature!  -- rgr, 20-Jul-95.  [this is now part of ilisp, distributed
;; separately, so don't fail if it can't be loaded.  -- rgr, 22-Apr-03.]
;; [now part of rgr-hacks.  -- rgr, 26-May-07.]
(require 'ilisp-possibilities)

;; Zmacs binds this to C-M-., but that's find-tag-regexp in GNU emacs.  -- rgr,
;; 17-Dec-00.
(global-set-key "\C-c\M-." 'rgr-elisp-find-tag-for-emacs-key)

;; Display the battery status if we have a battery.
(if (file-directory-p "/proc/acpi/battery")
    (display-battery-mode 1))

;; The default is after-completion (and used to be nil); setting it to t makes
;; it behave like Zmacs.  -- rgr, 1-Jul-09.
(setq confirm-nonexistent-file-or-buffer t)
;; We want to ignore these extensions during completion regardless of whether we
;; are actually running the relevant programs.
(setq completion-ignored-extensions
      (append '(;; ACL (and as modified).
		".fasl" ".aw86f" ".al86f"
		;; CMU Common Lisp.
		".x86f" ".axpf" ".sparcf" ".sparcf7"
		;; SBCL
		".sb86f"
		;; CLISP
		".fas" ".lib"
		;; LispWorks?
		".afasl"
		;; Parrot.
		".pbc")
	      completion-ignored-extensions))
;; Speaking of which, let's add a hook for Parrot.
(add-hook 'pir-mode-hook 'rgr-pir-mode-hook)
(let ((parrot "/usr/src/parrot/editor/parrot.new.el"))
  (if (file-readable-p parrot)
      (load-file parrot)))

(add-hook 'mail-mode-hook 'rgr-mail-mode-hook)
(add-hook 'mail-setup-hook 'rgr-mail-setup-hook)
(add-hook 'mail-setup-hook 'rgr-mail-abbrevs-setup)
(add-hook 'vm-mail-mode-hook 'rgr-vm-mail-mode-hook)
;; [this seems to be necessary, even with vm, in 23.2.  -- rgr, 11-Dec-09.]
(setq mail-signature nil)
(cond ((file-directory-p "/usr/src/viewmail/lisp")
	;; Enable the BZR version of vm 8.  -- rgr, 20-Apr-09.
	(setq load-path (cons "/usr/src/viewmail/lisp" load-path))
	;; [installed version goes into /usr/local/share/emacs/site-lisp/vm/,
	;; fwiw.  -- rgr, 20-Apr-09.]
        ; (setq debug-on-error t)
        (autoload 'vm-decode-postponed-mime-message "vm/vm-pine.el")
        (require 'vm-autoloads)))
(if (fboundp 'vm-mail)
    (global-set-key "\C-xm" 'vm-mail))
(setq mail-self-blind t)
;; Mailcrypt stuff.
(setq mc-default-scheme 'mc-scheme-gpg)
(setq mc-pgp-path "gpg")
(and (eq rgr-site 'home)
     (setq mc-gpg-user-id "Bob Rogers (main 2003) <rogers@rgrjr.dyndns.org>"))
(setq mc-pgp-keydir (expand-file-name "~/.gnupg"))
(setq mc-passwd-timeout 600)	;; ten minutes.
;; TMDA stuff.  -- rgr, 18-May-04.
(let ((base (expand-file-name (if (eq rgr-site 'home)
				  "~/.tmda/"
				  "~/.tmda/lists/"))))
  (setq tmda-default-whitelist
	(expand-file-name (if (eq rgr-site 'home) "accepted" "whitelist")
			  base))
  (setq tmda-default-blacklist (expand-file-name "rejected" base)))

(add-hook 'comint-mode-hook 'rgr-comint-mode-hook)
(add-hook 'shell-mode-hook 'rgr-shell-mode-hook)
(add-hook 'telnet-mode-hook 'rgr-telnet-mode-hook)
(add-hook 'sh-mode-hook 'rgr-sh-mode-hook)

(rgr-define-lisp-mode-commands emacs-lisp-mode-map)
(rgr-define-lisp-mode-commands lisp-interaction-mode-map)
(rgr-common-lisp-indentation)

(rgr-install-diff-hacks)
(add-hook 'log-view-mode-hook 'rgr-log-view-mode-hook)
(add-hook 'ilisp-mode-hook 'rgr-ilisp-mode-hook)
(add-hook 'lisp-mode-hook 'rgr-lisp-mode-hook)
;; [doing cmulisp seems to undo this.  -- rgr, 5-Apr-94.]  [actually, i think i
;; was taking ilisp-load-hook for ilisp-mode-hook.  -- rgr, 28-Jan-00.]
(add-hook 'cmulisp-hook 'rgr-lisp-mode-hook)
(add-hook 'emacs-lisp-mode-hook 'rgr-lisp-mode-hook)
(add-hook 'text-mode-hook 'rgr-text-mode-hook)
(add-hook 'c-mode-hook 'rgr-c-mode-hook)
(setq rgr-c-use-electric-dash-p t)
;; [this rules out files in CVS/SVN directories, TAGS files, normal emacs backup
;; and autosave files, and CVS ".#file.version" files.  -- rgr, 28-Feb-05.]
(setq grep-find-command
      (concat "find . -type f "
	      "| grep -Ev '/\\.?#|~$|/TAGS$|/\.svn/|/old|/cover_|/CVS/|\.(patch|tmp)$' "
	      "| xargs -e grep -n -e "))
(add-hook 'compilation-mode-hook 'rgr-compilation-mode-hook)
(add-hook 'makefile-mode-hook 'rgr-makefile-mode-hook)
(add-hook 'perl-mode-hook 'rgr-perl-mode-hook)
(add-hook 'cperl-mode-hook 'rgr-cperl-mode-hook)
(add-hook 'matlab-mode-hook 'rgr-matlab-mode-hook)
(add-hook 'matlab-shell-mode-hook 'rgr-matlab-shell-mode-hook)
(add-hook 'command-history-hook 'rgr-command-history-hook)
;; New language.  -- rgr, 11-Aug-99.
(add-hook 'tcl-mode-hook 'rgr-tcl-mode-hook)

;; Oops; vm doesn't do this.  -- rgr, 27-Nov-00.
(let ((entry '("[^/.]\\.vm$" . vm-mode)))
  (or (member entry auto-mode-alist)
      (setq auto-mode-alist (cons entry auto-mode-alist))))

;; Add html-helper-mode plus my HTML code hacks.
(autoload 'html-helper-mode "html-helper-mode" "Yay HTML" t)
(let ((cell (and (boundp 'magic-mode-alist)
		 ;; [in emacs 22, magic-mode-alist trumps auto-mode-alist.  --
		 ;; rgr, 28-Dec-06.]
		 (rassoc 'html-mode magic-mode-alist))))
  (if cell
      (setcdr cell 'html-helper-mode)
      ;; [not needed in 22?  -- rgr, 28-Dec-06.]
      (add-to-list 'auto-mode-alist '("\\.html?$" . html-helper-mode))))
(add-hook 'html-helper-load-hook 'rgr-html-define-commands)
(add-hook 'html-helper-load-hook 'rgr-html-fix-regexps)
(add-hook 'rgr-html-tags-load-hook '(lambda () (load "rgr-html-servers")))
;; This has some per-buffer stuff.  -- rgr, 19-Mar-96.
(add-hook 'html-helper-mode-hook 'rgr-html-helper-mode-hook)

;; Prefer mozilla, then firefox, then whatever browse-url-default-browser turns
;; up.  [Which does not always work; on carthage it's browse-url-gnome-moz,
;; which doesn't work without installing Gnome.  -- rgr, 18-Apr-07.]
(setq browse-url-browser-function
      (cond ((executable-find "mozilla")
	      (if (eq rgr-site 'home)
		  ;; [for some reason, "netscape" no longer works on my home
		  ;; system.  -- rgr, 7-Jul-07.]
		  'browse-url-firefox
		  'browse-url-netscape))
	    ((and (executable-find "firefox")
		  ;; Not available in Emacs 21.*.
		  (fboundp 'browse-url-firefox))
	      'browse-url-firefox)
	    (t browse-url-browser-function)))

;; Emacs/W3
(add-hook 'w3-load-hook 'rgr-w3-load-hook)
;; wiki-remote stuff.
(autoload 'wiki-remote-get "wiki-remote.el" "Get wiki PAGE from wiki NAME." t)

;; M-x ssh.  -- rgr, 1-Aug-03.
(setq ssh-host-history
      (append (if (eq rgr-site 'mgi)
		  nil
		  '("rogers@modulargenetics.com"))
	      '("rogers@rgrjr.dyndns.org" "rogers@huxley.bu.edu")))
(if (eq rgr-site 'home)
    ;; Enable tunnelling to make the ModGen database and intranet Web servers
    ;; available from home.  -- rgr, 29-Feb-04.
    (setq ssh-per-host-option-alist
	  '(("modulargenetics\\.com$"
	     "-L" "8080:rome:80"
	     "-L" "8081:alexandria:80"
	     "-L" "8082:karnak:80"
	     "-L" "8083:thebes:80"
	     "-L" "8084:carthage:80"
	     "-L" "8085:granada:80"
	     "-L" "8086:xanadu:80")
	    ;; This must be in the "home" list because lap on the road currently
	    ;; thinks its at home.
	    ("rgrjr\\.dyndns\\.org$"
	     "-L" "9143:localhost:143"))))

;; VC hacks.  -- rgr, 6-Aug-04.
(define-key text-mode-map "\C-c+" 'rgr-vc-log-plus)
(add-hook 'log-edit-mode-hook 'rgr-vc-log-edit-hook)
;; the -N is for files being added or deleted.
(setq vc-cvs-diff-switches '("-Nu"))
;; svn hacks.  -- rgr, 1-May-05.
(defvar rgr-new-vc-file
  (let* ((base-dir (if (and (eq rgr-site 'mgi)
			    (zerop (user-uid)))
		       ;; Obligatory root-owned location when root.
		       "/usr/local/src/emacs/"
		       ;; Normal location.
		       "/home/rogers/emacs/"))
	 (subdir (expand-file-name
		   (if (>= rgr-emacs-major-version 22) "new-vc-22" "new-vc")
		   base-dir)))
    (expand-file-name "new-vc.el" subdir)))
(if (and rgr-new-vc-file
	 ;; Use the native version control stuff in Emacs 23.
	 (<= rgr-emacs-major-version 22)
	 (file-readable-p rgr-new-vc-file))
    (load-file rgr-new-vc-file))
;; vc-dir-mode only appeared in Emacs 23.1
(add-hook 'vc-dir-mode-hook 'rgr-new-vc-install-vc-dir-mode-keys)

;; Prevent attempts by nroff to inflict novel Unicode characters on us.
(setq manual-program "LANG=en_US man")

;; See the M-x display-time-world command.
(setq display-time-world-list
      '(("America/Los_Angeles" "San Francisco")
	("America/New_York" "Boston")
	("Europe/London" "London")
	("Australia/Sydney" "Sydney")))

;; Ruby hacks.
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-hook 'ruby-mode-hook 'rgr-ruby-mode-hook)

;; Erlang hacks.
(let ((erlang-path "/shared/emacs/erlang"))
  (cond ((file-directory-p erlang-path)
	  (or (member erlang-path load-path)
	      (setq load-path (cons erlang-path load-path)))
	  (require 'erlang-start))))

;; Lua hacks.
(setq auto-mode-alist (cons '("\\.lua$" . lua-mode) auto-mode-alist))
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-hook 'lua-mode-hook 'rgr-lua-mode-hook)

(defun rgr-erlang-mode-hook ()
  ;; Try to avoid shifting.  -- rgr, 20-Dec-96.
  (define-key erlang-mode-map "-" 'rgr-c-electric-dash)
  (setq fill-column 80))
(add-hook 'erlang-mode-hook 'rgr-erlang-mode-hook)

;; Run this after all load-path directories are set up.
(rgr-make-tags-table-list-hook)

;;; Additional inits.
(cond ((and (eq rgr-site 'bmerc)
	    (not (equal (user-real-login-name) "rogers")))
	;; We are "su psa" (or thread, or somebody else) -- load that init too.
	;; [but disable the completion save.  -- rgr, 24-Mar-97.]
	(setq rgr-abbrev-completion-save-file nil)
	(load (concat "~" (user-real-login-name) "/.emacs") t)))
