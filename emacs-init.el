;;;****************************************************************************
;;;
;;;;   GNU Emacs init file.
;;;
;;; [created.  -- rgr, 8-Apr-94.]
;;;

(or (fboundp 'string-to-int)
    ;; [kludge to keep vm working in 26.0.50.  -- rgr, 30-Dec-16.]
    (fset 'string-to-int 'cl-parse-integer))

(defvar rgr-emacs (if load-file-name
		      (file-name-directory load-file-name)
		      ;; use explicit user id so su works.
		      (expand-file-name "~rogers/emacs/rgr-hacks"))
  "Where rgr-hacks files live.
This is a defvar so that it can be set globally by some prior .emacs file,
but it is usually sufficient to take the default.")
(add-to-list 'load-path rgr-emacs)
(load (expand-file-name "rgr-hacks-autoloads.el" rgr-emacs))
;; For other packages.
(if (file-directory-p "/usr/share/emacs/site-lisp/vm")
    ;; This is the emacs-vm RPM location.  Loading suse-start-vm.el adds it to
    ;; the load path and sets up autoloads.  -- rgr, 24-Jul-21.
    (load-file "/usr/share/emacs/site-lisp/vm/suse-start-vm.el"))
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

;; Turn the menu bar off.
(menu-bar-mode 0)

;; Look for search strings across newlines.  -- rgr, 5-Nov-21.
(setq search-whitespace-regexp "[ \t\n]+")

;; Keep on blinking when in focus (24.3.50).  -- rgr, 1-Jan-14.
(setq blink-cursor-blinks 0)

;; [kludge for vm in certain ModGen configs.  -- rgr, 31-Jul-12.]
(or (fboundp 'make-local-hook)
    (defun make-local-hook (&rest ignore) nil))

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
;; Get X11 selections on the kill ring.  This restores 23.x behavior in emacs 24
;; and later.  -- rgr, 15-Oct-10.
(setq select-enable-primary t)

;; Turn off paging in subordinate shells.  -- rgr, 17-Feb-00.
(setenv "PAGER" "cat")

;; Make C-x = show the names of characters (annoying for ASCII, but essential
;; for the wilder reaches of Unicode).  -- rgr, 24-Nov-21.
(setq what-cursor-show-names t)

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
(if (and (file-directory-p "/proc/acpi/battery")
	 (fboundp 'display-battery-mode))
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
		".afasl")
	      completion-ignored-extensions))

;; Set up mail stuff.
(add-hook 'mail-mode-hook 'rgr-mail-mode-hook)
(add-hook 'vm-mail-mode-hook 'rgr-vm-mail-mode-hook)
(add-hook 'mail-setup-hook 'rgr-mail-setup-hook)
(add-hook 'mail-setup-hook 'rgr-mail-abbrevs-setup)
(add-hook 'mail-citation-hook 'rgr-vm-mail-citation-hook)
(if (and (eq rgr-site 'home)
	 (not (equal (user-real-login-name) "root"))
	 (let ((conn (getenv "SSH_CONNECTION")))
	   ;; Insist on an SSH connection not within the same class C subnet.
	   (and conn
		(let* ((digits "[0-9]+")
		       (class-c (concat digits "\\." digits "\\." digits))
		       (ipv4 (concat "\\(" class-c "\\)\\." digits)))
		  ;; [the full string has another " <digits>"; the non-IPv4
		  ;; digits are the port numbers, which are uninteresting for
		  ;; our purposes.  -- rgr, 23-Mar-21.]
		  (not (and (string-match (concat "^" ipv4 " " digits " " ipv4)
					  conn)
			    (equal (match-string 1 conn)
				   (match-string 2 conn)))))))
	 (fboundp 'v+q-mbox-status))
    (add-hook 'focus-in-hook 'v+q-mbox-status))
(setq compose-mail-user-agent-warnings nil)	;; suppress warnings for root.
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
;; Double the default so we don't get queries about large mail files.
(setq large-file-warning-threshold 20000000)

;; snoop-maildir stuff.  -- rgr, 25-Mar-17.
(setq snoop-maildir-spam-folder
      (expand-file-name "~/mail/incoming/spam/cur/"))

;; Hookify some standard modes.
(add-hook 'comint-mode-hook 'rgr-comint-mode-hook)
(add-hook 'shell-mode-hook 'rgr-shell-mode-hook)
(add-hook 'telnet-mode-hook 'rgr-telnet-mode-hook)
(add-hook 'sh-mode-hook 'rgr-sh-mode-hook)
(add-hook 'dired-load-hook 'rgr-dired-load-hook)

(rgr-define-lisp-mode-commands emacs-lisp-mode-map)
(rgr-define-lisp-mode-commands lisp-interaction-mode-map)
(electric-indent-mode 0)
(rgr-common-lisp-indentation)

(rgr-install-diff-hacks)
(add-hook 'log-view-mode-hook 'rgr-log-view-mode-hook)
(add-hook 'lisp-mode-hook 'rgr-lisp-mode-hook)
(add-hook 'cmulisp-hook 'rgr-lisp-mode-hook)
(add-hook 'emacs-lisp-mode-hook 'rgr-lisp-mode-hook)
(add-hook 'text-mode-hook 'rgr-text-mode-hook)
(add-hook 'c-mode-hook 'rgr-c-mode-hook)
(setq rgr-c-use-electric-dash-p t)
;; [this rules out (among other things) files in VC directories, TAGS files,
;; test coverage output, and emacs backup/autosave files.  -- rgr, 28-Feb-05.]
(setq grep-find-command
      (concat "find . -type f "
	      "| grep -Ev '/\\.?#|~$|/(TAGS$|\\.svn|\\.git|old|blib|cover_)|\\.(patch|tmp)$' "
	      "| xargs -e grep -n -e "))
(add-hook 'compilation-mode-hook 'rgr-compilation-mode-hook)
(add-hook 'makefile-mode-hook 'rgr-makefile-mode-hook)
(add-hook 'perl-mode-hook 'rgr-perl-mode-hook)
;; Don't replace ASCII with symbols I can't read.  [Emacs 24.3.50.]
;; [this has to be done before loading perl-mode.  -- rgr, 4-Dec-12.]
(setq perl-prettify-symbols nil)
(add-hook 'cperl-mode-hook 'rgr-cperl-mode-hook)
(add-hook 'matlab-mode-hook 'rgr-matlab-mode-hook)
(add-hook 'matlab-shell-mode-hook 'rgr-matlab-shell-mode-hook)
(add-hook 'command-history-hook 'rgr-command-history-hook)

;; Oops; vm doesn't do this.  -- rgr, 27-Nov-00.
(add-to-list 'auto-mode-alist '("INBOX$\\|[^/.]\\.vm$" . vm-mode))

;; Add html-helper-mode plus my HTML code hacks.
(autoload 'html-helper-mode "html-helper-mode" "Yay HTML" t)
(add-to-list 'auto-mode-alist '("\\.html?$" . html-helper-mode))
(add-hook 'html-helper-load-hook 'rgr-html-define-commands)
(add-hook 'html-helper-load-hook 'rgr-html-fix-regexps)
;; This has some per-buffer stuff.  -- rgr, 19-Mar-96.
(add-hook 'html-helper-mode-hook 'rgr-html-helper-mode-hook)

;; Prefer Firefox.
;; [but let's try chromium on orion for a while.  -- rgr, 17-Mar-21.]
(setq browse-url-browser-function
      (if (equal (system-name) "orion")
	  'browse-url-chromium 'browse-url-firefox))

;; Emacs/W3
(add-hook 'w3-load-hook 'rgr-w3-load-hook)
;; wiki-remote stuff.
(autoload 'wiki-remote-get "wiki-remote.el" "Get wiki PAGE from wiki NAME." t)

;; M-x ssh.  -- rgr, 1-Aug-03.
(setq ssh-host-history
      (append (if (eq rgr-site 'mgi)
		  '("yuggoth" "granada")
		  '("rogers@lincoln.modulargenetics.com"))
	      '("rogers@rgrjr.dyndns.org")))
(if (eq rgr-site 'home)
    ;; Enable tunnelling to make the ModGen database and intranet Web servers
    ;; available from home.  -- rgr, 29-Feb-04.
    (setq ssh-per-host-option-alist
	  '(("woburn\\.modulargenetics\\.com$"
	     "-L" "8081:alexandria:80"
	     "-L" "8082:oxford:80"
	     "-L" "8083:persepolis:80"
	     "-L" "8084:yuggoth:80"
	     "-L" "8085:granada:80"
	     "-L" "8086:xanadu:80"
	     "-L" "8087:darwin:80")
	    ("lincoln\\.modulargenetics\\.com$"
	     "-L" "8081:localhost:80"))))

;; VC hacks.  -- rgr, 6-Aug-04.
(define-key text-mode-map "\C-c+" 'rgr-vc-log-plus)
(add-hook 'log-edit-mode-hook 'rgr-vc-log-edit-hook)
;; the -N is for files being added or deleted.
(setq vc-cvs-diff-switches '("-Nu"))
;; vc-dir-mode only appeared in Emacs 23.1
(add-hook 'vc-dir-mode-hook 'rgr-new-vc-install-vc-dir-mode-keys)
(add-to-list 'auto-mode-alist '("git-rebase-todo" . rgr-git-rebase-mode))
(add-to-list 'auto-mode-alist '("COMMIT_EDITMSG" . rgr-git-rebase-mode))

;; Prevent attempts by nroff to inflict novel Unicode characters on us.
(setq manual-program "LANG=en_US man")

;; See the M-x display-time-world command.
(setq display-time-world-list
      '(("America/New_York" "Boston")
	("Australia/Sydney" "Sydney")))

;; SQL hacks.
(setq sql-user "modest")
(setq sql-database "test_modest")
(setq sql-server "localhost")

(defun rgr-sql-mode-hook ()
  ;; Maybe set sql-buffer if we don't already have one.
  (unless (sql-buffer-live-p sql-buffer)
    (let* ((product (if (eq sql-product 'ansi)
			;; Don't take the default.
			'mariadb
		      sql-product))
	   (base-name (or (sql-get-product-feature product :name)
                          (symbol-name product)))
	   (prefix (concat "^*SQL: " (regexp-quote base-name)))
	   (new-buffer nil))
      ;; Look for an *SQL* buffer for the product.  See the
      ;; sql-generate-unique-sqli-buffer-name function.  We ignore the fact that
      ;; multiple buffers can be generated for a given product.
      (dolist (buffer (reverse (buffer-list)))
	(when (string-match prefix (buffer-name buffer))
	  (setq new-buffer buffer)))
      ;; If we found a live one, install it.
      (when (and new-buffer
		 (sql-buffer-live-p new-buffer))
	(setq sql-product product)
	(setq sql-buffer new-buffer)
	(run-hooks 'sql-set-sqli-hook)))))

(add-hook 'sql-mode-hook 'rgr-sql-mode-hook)

;; Ruby hacks.
(add-hook 'ruby-mode-hook 'rgr-ruby-mode-hook)

;; Erlang hacks.
(let ((erlang-path "/shared/emacs/erlang"))
  (cond ((file-directory-p erlang-path)
	  (or (member erlang-path load-path)
	      (setq load-path (cons erlang-path load-path)))
	  (require 'erlang-start))))

(defun rgr-erlang-mode-hook ()
  ;; Try to avoid shifting.  -- rgr, 20-Dec-96.
  (define-key erlang-mode-map "-" 'rgr-c-electric-dash)
  (setq fill-column 80))
(add-hook 'erlang-mode-hook 'rgr-erlang-mode-hook)

;; Python hacks.
(setq python-shell-completion-native-enable nil)

;; Javascript support.  -- rgr, 11-Mar-10.
(autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)
(autoload 'espresso-mode "espresso" "Espresso mode for editing Javascript" t)
(defun espresso-custom-setup ()
  (define-key espresso-mode-map "-" 'rgr-c-electric-dash)
  (moz-minor-mode 1))
(add-hook 'espresso-mode-hook 'espresso-custom-setup)

;; Run this after all load-path directories are set up.
(rgr-make-tags-table-list-hook)
(setq tags-revert-without-query t)
