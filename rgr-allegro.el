;;;; Hacking the allegro interface.
;;;
;;;    Modification history:
;;;
;;; created.  -- rgr, 10-Jan-00.
;;; clear fi:define-emacs-lisp-mode switch, random fixes.  -- rgr, 22-Jan-00.
;;; rgr-allegro-load-file-maybe-compile: new hack.  -- rgr, 5-Feb-00.
;;; rgr-fi-inferior-lisp-show-output: new, related comint.  -- rgr, 12-Feb-00.
;;; include eli on tags-table-list.  -- rgr, 13-Feb-00.
;;; clim: remove stupid binding.  -- rgr, 20-Feb-00.
;;; indentation hacking.  -- rgr, 19-Nov-00.
;;; Allegro 6.0 upgrade.  -- rgr, 21-Nov-00.
;;; rgr-fi-inferior-common-lisp-mode-hook: fix comint-prompt-regexp for ACL 6.0
;;;	debugger prompts (e.g. "[1c] icancad(8): ").  -- rgr, 24-Nov-00.
;;; fi:lisp-find-definition-other-window bindings.  -- rgr, 17-Mar-01.
;;; indentation fixing.  -- rgr, 23-May-01.
;;; fi:list-class-methods: new.  -- rgr, 31-May-01.
;;; fi:list-class-inherited-methods: new.  -- rgr, 4-Jul-01.
;;; alisp: new fn.  -- rgr, 13-Jul-01.
;;; (setq fi:common-lisp-host "localhost").  -- rgr, 18-Oct-01.
;;; bind fi:lisp-function-documentation to "C-z d".  -- rgr, 27-Oct-01.
;;; Allegro 6.1 upgrade.  -- rgr, 16-Nov-01.
;;; tweak indentation fixes.  -- rgr, 15-Mar-02.
;;; ACL 6.2 beta.  -- rgr, 30-Apr-02.
;;; disable ACL 6.2 beta.  -- rgr, 10-Sep-02.
;;;

(require 'comint)

(defvar rgr-allegro-home-directory "/usr/local/acl61")
(defvar rgr-allegro-elisp-home-directory
        (expand-file-name "eli" rgr-allegro-home-directory))

;; skip some binary extensions installed by ~rogers/clinit.cl hacks.  -- rgr,
;; 30-Apr-02.
(setq completion-ignored-extensions
      (append '(".aw86f" ".al86f") completion-ignored-extensions))

(setq case-fold-search t)
(setq fi:find-tag-lock nil)
;; [franz's fi:emacs-lisp-mode-map bindings are not my idea of fun.  -- rgr,
;; 12-Feb-00.]
(setq fi:define-emacs-lisp-mode nil)
;; Use Franz interface.
(load (expand-file-name "fi-site-init.el" rgr-allegro-elisp-home-directory))
(setq fi:common-lisp-image-name
      (expand-file-name "clim" rgr-allegro-home-directory))
;; [tried to avoid hanging the network when dns is down, but this didn't help.
;; -- rgr, 4-Oct-01.]  [try it again.  -- rgr, 18-Oct-01.]
(setq fi:common-lisp-host "localhost")

(setq tags-table-list
      (append (rgr-make-tags-table-list (list rgr-allegro-elisp-home-directory))
	      tags-table-list))

;;;; Un-breaking indentation.

(remprop 'setf 'fi:lisp-indent-hook)
(remprop 'setq 'fi:lisp-indent-hook)
(remprop 'if 'fi:lisp-indent-hook)
;; [damn; must be special-cased.  -- rgr, 30-May-01.]
;; (remprop 'make-instance 'fi:lisp-indent-hook)
;; New ICanCAD definitions that need special indentation.
(let ((indent-mapping-form (get 'dolist 'fi:lisp-indent-hook)))
  (put 'mapping-parts 'fi:lisp-indent-hook indent-mapping-form)
  (put 'mapping-uses 'fi:lisp-indent-hook indent-mapping-form)
  (put 'mapping-bristles 'fi:lisp-indent-hook indent-mapping-form)
  (put 'mapping-extent 'fi:lisp-indent-hook indent-mapping-form))
(put 'dpt-let 'fi:lisp-indent-hook (get 'flet 'fi:lisp-indent-hook))
(put 'dpt-let-if 'fi:lisp-indent-hook (get 'flet 'fi:lisp-indent-hook))
(put 'dpt-case 'fi:lisp-indent-hook '((1 1 lambda-list) (0 t 1)))
(put 'mouse-binding-case 'fi:lisp-indent-hook
     ;; [can't seem to get this right . . .  -- rgr, 23-May-01.]
     ;; '((2 1 1) (1 t ((1 1 lambda-list) (0 t 1))))
     '((1 t ((1 1 lambda-list) (0 t 1)))))

;;;; Compile and load

(require 'comint)

;; [this is stolen from ilisp for cross-compatibility.  -- rgr, 5-Feb-00.]
(defvar lisp-prev-l/c-dir/file nil
  "Record last directory and file used in loading or compiling.
This holds a cons cell of the form `(DIRECTORY . FILE)'
describing the last `lisp-load-file' or `lisp-compile-file' command.")

(defvar lisp-source-modes '(fi:common-lisp-mode)
  "*Used to determine if a buffer contains Lisp source code.
If it's loaded into a buffer that is in one of these major modes, it's
considered a Lisp source file by `lisp-load-file' and `lisp-compile-file'.
Used by these commands to determine defaults.")

(defun lisp-file-extension (file extension)
  ;; [more ilisp theft.  -- rgr, 5-Feb-00.]
  "Return FILE with new EXTENSION."
  (concat (substring file 0 (string-match ".[^.]*$" file))
	  "." extension))

(defun rgr-allegro-load-file-maybe-compile (file-name)
  "Load a lisp file, offering to save or compile if appropriate."
  (interactive (comint-get-source
		"Load Lisp file: " lisp-prev-l/c-dir/file
		lisp-source-modes nil))
  (comint-check-source file-name)	; Check to see if buffer needs saving.
  (setq lisp-prev-l/c-dir/file (cons (file-name-directory    file-name)
				     (file-name-nondirectory file-name)))
  ;; Ivan's hack for ange-ftp pathnames...
  (let* ((file-name (if (string-match "/.*?@.*:" file-name)
			(substring file-name (match-end 0))
			file-name))
	 (binary (lisp-file-extension file-name "fasl")))
    (cond ((and (file-newer-than-file-p file-name binary)
		(y-or-n-p "Compile first? "))
	    (fi:compile-and-load-file file-name))
	  ((file-readable-p binary)
	    (fi:load-file binary))
	  (t
	    (fi:load-file file-name)))))

(defun fi:list-class-methods (class-name)
  ;; [requires icancad::class-methods-function-specs in
  ;; ~rogers/projects/sas/code/debug.lisp to work.  -- rgr, 31-May-01.]
  "List all the generic function methods of CLASS-NAME.  `List' means to show
them in a buffer in definition mode.  The source for each definition can be
easily found via key bindings in definition mode.  The default CLASS-NAME is
taken from the text surrounding the point.  fi:package is used to determine
from which Common Lisp package the operation is done.  In a subprocess
buffer, the package is tracked automatically.  In source buffer, the
package is parsed at file visit time."
  (interactive (fi::get-default-symbol "List methods of class" t t))
  ;; Since this takes a while, tell the user that it has started.
  (message "Finding methods defined for class %s..." class-name)
  (lep::list-fspecs-common class-name
			   'icancad::class-methods-function-specs
			   "Cannot find class methods: %s"
			   "class method"))

(defun fi:list-class-inherited-methods (class-name)
  ;; [requires icancad::class-inherited-methods-function-specs in
  ;; ~rogers/projects/sas/code/debug.lisp to work.  -- rgr, 31-May-01.]
  "List all the generic function methods of CLASS-NAME plus those of all
of its superclasses, in class precedence list order.  This is roughly
comparable to doing :which-operations in Flavors, and then asking for
the names of all such methods defined on superclasses, except that
shadowed methods are also included.  As for the other fi:list-class-*
commands, `list' means to show them in a buffer in definition mode.  The
source for each definition can be easily found via key bindings in
definition mode."
  (interactive (fi::get-default-symbol "List methods of class" t t))
  ;; Since this takes a while, tell the user that it has started.
  (message "Finding methods defined for class %s..." class-name)
  (lep::list-fspecs-common class-name
			   'icancad::class-inherited-methods-function-specs
			   "Cannot find class methods: %s"
			   "class method"))
;;;###autoload
(defun rgr-fi-common-lisp-mode-hook ()
  ;; This is for editing lisp source files.  -- rgr, 17-Mar-01.
  (setq fill-column 80)
  (define-key fi:common-lisp-mode-map "\C-za" "\C-ca")
  (define-key fi:common-lisp-mode-map "\C-zd" 'fi:lisp-function-documentation)
  (define-key fi:common-lisp-mode-map "\C-zc" 'fi:lisp-eval-or-compile-defun)
  (define-key fi:common-lisp-mode-map "\C-zl"
    'rgr-allegro-load-file-maybe-compile)
  (define-key fi:common-lisp-mode-map "\M-."
    ;; need to look this up for rgr-ed (q.v.).
    (lookup-key fi:common-lisp-mode-map "\C-c."))
  ;; don't know why this isn't the default . . .
  (define-key fi:common-lisp-mode-map "\C-x4."
    'fi:lisp-find-definition-other-window)
  (rgr-define-lisp-mode-commands fi:common-lisp-mode-map)
  '(mapcar (function (lambda (key)
	    (define-key (current-local-map) key nil)))
	  '("\M-a" "\M-c" "\M-d"))
  )

(defun rgr-fi-inferior-lisp-show-output ()
  ;; conceptually based on comint-show-output, but not restricted to the
  ;; currently executing (or last) command.
  "Display start of this batch of interpreter output at top of window.
Sets mark to the value of point when this command is run."
  (interactive)
  (push-mark)
  (comint-previous-prompt 1)
  (beginning-of-line)		;; keeps prompt on screen!
  (set-window-start (selected-window) (point))
  (comint-skip-prompt))

(defun rgr-fi-inferior-common-lisp-mode-hook ()
  ;; (interactive)	;; for testing
  ;; needed for the hacks below to work.
  (make-local-variable 'comint-prompt-regexp)
  (setq comint-prompt-regexp
	"^\\(\\[[0-9a-zA-Z]+\\] \\)?[-_a-zA-Z0-9]+([0-9]+): *")
  ;; [comint-show-output doesn't work; comint-last-input-end is not defined.  --
  ;; rgr, 12-Feb-00.]
  (define-key fi:inferior-common-lisp-mode-map "\M-\C-l"
    'rgr-fi-inferior-lisp-show-output)
  ;; [help! why did these keys go away?  -- rgr, 12-Feb-00.]
  (define-key fi:inferior-common-lisp-mode-map "\M-n" 'fi:push-input)
  (define-key fi:inferior-common-lisp-mode-map "\M-p" 'fi:pop-input)
  (define-key fi:inferior-common-lisp-mode-map "\C-c\C-n" 'comint-next-prompt)
  (define-key fi:inferior-common-lisp-mode-map "\C-c\C-p"
    'comint-previous-prompt)
  ;; don't know why this isn't the default . . .
  (define-key fi:common-lisp-mode-map "\C-x4."
    'fi:lisp-find-definition-other-window))

(defun rgr-acl-setup ()
  (define-key fi:inferior-common-lisp-mode-map "\C-za" "\C-ca")
  (define-key fi:inferior-common-lisp-mode-map "\M-."
    ;; need to look this up for rgr-ed (q.v.).
    (lookup-key fi:inferior-common-lisp-mode-map "\C-c."))
  ;; [why did I do this?!?  -- rgr, 19-Feb-00.]
  '(define-key fi:inferior-common-lisp-mode-map "\M-p"
    (lookup-key fi:inferior-common-lisp-mode-map "\C-c\C-p")))

;;;###autoload
(defun clim ()
  (interactive)
  (fi:common-lisp "*clim*" default-directory
		  (expand-file-name "clim" rgr-allegro-home-directory))
  (rgr-acl-setup))

;;;###autoload
(defun alisp ()
  (interactive)
  (fi:common-lisp "*alisp*" default-directory
		  (expand-file-name "alisp" rgr-allegro-home-directory))
  (rgr-acl-setup))

(add-hook 'fi:common-lisp-mode-hook 'rgr-lisp-mode-hook)
(add-hook 'fi:common-lisp-mode-hook 'rgr-fi-common-lisp-mode-hook)
(remove-hook 'fi:common-lisp-mode-hook 'turn-on-font-lock)
(add-hook 'fi:inferior-common-lisp-mode-hook
	  'rgr-fi-inferior-common-lisp-mode-hook)
