;;;****************************************************************************
;;;
;;;    Emacs mouse hackery.
;;;
;;;    The rgr-mouse package defines a number of Symbolics-compatible mouse
;;; commands that work under X11 in either FSF emacs 18 or FSF emacs 19 through
;;; 21.  (I haven't tried it with xemacs, which is probably different.)  Of
;;; greatest interest is rgr-mouse-edit-thing, which finds the file, directory,
;;; or source code (using either ilisp, Franz Inc's eli, or tags) of the thing
;;; clicked on, as appropriate.  It is mostly compatible with Symbolics Genera
;;; #\Meta-Mouse-Left click on random objects.  Other features are described
;;; below.
;;;
;;;    rgr-document-mouse-commands -- Print the list of all currently-bound
;;; mouse commands and their gestures.  (In emacs 19, "drag" means a drag event,
;;; and "down" means a down event; otherwise, it's an up event, even though the
;;; names of the commands bound to these events would seem to conflict.  The
;;; triple of down, drag, and up must always be bound consistently.)
;;;
;;;    rgr-mouse-insert-thing -- Insert the thing under the mouse at point.
;;; Sets the mark to the beginning of the insertion, and leaves point at the
;;; end.  This works between windows/buffers, including the minibuffer, and does
;;; not change the current buffer.
;;;
;;;    rgr-mouse-mark-text-down and rgr-mouse-mark-text-up -- Set mark or copy
;;; text to kill ring.  If clicked in a single spot, sets the mark there.  If
;;; clicked in one spot & dragged to and released at another, copies the
;;; intervening text to the kill ring.  Does not change the current buffer.  For
;;; Symbolics hackers, this is like #\C-Mouse-Left.  [unfortunately, it doesn't
;;; highlight during the drag in emacs 19.  -- rgr, 27-Jan-95.]
;;;
;;;    rgr-install-mouse-commands -- binds these to Symbolics-compatible mouse
;;; clicks.  Note that this shadows mouse-set-secondary (on M-left) and moves
;;; mouse-buffer-menu from C-left to M-right (which in turn shadows
;;; mouse-secondary-save-then-kill).
;;;
;;;*****************************************************************************
;;;
;;; To do:
;;;
;;;    1.  Make rgr-mouse-mark-text-down highlight during the drag.
;;;
;;;    [original] Modification history:
;;;
;;; rgr-mouse-edit-thing, other mouse hackery.  -- rgr, 30-Mar-94.
;;; rgr-mouse-insert-thing, better -edit-thing.  -- rgr, 1-Apr-94.
;;; . . .
;;; *** emacs 19 update ***
;;;	Split out ./rgr-mouse-18.el and ./rgr-rmail-18.el .  -- rgr, 17-Jan-95.
;;; split out of ./rgr-hacks.el as the rgr-mouse package.  -- rgr, 27-Jan-95.
;;; rgr-find-url: teach rgr-ed to find URL's.  -- rgr, 30-Jan-95.
;;; . . .
;;; split out lisp stuff to new ilisp-mouse.el file.  -- rgr, 8-Apr-03.
;;;
;;; $Id$

;;;; Loading required code.

(require 'rgr-mouse-21)

(provide 'rgr-mouse)
