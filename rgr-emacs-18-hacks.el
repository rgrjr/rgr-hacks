;; Definition of add-hook taken from GNU emacs 19.27.  -- rgr, 21-Feb-95.

(defun add-hook (hook function &optional append)
  "Add to the value of HOOK the function FUNCTION.
FUNCTION is not added if already present.
FUNCTION is added (if necessary) at the beginning of the hook list
unless the optional argument APPEND is non-nil, in which case
FUNCTION is added at the end.

HOOK should be a symbol, and FUNCTION may be any valid function.  If
HOOK is void, it is first set to nil.  If HOOK's value is a single
function, it is changed to a list of functions."
  (or (boundp hook) (set hook nil))
  ;; If the hook value is a single function, turn it into a list.
  (let ((old (symbol-value hook)))
    (if (or (not (listp old)) (eq (car old) 'lambda))
        (set hook (list old))))
  (or (if (consp function)
          (member function (symbol-value hook))
        (memq function (symbol-value hook)))
      (set hook
           (if append
               (nconc (symbol-value hook) (list function))
             (cons function (symbol-value hook))))))

