;;;; Analytic geometry functions.
;;;
;;;    Modification history:
;;;
;;; created.  -- rgr, 19-Mar-99.
;;; rgr-v-angle &c.  -- rgr, 23-Mar-99.
;;;

(defsubst rgr-vx (v) (car v))
(defsubst rgr-vy (v) (car (cdr v)))
(defsubst rgr-vz (v) (car (cdr (cdr v))))
(defsubst rgr-make-point (x y z) (list x y z))

(defsubst rgr-v+ (v1 v2)
  ;; vector sum
  (rgr-make-point (+ (rgr-vx v1) (rgr-vx v2))
		  (+ (rgr-vy v1) (rgr-vy v2))
		  (+ (rgr-vz v1) (rgr-vz v2))))

(defun rgr-v- (v1 v2)
  ;; vector difference
  (rgr-make-point (- (rgr-vx v1) (rgr-vx v2))
		  (- (rgr-vy v1) (rgr-vy v2))
		  (- (rgr-vz v1) (rgr-vz v2))))

(defun rgr-dot (v1 v2)
  (+ (* (rgr-vx v1) (rgr-vx v2))
     (* (rgr-vy v1) (rgr-vy v2))
     (* (rgr-vz v1) (rgr-vz v2))))

(defsubst rgr-vector-length (v)
  (sqrt (rgr-dot v v)))

(defun rgr-normalize (v)
  ;; Return a unit vector in the direction of V.
  (let ((length (rgr-vector-length v)))
    (rgr-make-point (/ (rgr-vx v) length)
		    (/ (rgr-vy v) length)
		    (/ (rgr-vz v) length))))

(defun rgr-v-angle (v1 v2)
  ;; Given two vectors v1 and v2, find the angle between them.
  (let ((len1^2 (rgr-dot v1 v1))
	(len2^2 (rgr-dot v2 v2)))
    ;; (message "%s %s" v1 v2)
    ;; (sit-for 2)
    (if (or (zerop len1^2) (zerop len2^2))
	0.0
	(acos (/ (rgr-dot v1 v2)
		 (sqrt (* len1^2 len2^2)))))))

(defsubst rgr-pt-angle (a b c)
  ;; Given three points A, B, and C, find the angle made by the two vectors BA
  ;; and BC around the central point B.
  (rgr-v-angle (rgr-v- b a) (rgr-v- b c)))

(defun rgr-transform (matrix v)
  ;; this doesn't allow for translation.
  (rgr-make-point (rgr-dot v (rgr-vx matrix))
		  (rgr-dot v (rgr-vy matrix))
		  (rgr-dot v (rgr-vz matrix))))

(provide 'rgr-geometry)
