(in-package :prolog)

(defvar *trail* (make-array 200 :fill-pointer 0 :adjustable t))
(defvar *var-counter* 0)

(defparameter unbound "Unbound")

(defmacro deref (exp)
  "Follow pointers for bound variables."
  `(progn (loop while (and (var-p ,exp) (bound-p ,exp))
               do (setf ,exp (var-binding ,exp)))
	  ,exp))

(defstruct (var (:constructor ? ())
		(:print-function print-var))
  (name (incf *var-counter*))
  (binding unbound))

(defun bound-p (var) (not (eq (var-binding var) unbound)))

(defun print-var (var stream depth)
  (if (or (and (numberp *print-level*)
	       (>= depth *print-level*))
	  (var-p (deref var)))
      (format stream "?~a" (var-name var))
      (write var :stream stream)))

(defun undo-bindings! (old-trail)
  "Undo all bindings back to a given point in the trail."
  (loop until (= (fill-pointer *trail*) old-trail)
       do (setf (var-binding (vector-pop *trail*)) unbound)))

(defun unify! (x y)
  (cond ((eql (deref x) (deref y)) t)
	((var-p x) (set-binding! x y))
	((var-p y) (set-binding! y x))
	((and (consp x) (consp y))
	 (and (unify! (first x) (first y))
	      (unify! (rest x) (rest y))))
	(t nil)))

(defun set-binding! (var value)
  (unless (eq var value)
    (vector-push-extend var *trail*)
    (setf (var-binding var) value))
  t)
