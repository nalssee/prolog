(in-package :prolog)

(defparameter no-bindings '((t . t)))

(defun find-all (item sequence &rest keyword-args
		 &key (test #'eql) test-not &allow-other-keys)
  "Find all those elements of sequence that match item."
  (if test-not
      (apply #'remove item sequence
	     :test-not (complement test-not) keyword-args)
      (apply #'remove item sequence
	     :test (complement test) keyword-args)))

(defun symbol-1 (&rest args)
  "Concatenate symbols or strings to form an interned symbol."
  (intern (format nil "~{~a~}" args)))

(defun new-symbol (&rest args)
  "Concatenate symbols or strings to form an uninterned symbol."
  (make-symbol (format nil "~{~a~}" args)))

(defun find-if-anywhere (pred tree)
  (cond ((funcall pred tree) tree)
	((atom tree) nil)
	((find-if-anywhere pred (first tree)))
	((find-if-anywhere pred (rest tree)))))

(defun find-anywhere (x tree)
  (cond ((eql x tree) x)
        ((atom tree) nil)
        ((find-anywhere x (first tree)))
        ((find-anywhere x (rest tree)))))

(defun length=1 (x) (and (consp x) (null (rest x))))

(defun reuse-cons (x y x-y)
  "Return (cons x y), or just x-y if it is equal to (cons x y)."
  (if (and (eql x (car x-y)) (eql y (cdr x-y)))
      x-y
      (cons x y)))


(defun get-binding (var bindings)
  (assoc var bindings))

(defun binding-val (binding)
  (cdr binding))

(defun extend-bindings (var val bindings)
  (cons (cons var val)
        (if (eq bindings no-bindings)
            nil
            bindings)))






