(in-package :prolog)

(defvar *uncompiled* nil
  "Prolog symbols that have not been compiled.")

(defvar *db-predicates* nil
  "A list of all predicates stored in the database.")

(defmacro <- (&rest clause)
  "Add a clause to the date base."
  `(add-clause ',(make-anonymous clause)))

(defmacro ?- (&rest goals)
  "Make a query and print answers."
  ;; fixed, no one wants to see the values of anonymous vars
  ;;  `(top-level-prove ',(replace-?-vars goals))
  ;; let "top-level-prove" delete ?
  `(top-level-prove ',goals))


(defun add-clause (clause)
  "Add a clause to the data base, indexed by head's predicte."
  ;; The predicate must be a non-variable symbol.
  (let ((pred (predicate (clause-head clause))))
    (assert (and (symbolp pred) (not (variable-p pred))))
    (pushnew pred *db-predicates*)
    (pushnew pred *uncompiled*)
    (setf (get-pred pred 'clauses)
	  (nconc (get-clauses pred) (list clause)))
    pred))


(defun replace-?-vars (exp)
  "Replace any ? within exp with a var of the form ?123."
  (cond ((eq exp '?) (gensym "?"))
	((atom exp) exp)
	(t (reuse-cons (replace-?-vars (first exp))
		       (replace-?-vars (rest exp))
		       exp))))


(defun top-level-prove (goals)
  "Prove the list of goals by compiling and calling it."
  ;; First redefine top-level-query
  (clear-predicate 'top-level-query)
  (let ((vars (delete '? (variables-in goals))))
    (add-clause `((top-level-query)
		  ,@goals
		  (show-prolog-vars ,(mapcar #'symbol-name vars) ,vars))))
  ;; Now run it
  (run-prolog 'top-level-query/0 #'ignore-1)
  (format t "~&No.")
  (values))


(defmacro with-inference (goal &body body)
  (let ((vars (delete '? (variables-in goal)))
	(svars (gensym "V"))
	(cont (gensym "K")))
    `(progn
       (setf (symbol-function 'dowith-prolog-vars/1)
	     #'(lambda (,svars ,cont)
		 (let ,(loop for v in vars
			  for i from 0 collect
			    (list v `(deref-exp (nth ,i ,svars))))
		   
		   ,@body
		   (funcall ,cont))))
       (top-level-infer ',goal))))

(defun top-level-infer (goal)
  (clear-predicate 'top-level-query)
  (let ((vars (delete '? (variables-in goal))))
    (add-clause `((top-level-query)
		  ,goal
		  (dowith-prolog-vars ,vars))))
  (run-prolog 'top-level-query/0 #'ignore-1) ; 'ignore' in PAIP
  (values))

;; Just a place holder
(defun dowith-prolog-vars/1 (vars cont)
  (declare (ignore vars))
  (funcall cont))




(defun show-prolog-vars/2 (var-names vars cont)
  "Display the variables, and prompt the user to see
if we should continue. If not, return to the top level."
  (if (null vars)
      (format t "~&Yes")
      (loop for name in var-names
	 for var in vars do
           (format t "~&~a = ~a" name (deref-exp var))))
  (if (continue-p)
      (funcall cont)
      (throw 'top-level-prove nil)))



(defun run-prolog (procedure cont)
  "Run a 0-ary prolog procedure with a given continuation."
  ;; First compile anything else that needs it
  (prolog-compile-symbols)
  ;; Reset the trail and the new variable counter
  (setf (fill-pointer *trail*) 0)
  (setf *var-counter* 0)
  ;; Finally, call the query.
  (catch 'top-level-prove
    (funcall procedure cont)))


(defun prolog-compile-symbols (&optional (symbols *uncompiled*))
  "Compile a list of Prolog symbols.
By default, the list is all symbols that need it."

  ;; To remove warning messages.
  ;; Compile top-level-query last not first.
  (mapc #'prolog-compile symbols)
  (setf *uncompiled* (set-difference *uncompiled* symbols)))


(defun ignore-1 (&rest args)
  (declare (ignore args))
  nil)





(defun deref-exp (exp)
  "Build something equivalent to EXP with variables dereferenced."
  (if (atom (deref exp))
      exp
      (reuse-cons
       (deref-exp (first exp))
       (deref-exp (rest exp))
       exp)))

(defun clear-predicate (predicate)
  (setf (get-pred predicate 'clauses) nil))

(defun continue-p ()
  "Ask user if we should continue looking for solutions."
  (case (read-char)
    (#\; t)
    (#\. nil)
    (#\newline (continue-p))
    (otherwise
     (format t " Type ; to see more or . to stop")
     (continue-p))))
