(in-package :prolog)

(defvar *db* (make-hash-table :test #'equal))
(defvar *predicate* nil)

(defun get-pred (name category)
  "Gets predicate from the hash table *predicates*."
  (gethash (list name category) *db* nil))

(defun (setf get-pred) (val k1 k2)
  (setf (gethash (list k1 k2) *db*)
	val))

(defun clear-db ()
  (mapc #'clear-predicate *db-predicates*))
  
(defun prolog-compile (symbol &optional
		       (clauses (get-clauses symbol)))
  "Compile a symbol; make a separate function for each arity."
  (unless (null clauses)
    (let ((arity (relation-arity (clause-head (first clauses)))))
      ;; Compile the file with this arity.
      (compile-predicate
       symbol arity (clauses-with-arity clauses #'= arity))
      ;; Compile all the clauses with any other arity.
      (prolog-compile
       symbol (clauses-with-arity clauses #'/= arity)))))


(defun clauses-with-arity (clauses test arity)
  "Return all clauses whose head has given arity."
  (find-all arity clauses
	    :key #'(lambda (clause)
		     (relation-arity (clause-head clause)))
	    :test test))

(defun relation-arity (relation)
  "The number of arguments to a relation.
Example: (relation-arity '(p a b c)) => 3"
  (length (args relation)))

(defun args (x) "The arguments of a relation." (rest x))

(defun compile-predicate (symbol arity clauses)
  "Compile all the clauses for a given symbol/arity
into a single LISP function."
  (let ((predicate (make-predicate symbol arity))
	(parameters (make-parameters arity)))
    (compile
     (eval   
      `(defun ,predicate (,@parameters cont)
	 .,(maybe-add-undo-bindings
	    (mapcar #'(lambda (clause)
		       (compile-clause parameters clause 'cont))
		    clauses)))))))

(defun make-parameters (arity)
  "Return the list (?arg1 ?arg2 ... ?arg-arity)"
  (loop for i from 1 to arity
       collect (new-symbol '?arg i)))

(defun make-predicate (symbol arity)
  "Return the symbol: symbol/arity."
  (symbol-1 symbol '/ arity))

;; (defun compile-clause (parms clause cont)
;;   "Transform away the head, and compile the resulting body."
;;   (bind-unbound-vars
;;    parms
;;    (compile-body
;;     (nconc
;;      (mapcar #'make-= parms (args (clause-head clause)))
;;      (clause-body clause))
;;     cont)))

(defun maybe-add-undo-bindings (compiled-exps)
  "Undo any bindings that need undoing.
If there are any, bind the trail before we start."
  (if (length=1 compiled-exps)
      compiled-exps
      `((let ((old-trail (fill-pointer *trail*)))
	  ,(first compiled-exps)
	  ,@(loop for exp in (rest compiled-exps)
               collect '(undo-bindings! old-trail)
               collect exp)))))

(defun bind-unbound-vars (parameters exp)
  "If there are any variables in exp (besides the parameters)
then bind them to new vars"
  (let ((exp-vars (set-difference (delete '? (variables-in exp))
				  parameters)))
    (if exp-vars
	`(let ,(mapcar #'(lambda (var) `(,var (?)))
		       exp-vars)
	   ,exp)
	exp)))

(defun make-= (x y) `(= ,x ,y))

;; (defun compile-body (body cont)
;;   "Compile the body of a clause."
;;   (if (null body)
;;       `(funcall ,cont)
;;       (let* ((goal (first body))
;; 	     (macro (prolog-compiler-macro (predicate goal)))
;; 	     (macro-val (if macro
;; 			    (funcall macro goal (rest body) cont))))
;; 	(if (and macro (not (eq macro-val :pass)))
;; 	    macro-val
;; 	    (compile-call
;; 	     (make-predicate (predicate goal)
;; 			     (relation-arity goal))
;; 	     (mapcar #'(lambda (arg) (compile-arg arg))
;; 		     (args goal))
;; 	     (if (null (rest body))
;; 		 cont
;; 		 `#'(lambda ()
;; 		      ,(compile-body (rest body) cont))))))))


(defun compile-body (body cont bindings)
  "Compile the body of a clause."
  (cond
    ((null body)
     `(funcall ,cont))
    ((eq (first body) '!)			       ;***
     `(progn ,(compile-body (rest body) cont bindings) ;***
             (return-from ,*predicate* nil)))          ;***
    (t (let* ((goal (first body))
              (macro (prolog-compiler-macro (predicate goal)))
              (macro-val (if macro
                             (funcall macro goal (rest body)
                                      cont bindings))))
         (if (and macro (not (eq macro-val :pass)))
             macro-val
             `(,(make-predicate (predicate goal)
                                (relation-arity goal))
                ,@(mapcar #'(lambda (arg)
                              (compile-arg arg bindings))
                          (args goal))
                ,(if (null (rest body))
                     cont
                     `#'(lambda ()
                          ,(compile-body
                            (rest body) cont
                            (bind-new-variables bindings goal))))))))))


(defun compile-call (predicate args cont)
  "Compile a call to a prolog predicate"
  `(,predicate ,@args ,cont))

(defun prolog-compiler-macro (name)
  "Fetch the compiler macro for a Prolog predicate."
  ;; Note NAME is the raw name, not the name/arity.
  (get-pred name 'prolog-compiler-macro))

(defmacro def-prolog-compiler-macro (name arglist &body body)
  "Define a compiler macro for prolog"
  `(setf (get-pred ',name 'prolog-compiler-macro)
	 #'(lambda ,arglist .,body)))

;; (def-prolog-compiler-macro = (goal body cont)
;;   (let ((args (args goal)))
;;     (if (/= (length args) 2)
;; 	:pass
;; 	`(if ,(compile-unify (first args) (second args))
;; 	     ,(compile-body body cont)))))


;; (defun compile-unify (x y)
;;   "Return code that tests if var and term unify."
;;   `(unify! ,(compile-arg x) ,(compile-arg y)))






(defun compile-unify (x y bindings)
  "Return 2 values: code to test if x and y unify,
  and a new binding list."
  (cond
    ;; Unify constants and conses:                       ; Case
    ((not (or (has-variable-p x) (has-variable-p y)))    ; 1,2
     (values (equal x y) bindings))
    ((and (consp x) (consp y))                           ; 3
     (multiple-value-bind (code1 bindings1)
         (compile-unify (first x) (first y) bindings)
       (multiple-value-bind (code2 bindings2)
           (compile-unify (rest x) (rest y) bindings1)
         (values (compile-if code1 code2) bindings2))))
    ;; Here x or y is a variable.  Pick the right one:
    ((variable-p x) (compile-unify-variable x y bindings))
    (t              (compile-unify-variable y x bindings))))


(defun compile-if (pred then-part)
  "Compile a Lisp IF form. No else-part allowed."
  (case pred
    ((t) then-part)
    ((nil) nil)
    (otherwise `(if ,pred ,then-part))))

(defun compile-unify-variable (x y bindings)
  "X is a variable, and Y may be."
  (let* ((xb (follow-binding x bindings))
         (x1 (if xb (cdr xb) x))
         (yb (if (variable-p y) (follow-binding y bindings)))
         (y1 (if yb (cdr yb) y)))
    (cond                                                 ; Case:
      ((or (eq x '?) (eq y '?)) (values t bindings))      ; 12
      ((not (and (equal x x1) (equal y y1)))              ; deref
       (compile-unify x1 y1 bindings))
      ((find-anywhere x1 y1) (values nil bindings))       ; 11
      ((consp y1)                                         ; 7,10
       (values `(unify! ,x1 ,(compile-arg y1 bindings))
               (bind-variables-in y1 bindings)))
      ((not (null xb))
       ;; i.e. x is an ?arg variable
       (if (and (variable-p y1) (null yb))
           (values 't (extend-bindings y1 x1 bindings))   ; 4
           (values `(unify! ,x1 ,(compile-arg y1 bindings))
                   (extend-bindings x1 y1 bindings))))    ; 5,6
      ((not (null yb))
       (compile-unify-variable y1 x1 bindings))
      (t (values 't (extend-bindings x1 y1 bindings)))))) ; 8,9

(defun bind-variables-in (exp bindings)
  "Bind all variables in exp to themselves, and add that to
  bindings (except for variables already bound)."
  (dolist (var (variables-in exp))
    (unless (get-binding var bindings)
      (setf bindings (extend-bindings var var bindings))))
  bindings)

(defun follow-binding (var bindings)
  "Get the ultimate binding of var according to bindings."
  (let ((b (get-binding var bindings)))
    (if (eq (car b) (cdr b))
        b
        (or (follow-binding (cdr b) bindings)
            b))))




;; (defun compile-arg (arg)
;;   "Generate code for an argument to a goal in the body."
;;   (cond ((eq arg '?) '(?))
;; 	((variable-p arg) arg)
;; 	((not (has-variable-p arg)) `',arg)
;; 	((proper-listp arg)
;; 	 `(list .,(mapcar #'compile-arg arg)))
;; 	(t `(cons ,(compile-arg (first arg))
;; 		  ,(compile-arg (rest arg))))))


(defun compile-arg (arg bindings)
  "Generate code for an argument to a goal in the body."
  (cond ((eq arg '?) '(?))
        ((variable-p arg)
         (let ((binding (get-binding arg bindings)))
           (if (and (not (null binding))
                    (not (eq arg (binding-val binding))))
               (compile-arg (binding-val binding) bindings)
               arg)))
        ((not (find-if-anywhere #'variable-p arg)) `',arg)
        ((proper-listp arg)
         `(list .,(mapcar #'(lambda (a) (compile-arg a bindings))
                          arg)))
        (t `(cons ,(compile-arg (first arg) bindings)
                  ,(compile-arg (rest arg) bindings)))))

(defun bind-new-variables (bindings goal)
  "Extend bindings to include any unbound variables in goal."
  (let ((variables (remove-if #'(lambda (v) (assoc v bindings))
                              (variables-in goal))))
    (nconc (mapcar #'self-cons variables) bindings)))

(defun self-cons (x) (cons x x))

(def-prolog-compiler-macro = (goal body cont bindings)
  "Compile a goal which is a call to =."
  (let ((args (args goal)))
    (if (/= (length args) 2)
        :pass ;; decline to handle this goal
        (multiple-value-bind (code1 bindings1)
            (compile-unify (first args) (second args) bindings)
          (compile-if
           code1
           (compile-body body cont bindings1))))))



(def-prolog-compiler-macro and (goal body cont bindings)
  (compile-body (append (args goal) body) cont bindings))


(def-prolog-compiler-macro or (goal body cont bindings)
  (let ((disjuncts (args goal)))
    (case (length disjuncts)
      ;; not perfectly sure, maybe right
      (0 `(funcall ,cont))
      (1 (compile-body (cons (first disjuncts) body)
		       cont bindings))
      (t (let ((fn (gensym "F")))
	   `(flet ((,fn () ,(compile-body body cont bindings)))
	      .,(maybe-add-undo-bindings
		 (loop for g in disjuncts collect
		      (compile-body (list g) `#',fn
				    bindings)))))))))





(defun compile-clause (parms clause cont)
  "Transform away the head, and compile the resulting body."
  (bind-unbound-vars
   parms
   (compile-body
    (nconc
     (mapcar #'make-= parms (args (clause-head clause)))
     (clause-body clause))
    cont
    (mapcar #'self-cons parms))))                    ;***

(defun make-anonymous (exp &optional
		       (anon-vars (anonymous-variables-in exp)))
  "Replace variables that are only used once with ?."
  (cond ((consp exp)
	 (reuse-cons (make-anonymous (first exp) anon-vars)
		     (make-anonymous (rest exp) anon-vars)
		     exp))
	((member exp anon-vars) '?)
	(t exp)))

(defun anonymous-variables-in (tree)
  "Return a list of all variables that occur only once in tree."
  (let ((seen-once nil)
	(seen-more nil))
    (labels ((walk (x)
	       (cond
		 ((variable-p x)
		  (cond ((member x seen-once)
			 (setf seen-once (delete x seen-once))
			 (push x seen-more))
			((member x seen-more) nil)
			(t (push x seen-once))))
		 ((consp x)
		  (walk (first x))
		  (walk (rest x))))))
      (walk tree)
      seen-once)))
	

(defun has-variable-p (x)
  "Is ther a variable anywhere in the expression x?"
  (find-if-anywhere #'variable-p x))

(defun proper-listp (x)
  "Is x a proper (non-dotted) list?"
  (or (null x)
      (and (consp x) (proper-listp (rest x)))))

(defun get-clauses (pred)
  (get-pred pred 'clauses))

(defun predicate (relation) (first relation))

(defun clause-head (clause) (first clause))
(defun clause-body (clause) (rest clause))

(defun variable-p (x)
  "Is x a variable (a symbol beginning with '?')?"
  (and (symbolp x) (eql (char (symbol-name x) 0) #\?)))

(defun variables-in (exp)
  "Return a list of all the variables in EXP."
  (unique-find-anywhere-if #'variable-p exp))

(defun unique-find-anywhere-if (predicate tree
				&optional found-so-far)
  "Return a list of leaves of tree satisfying predicate,
with duplicates removed."
  (if (atom tree)
      (if (funcall predicate tree)
	  (adjoin tree found-so-far)
	  found-so-far)
      (unique-find-anywhere-if
       predicate
       (first tree)
       (unique-find-anywhere-if predicate (rest tree)
				found-so-far))))


