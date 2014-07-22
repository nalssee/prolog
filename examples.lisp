(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :prolog))

(defpackage :examples
  (:use :cl :prolog))

(in-package :examples)

(declaim #+sbcl(sb-ext:muffle-conditions style-warning))

(clear-db)

(<- (member ?item (?item . ?rest)))

(<- (member ?item (?x . ?rest))
    (member ?item ?rest))
;; (?- (member ?x (1 2 3 4)))
(<- (nextto ?x ?y ?list)
    (iright ?x ?y ?list))
(<- (nextto ?x ?y ?list)
    (iright ?y ?x ?list))

(<- (iright ?left ?right (?left ?right . ?rest)))
(<- (iright ?left ?right (?x . ?rest))
    (iright ?left ?right ?rest))

(<- (= ?x ?x))
(<- (zebra ?h ?w ?z)
    (= ?h ((house norwegian ? ? ? ?)
	   ?
	   (house ? ? ? milk ?) ? ?))
    (member (house englishman ? ? ? red) ?h)
    (member (house spaniard dog ? ? ?) ?h)
    (member (house ? ? ? coffee green) ?h)
    (member (house ukrainian ? ? tea ?) ?h)
    
    (iright (house ? ? ? ? ivory)
	    (house ? ? ? ? green) ?h)
    
    (member (house ? snails winston ? ?) ?h)
    (member (house ? ? kools ? yellow) ?h)

    (nextto (house ? ? chesterfield ? ?)
	    (house ? fox ? ? ?) ?h)
    (nextto (house ? ? kools ? ?)
	    (house ? horse ? ? ?) ?h)
    
    (member (house ? ? luckystrike orange-juice ?) ?h)
    (member (house japanese ? parliaments ? ?) ?h)
    
    (nextto (house norwegian ? ? ? ?)
	    (house ? ? ? ? blue) ?h)
    
    (member (house ?w ? ? water ?) ?h)
    (member (house ?z zebra ? ? ?) ?h))

;; (time (?- (zebra ?houses ?water-drinker ?zebra-owner)))
;; (?- (member ?x (a b c)) (not (= ?x b)))

(<- (likes Kim Robin))
(<- (likes Sandy Lee))
(<- (likes Sandy Kim))
(<- (likes Robin cats))
(<- (likes Sandy ?x) (likes ?x cats))
(<- (likes Kim ?x) (likes ?x Lee) (likes ?x Kim))
(<- (likes ?x ?x))

;; (?- (likes ?x kim))
;; (?- (bagof ?who (likes Sandy ?who) ?bag))

(<- (fib 1 1))
(<- (fib 0 0))
(<- (fib ?n ?v)
    (> ?n 0)
    (is ?n1 (- ?n 1))
    (fib ?n1 ?v1)
    (is ?n2 (- ?n 2))
    (fib ?n2 ?v2)
    (is ?v (+ ?v1 ?v2)))

(<- (small ?x) (or (member ?x (1 2 3))
		   (member ?x (2 3 4))
		   (member ?x (3 4 5))))

(<- (foo ?x) (and (small ?x) (member ?x (2 1))))

(with-inference (zebra ?x ?y ?z)
  (format t "~%~A ~A ~A" ?x ?y ?z))

(with-inference (fib 10 ?x)
  (format t "~%fib 10: ~A" ?x))



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar microshaft-data-base
    '(
      ;; from section 4.4.1
      (address (Bitdiddle Ben) (Slumerville (Ridge Road) 10))
      (job (Bitdiddle Ben) (computer wizard))
      (salary (Bitdiddle Ben) 60000)
      (address (Hacker Alyssa P) (Cambridge (Mass Ave) 78))
      (job (Hacker Alyssa P) (computer programmer))
      (salary (Hacker Alyssa P) 40000)
      (supervisor (Hacker Alyssa P) (Bitdiddle Ben))
      (address (Fect Cy D) (Cambridge (Ames Street) 3))
      (job (Fect Cy D) (computer programmer))
      (salary (Fect Cy D) 35000)
      (supervisor (Fect Cy D) (Bitdiddle Ben))
      (address (Tweakit Lem E) (Boston (Bay State Road) 22))
      (job (Tweakit Lem E) (computer technician))
      (salary (Tweakit Lem E) 25000)
      (supervisor (Tweakit Lem E) (Bitdiddle Ben))
      (address (Reasoner Louis) (Slumerville (Pine Tree Road) 80))
      (job (Reasoner Louis) (computer programmer trainee))
      (salary (Reasoner Louis) 30000)
      (supervisor (Reasoner Louis) (Hacker Alyssa P))
      (supervisor (Bitdiddle Ben) (Warbucks Oliver))
      (address (Warbucks Oliver) (Swellesley (Top Heap Road)))
      (job (Warbucks Oliver) (administration big wheel))
      (salary (Warbucks Oliver) 150000)
      (address (Scrooge Eben) (Weston (Shady Lane) 10))
      (job (Scrooge Eben) (accounting chief accountant))
      (salary (Scrooge Eben) 75000)
      (supervisor (Scrooge Eben) (Warbucks Oliver))
      (address (Cratchet Robert) (Allston (N Harvard Street) 16))
      (job (Cratchet Robert) (accounting scrivener))
      (salary (Cratchet Robert) 18000)
      (supervisor (Cratchet Robert) (Scrooge Eben))
      (address (Aull DeWitt) (Slumerville (Onion Square) 5))
      (job (Aull DeWitt) (administration secretary))
      (salary (Aull DeWitt) 25000)
      (supervisor (Aull DeWitt) (Warbucks Oliver))
      (can-do-job (computer wizard) (computer programmer))
      (can-do-job (computer wizard) (computer technician))
      (can-do-job (computer programmer) (computer programmer trainee))
      (can-do-job (administration secretary) (administration big wheel))
      (rule (lives-near ?person-1 ?person-2) (and (address ?person-1 (?town . ?rest-1))
					      (address ?person-2 (?town . ?rest-2))
					      (not (same ?person-1 ?person-2))))
      (rule (same ?x ?x))
      (rule (wheel ?person) (and (supervisor ?middle-manager ?person)
			     (supervisor ?x ?middle-manager)))
      (rule (outranked-by ?staff-person ?boss)
       (or (supervisor ?staff-person ?boss)
	(and (supervisor ?staff-person ?middle-manager)
	     (outranked-by ?middle-manager ?boss)))))))

(defmacro insert-microshaft-data-base ()
  `(progn
     ,@(mapcar (lambda (x)
		 (if (eq (car x) 'rule)
		     `(<- ,@(rest x))
		     `(<- ,x)))
	      microshaft-data-base)))

(insert-microshaft-data-base)

(with-inference (job ?x (computer . ?type))
  (format t "~% ~A ~A" ?x ?type))
(with-inference (and (job ?person (computer programmer))
		     (address ?person ?where))
  (format t "~% person: ~A where: ~A" ?person ?where))

(with-inference (and (salary ?person ?amount)
		     (> ?amount 30000))
  (format t "~% person ~A amount: ~A" ?person ?amount))

(with-inference (outranked-by ?x ?y)
  (format t "~% ~A ~A" ?x ?y))

(with-inference (and (lives-near ?x ?y) (outranked-by ?x ?y))
  (format t "~A and ~A's boss ~A lives close together" (second ?x) (second ?x) (second ?y)))


(<- (max ?x ?y ?x) (>= ?x ?y) !)
(<- (max ?x ?y ?y))

(with-inference (max 30 190 ?x)
  (print ?x))


