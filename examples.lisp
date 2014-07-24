(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :prolog))

(defpackage :examples
  (:use :cl :prolog))

(in-package :examples)

;; (declaim #+sbcl(sb-ext:muffle-conditions style-warning))
;; (declaim #+sbcl(sb-ext:muffle-conditions sb-kernel:redefinition-warning))

(clear-db)

;; =====================================
;; Zebra Puzzle
;; =====================================
(<- (member ?item (?item . ?rest)))
(<- (member ?item (?x . ?rest))
    (member ?item ?rest))

(<- (nextto ?x ?y ?list)
    (iright ?x ?y ?list))
(<- (nextto ?x ?y ?list)
    (iright ?y ?x ?list))

(<- (iright ?left ?right (?left ?right . ?rest)))
(<- (iright ?left ?right (?x . ?rest))
    (iright ?left ?right ?rest))


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

(with-inference (zebra ? ?y ?z)
  (format t "~%~A ~A" ?y ?z))

;; =====================================
;; Recursive version Factorial
;; =====================================
;; (<- (factorial 0 1))
;; (<- (factorial ?n ?f)
;;     (> ?n 0)
;;     (is ?n1 (- ?n 1))
;;     (factorial ?n1 ?f1)
;;     (is ?f (* ?n ?f1)))

(<- (factorial 0 1))
(<- (factorial ?n ?v)
    (factorial ?n 1 ?v))

(<- (factorial 0 ?f ?f))
(<- (factorial ?n ?a ?f)
    (> ?n 0)
    (is ?a1 (* ?n ?a))
    (is ?n1 (- ?n 1))
    (factorial ?n1 ?a1 ?f))


(with-inference (factorial 10 ?x)
  (format t "~%~A~%" ?x))


;; recursive version
;; (<- (fib 1 1))
;; (<- (fib 0 0))
;; (<- (fib ?n ?v)
;;     (> ?n 0)
;;     (is ?n1 (- ?n 1))
;;     (fib ?n1 ?v1)
;;     (is ?n2 (- ?n 2))
;;     (fib ?n2 ?v2)
;;     (is ?v (+ ?v1 ?v2)))

(<- (fib 0 0))
(<- (fib ?x ?y)
    (> ?x 0)
    (fib ?x ?y ?))

(<- (fib 1 1 0))
(<- (fib ?x ?y1 ?y2)
    (> ?x 1)
    (is ?x1 (- ?x 1))
    (fib ?x1 ?y2 ?y3)
    (is ?y1 (+ ?y2 ?y3)))

(with-inference (fib 100 ?x)
  (format t "~%fib 100: ~A" ?x))

;; ====================================
;; Chess queens puzzle
;; ====================================
(<- (queens ?p)
    (perm (1 2 3 4 5 6 7 8) ?p)
    (combine (1 2 3 4 5 6 7 8) ?p ?s ?d)
    (all_diff ?s)
    (all_diff ?d))
(<- (combine (?x1 . ?x) (?y1 . ?y) (?s1 . ?s) (?d1 . ?d))
    (is ?s1 (+ ?x1 ?y1))
    (is ?d1 (- ?x1 ?y1))
    (combine ?x ?y ?s ?d))
(<- (combine () () () ()))

(<- (all_diff (?x . ?y))
    (not (member ?x ?y))
    (all_diff ?y))
(<- (all_diff (?x)))

(<- (takeout ?x (?x . ?r) ?r))
(<- (takeout ?x (?f . ?r) (?f . ?s))
    (takeout ?x ?r ?s))


(<- (perm (?x . ?y) ?z)
    (perm ?y ?w)
    (takeout ?x ?z ?w))
(<- (perm () ()))

(<- (length () 0))
(<- (length (? . ?y) ?l)
    (length ?y ?l1)
    (is ?l (+ ?l1 1)))

;; (with-inference (and (setof ?p (queens ?x) ?set) (length ?set ?l))
;;   (declare (ignore ?x ?p ?set))
;;   (format t "~%~A" ?l))
(with-inference (and (queens (?x . ?y)) (< ?x 4) !)
  (format t "~% ~A" (cons ?x ?y)))

(with-inference (perm (1 2 3) ?p)
  (format t "~%~A" ?p))





;; =================================
;; Towers of Hanoi Puzzle
;; =================================

(<- (move 1 ?x ?y ?)
    (nl)
    (write "Move top disk from ")
    (write ?x)
    (write " to ")
    (write ?y)
)
(<- (move ?n ?x ?y ?z)
    (> ?n 1)
    (is ?M (- ?n 1))
    (move ?m ?x ?z ?y)
    (move 1 ?x ?y ?)
    (move ?m ?z ?y ?x))

(with-inference (move 3 left right center)
  (values))




(<- (rev () ()))
(<- (rev (?x . ?a) ?b)
    (rev ?a ?c) 
    (append ?c (?x) ?b))

(<- (append () ?ys ?ys))
(<- (append (?x . ?xs) ?ys (?x . ?zs))
    (append ?xs ?ys ?zs))

(<- (quicksort (?x . ?xs) ?ys)
    (partition ?xs ?x ?littles ?bigs)
    (quicksort ?littles ?ls)
    (quicksort ?bigs ?bs)
    (append ?ls (?x . ?bs) ?ys))
(<- (quicksort () ()))

(<- (partition (?x . ?xs) ?y (?x . ?ls) ?bs)
    (<= ?x ?y)
    (partition ?xs ?y ?ls ?bs))
(<- (partition (?x . ?xs) ?y ?ls (?x . ?bs))
    (> ?x ?y)
    (partition ?xs ?y ?ls ?bs))
(<- (partition () ?y () ()))
(<- (union (?x . ?y) ?z ?w)
    (member ?x ?z) !
    (union ?y ?z ?w))
(<- (union (?x . ?y) ?z (?x . ?w))
    (union ?y ?z ?w))
(<- (union () ?z ?z))

(with-inference (union (1 2 3) (2 8 3 10) ?z)
  (format t "~%union ~A" ?z))

  






(with-inference (quicksort (9 1 2 9 10 -2) ?x)
  (format t "~%~A" ?x))




  
;; ==================================
;; cut
;; ==================================
(<- (max ?x ?y ?x) (>= ?x ?y) !)
(<- (max ?x ?y ?y))


(with-inference (max 30 190 ?x)
  (print ?x))


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
      (rule
       (lives-near ?person-1 ?person-2)
       (and
	(address ?person-1 (?town . ?rest-1))
	(address ?person-2 (?town . ?rest-2))
	(not (same ?person-1 ?person-2))))
      (rule
       (same ?x ?x))
      (rule
       (wheel ?person)
       (and
	(supervisor ?middle-manager ?person)
	(supervisor ?x ?middle-manager)))
      (rule
       (outranked-by ?staff-person ?boss)
       (or
	(supervisor ?staff-person ?boss)
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



(with-inference (and (salary ?person ?amount)
		     (> ?amount 60000))
  (format t "~% person ~A amount: ~A" ?person ?amount))



