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

(with-inference (zebra ?z ?x ?y)
  (format t "~%~A ~A ~A" ?x ?y ?z))

(with-inference (fib 10 ?x)
  (format t "~%fib 10: ~A" ?x))

