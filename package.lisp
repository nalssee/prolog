(defpackage :prolog
  (:use :cl)
  (:export :<-
	   :?-
	   :clear-db

	   :top-level-query/0
	   :show-prolog-vars/2
	   :dowith-prolog-vars/1
	   :with-inference 
	   
	   ;; :deref-exp
	   :bagof/3
	   :not/1
	   :=/2
	   :==/2
	   :lisp/2
	   :is/2
	   :!
	   :?
	   :numberp/1
	   
	   :>/2
	   :>=/2
	   :</2
	   :<=/2
	   
	   :and
	   :or
	   ))


