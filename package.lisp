(defpackage :prolog
  (:use :cl)
  (:export :<-
	   :?-
	   :clear-db

	   :top-level-query/0
	   :show-prolog-vars/2
	   :dowith-prolog-vars/1
	   :with-inference 

	   :read/1
	   :write/1
	   :call/1
	   :var/1
	   :nl/0

	   :true
	   :fail
	   
	   :bagof/3
	   :setof/3
	   :not/1
	   :=/2
	   :==/2
	   :lisp/2
	   :is/2
	   :!
	   :?
	   :numberp/1
	   :repeat/0
	   :atom/1

	   :if
	   
	   :>/2
	   :>=/2
	   :</2
	   :<=/2
	   
	   :and
	   :or
	   ))


