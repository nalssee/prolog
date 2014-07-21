(asdf:defsystem :prolog
  :name "prolog"
  :components ((:file "package")
	       (:file "interface"
		      :depends-on ("package"
				   "unify"))
	       (:file "aux"
		      :depends-on ("package"))
	       (:file "unify"
		      :depends-on ("package"))
	       (:file "compile"
		      :depends-on ("package"
				   "interface"
				   "unify"
				   "aux"))
               (:file "predicates"
                      :depends-on ("package"
                                   "unify"
                                   "compile"))
               
               ))

