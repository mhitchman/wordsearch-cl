(asdf:defsystem #:wordsearcher
  :serial t
  :description "Finds words that are subsets of a set of 9 provided letters"
  :author "Matthew Hitchman"
  :components ((:file "packages")
	       (:file "solver"))
  :build-operation "asdf:program-op"
  :build-pathname "build/wordsearcher"
  :entry-point "wordsearcher:start")
