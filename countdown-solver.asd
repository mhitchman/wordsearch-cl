(asdf:defsystem #:countdown-solver
  :serial t
  :description "Finds words that are subsets of a set of 9 provided letters"
  :author "Matthew Hitchman"
  :license "GPLv3"
  :components ((:file "packages")
	       (:file "solver"))
  :build-operation "asdf:program-op"
  :build-pathname "countdown-solver"
  :entry-point "ctdnslvr:start")
