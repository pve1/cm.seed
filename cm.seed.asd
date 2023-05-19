(asdf:defsystem :cm.seed
  :description "Provides assignment (<-) and return (^) operators."
  :author "Peter von Etter"
  :license "LGPL-3.0"
  :version "0.0.1"
  :serial t
  :components ((:file "cm.seed"))
  :depends-on (#:package.seed
               #:tree-walking.seed
               #:alexandria))
