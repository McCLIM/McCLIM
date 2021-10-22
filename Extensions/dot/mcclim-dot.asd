(in-package #:asdf-user)

(defsystem "mcclim-dot"
  :license "MIT"
  :description "Support for using DOT based graph layout engines."
  :depends-on ("mcclim-dot/core" "mcclim-dot/graphviz")
  :in-order-to ((test-op (test-op "mcclim-dot/test"))))

(defsystem "mcclim-dot/core"
  :license "MIT"
  :description "Core DOT routines."
  :depends-on ("mcclim" "mcclim-bezier" "cl-dot" "alexandria" "split-sequence" "parse-number"
               "closer-mop")
  :components ((:file "package")
               (:file "core" :depends-on ("package"))))

(defsystem "mcclim-dot/graphviz"
  :license "MIT"
  :description "Interface to graphviz via an external process."
  :depends-on ("mcclim-dot/core" "jsown" "uiop")
  :components ((:file "graphviz")))

(defsystem "mcclim-dot/test"
  :license "MIT"
  :description "Test suite for mcclim-dot."
  :depends-on ("mcclim-dot/core"
               "mcclim-dot/graphviz"
               "mcclim-raster-image"
               "fiveam")
  :pathname "test"
  :components ((:file "package")
               (:file "smoke" :depends-on ("package"))
               (:file "edge-labels" :depends-on ("package")))
  :perform (test-op (operation component)
             (uiop:symbol-call '#:mcclim-dot.test '#:run-tests)))
