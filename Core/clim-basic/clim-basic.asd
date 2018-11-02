(defsystem #:clim-basic
  :depends-on (#:clim-lisp
               #:spatial-trees
               (:version "flexichain" "1.5.1")
               #:bordeaux-threads
               #:trivial-garbage
               #:trivial-features
               #:babel)
  :serial t
  :components
  ((:file "setf-star")
   (:file "decls")
   (:file "protocol-classes")
   (:file "multiprocessing") ; legacy mp backends are in Lisp-Dep/mp-*.lisp
   (:file "utils")
   (:file "coordinates")
   (:file "transforms")
   (:file "design")
   (:file "regions")
   (:file "X11-colors")
   (:file "dead-keys")
   (:file "pattern")
   (:file "sheets")
   (:file "mirrors")
   (:file "pixmap")
   (:file "events")
   (:file "ports")
   (:file "grafts")
   (:file "medium")
   (:file "output")
   (:file "input")
   (:file "graphics")
   (:file "repaint")
   (:file "views")
   (:file "stream-output")
   (:file "recording")
   (:file "encapsulate")
   (:file "stream-input")
   (:file "text-selection")))
