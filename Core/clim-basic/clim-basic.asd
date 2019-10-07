(defsystem #:clim-basic
  :depends-on ("clim-lisp"
               "spatial-trees"
               (:version "flexichain" "1.5.1")
               "bordeaux-threads"
               "trivial-garbage"
               "trivial-features"
               "babel")
  :components
  ((:file "setf-star")
   (:file "decls" :depends-on ("setf-star"))
   (:file "protocol-classes" :depends-on ("decls"))
   (:file "multiprocessing" :depends-on ("decls")) ; legacy mp backends are in Lisp-Dep/mp-*.lisp
   (:file "utils" :depends-on ("decls" "multiprocessing"))
   (:file "X11-colors" :depends-on ("decls" "protocol-classes" "multiprocessing" "design" "geometry"))
   (:file "design" :depends-on ("decls" "protocol-classes" "utils"))
   (:file "dead-keys" :depends-on ("decls"))
   (:module "geometry"
    :depends-on ("decls" "protocol-classes" "multiprocessing" "utils" "setf-star")
    :serial t
    :components ((:file "coordinates")
                 (:file "transforms")
                 (:file "regions")
                 (:file "bounding-rectangle")
                 (:file "region-bounding-rectangles")
                 (:file "region-transformations")
                 (:file "region-predicates")
                 (:file "region-composition")))
   (:file "pattern" :depends-on ("decls" "protocol-classes" "utils" "design"))
   (:file "sheets" :depends-on ("decls" "protocol-classes" "multiprocessing" "utils" "geometry"))
   (:file "mirrors" :depends-on ("sheets"))
   (:file "pixmap" :depends-on ("decls" "protocol-classes" "multiprocessing" "sheets" "geometry"))
   (:file "events" :depends-on ("decls" "protocol-classes" "multiprocessing" "sheets" "utils"))
   (:file "ports" :depends-on ("decls" "protocol-classes" "multiprocessing" "events" "sheets" "pixmap" "utils"))
   (:file "grafts" :depends-on ("decls" "protocol-classes" "multiprocessing" "sheets" "ports" "geometry"))
   (:file "medium" :depends-on ("decls" "protocol-classes" "multiprocessing" "ports" "X11-colors" "utils" "pixmap" "geometry" "design"))
   (:file "output" :depends-on ("decls" "protocol-classes" "multiprocessing" "medium"))
   (:file "input" :depends-on ("decls" "protocol-classes" "multiprocessing" "events" "geometry" "sheets"))
   (:file "repaint" :depends-on ("decls" "protocol-classes" "multiprocessing" "sheets" "events" "graphics"))
   (:file "graphics" :depends-on ("decls" "protocol-classes" "multiprocessing" "output" "utils" "medium" "sheets" "pixmap" "geometry" "design"))
   (:file "views" :depends-on ("utils" "protocol-classes"))
   (:file "text-formatting" :depends-on ("utils" "protocol-classes"))
   (:file "stream-output" :depends-on ("decls" "protocol-classes" "multiprocessing" "design" "utils" "X11-colors" "views" "output" "sheets" "geometry" "graphics" "medium" "setf-star"))
   (:file "recording" :depends-on ("decls" "protocol-classes" "multiprocessing" "output" "graphics" "design" "medium" "geometry" "sheets" "utils" "stream-output"))
   (:file "encapsulate" :depends-on ("decls" "protocol-classes" "multiprocessing" "sheets" "graphics" "utils" "medium" "input" "stream-output" "recording"))
   (:file "stream-input" :depends-on ("decls" "protocol-classes" "multiprocessing" "input" "ports" "sheets" "events" "encapsulate" "utils" "dead-keys"))
   (:file "pointer-tracking" :depends-on ("decls" "protocol-classes" "input" "ports" "sheets" "events" "stream-output" "stream-input"))
   (:file "text-selection" :depends-on ("decls" "protocol-classes" "multiprocessing" "X11-colors" "medium" "output" "sheets" "stream-output" "ports" "recording" "geometry" "events"))))
