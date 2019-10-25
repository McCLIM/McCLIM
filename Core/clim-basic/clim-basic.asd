(in-package #:asdf-user)

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
   (:module "windowing"
    :depends-on ("utils" "decls" "protocol-classes" "multiprocessing" "geometry")
    :components ((:file "events")
                 (:file "output")
                 (:file "pixmap"  :depends-on ("output"))
                 (:file "sheets")
                 (:file "mirrors" :depends-on ("sheets"))
                 (:file "ports"   :depends-on ("sheets" "pixmap" "events"))
                 (:file "input"   :depends-on ("sheets" "ports"))
                 (:file "grafts"  :depends-on ("sheets" "ports"))
                 (:file "repaint" :depends-on ("sheets" "ports" "grafts" "events" "output"))))
   (:file "design" :depends-on ("decls" "protocol-classes" "utils" "geometry"))
   (:file "X11-colors" :depends-on ("decls" "protocol-classes" "multiprocessing" "design" "geometry"))
   (:file "pattern" :depends-on ("decls" "protocol-classes" "utils" "design"))
   (:file "medium" :depends-on ("decls" "protocol-classes" "multiprocessing" "X11-colors" "utils" "windowing" "geometry" "design"))
   (:file "graphics" :depends-on ("decls" "protocol-classes" "multiprocessing" "utils" "medium" "windowing" "geometry" "design"))
   (:file "views" :depends-on ("utils" "protocol-classes"))
   (:file "text-formatting" :depends-on ("utils" "protocol-classes"))
   (:file "stream-output" :depends-on ("decls" "protocol-classes" "multiprocessing" "design" "utils" "X11-colors" "views" "windowing" "geometry" "graphics" "medium" "setf-star"))
   (:file "recording" :depends-on ("decls" "protocol-classes" "multiprocessing" "windowing" "graphics" "design" "medium" "geometry" "utils" "stream-output"))
   (:file "encapsulate" :depends-on ("decls" "protocol-classes" "multiprocessing" "windowing" "graphics" "utils" "medium" "stream-output" "recording"))
   (:file "stream-input" :depends-on ("decls" "protocol-classes" "multiprocessing" "windowing" "encapsulate" "utils" "dead-keys"))
   (:file "pointer-tracking" :depends-on ("decls" "protocol-classes" "windowing" "stream-output" "stream-input"))
   (:file "text-selection" :depends-on ("decls" "protocol-classes" "multiprocessing" "X11-colors" "medium" "windowing" "stream-output" "recording" "geometry"))))
