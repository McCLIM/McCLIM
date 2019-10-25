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
   (:module "drawing"
    :depends-on ("utils" "decls" "protocol-classes" "geometry")
    :components ((:file "design")
                 (:file "colors"   :depends-on ("design"))
                 (:file "pattern"  :depends-on ("design"))
                 (:file "medium"   :depends-on ("design" "colors"))
                 (:file "graphics" :depends-on ("design" "medium"))))
   (:file "views" :depends-on ("utils" "protocol-classes"))
   (:file "text-formatting" :depends-on ("utils" "protocol-classes"))
   (:file "stream-output" :depends-on ("decls" "protocol-classes" "multiprocessing" "utils" "views" "windowing" "geometry" "drawing" "setf-star"))
   (:file "recording" :depends-on ("decls" "protocol-classes" "multiprocessing" "windowing" "drawing" "utils" "stream-output"))
   (:file "encapsulate" :depends-on ("decls" "protocol-classes" "multiprocessing" "windowing" "drawing" "utils" "stream-output" "recording"))
   (:file "stream-input" :depends-on ("decls" "protocol-classes" "multiprocessing" "windowing" "encapsulate" "utils" "dead-keys"))
   (:file "pointer-tracking" :depends-on ("decls" "protocol-classes" "windowing" "stream-output" "stream-input"))
   (:file "text-selection" :depends-on ("decls" "protocol-classes" "multiprocessing" "drawing" "windowing" "stream-output" "recording" "geometry"))))
