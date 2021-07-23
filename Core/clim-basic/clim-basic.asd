(in-package #:asdf-user)

(defsystem "clim-basic"
  :depends-on ("clim-lisp"
               "alexandria"
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
   (:file "multiprocessing" :depends-on ("decls"))
   (:file "utils" :depends-on ("decls" "multiprocessing"))
   (:module "geometry"
    :depends-on ("decls" "protocol-classes" "multiprocessing" "utils" "setf-star")
    :serial t
    :components ((:file "coordinates")
                 (:file "transforms")
                 (:file "bounding-rectangle")
                 (:file "regions")
                 (:file "3.2.1-points")
                 (:file "3.2.2-polygons-and-polylines")
                 (:file "3.2.3-lines")
                 (:file "3.2.4-rectangles")
                 (:file "3.2.5-ellipses-and-elliptical-arcs")
                 (:file "3.2.6-bezigons-and-bezier-curves")
                 (:file "region-utilities")
                 (:file "region-predicates")
                 (:file "region-composition")
                 (:file "region-set-composition")))
   (:module "windowing"
    :depends-on ("utils" "decls" "protocol-classes" "multiprocessing" "geometry")
    :components ((:file "events")
                 (:file "output")
                 (:file "sheets")
                 (:file "mirrors" :depends-on ("sheets"))
                 (:file "ports"   :depends-on ("sheets" "events"))
                 (:file "input"   :depends-on ("sheets" "ports"))
                 (:file "grafts"  :depends-on ("sheets" "ports"))
                 (:file "repaint" :depends-on ("sheets" "ports" "grafts" "events" "output"))))
   (:module "drawing"
    :depends-on ("utils" "decls" "protocol-classes" "geometry" "windowing")
    :components ((:file "design")
                 (:file "text-style")
                 (:file "colors"   :depends-on ("design"))
                 (:file "pattern"  :depends-on ("design"))
                 (:file "medium"   :depends-on ("design" "text-style" "colors"))
                 (:file "graphics" :depends-on ("design" "text-style" "medium"))))
   (:module "extended-streams"
    :depends-on ("setf-star" "decls" "utils" "protocol-classes" "multiprocessing" "geometry" "windowing" "drawing")
    :components ((:file "text-formatting") ; standard-page-layout
                 (:file "views")           ; stream-default-view
                 (:file "dead-keys")       ; dead-key merging
                 (:file "stream-output"     :depends-on ("text-formatting" "views"))
                 (:file "recording"         :depends-on ("stream-output"))
                 (:file "text-selection"    :depends-on ("recording"))
                 (:file "encapsulate"       :depends-on ("stream-output" "recording"))
                 (:file "stream-input"      :depends-on ("encapsulate" "dead-keys"))
                 (:file "gestures")
                 (:file "standard-gestures" :depends-on ("gestures"))
                 (:file "pointer-tracking"  :depends-on ("stream-output" "stream-input"))))))
