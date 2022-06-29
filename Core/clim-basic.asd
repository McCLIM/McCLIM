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
  ((:module "system"
    :components ((:file "utilities")
                 (:file "macros")
                 (:file "multiprocessing")
                 (:file "resources")
                 (:file "setf-star")))
   (:module "protocols"
    :depends-on ("system")
    :components ((:file "declarations")
                 (:file "protocol-classes")))
   (:module "geometry"
    :pathname "clim-basic/geometry/"
    :depends-on ("system" "protocols")
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
    :pathname "clim-basic/windowing"
    :depends-on ("system" "protocols" "geometry")
    :components ((:file "events")
                 (:file "output")
                 (:file "sheets")
                 (:file "pixmaps" :depends-on ("output"))
                 (:file "mirrors" :depends-on ("sheets"))
                 (:file "ports"   :depends-on ("sheets" "events"))
                 (:file "input"   :depends-on ("sheets" "ports"))
                 (:file "grafts"  :depends-on ("sheets" "ports"))
                 (:file "repaint" :depends-on ("sheets" "ports" "grafts" "events" "output"))))
   (:module "drawing"
    :pathname "clim-basic/drawing"
    :depends-on ("system" "protocols" "geometry" "windowing")
    :components ((:file "design")
                 (:file "text-style")
                 (:file "colors"   :depends-on ("design"))
                 (:file "pattern"  :depends-on ("design"))
                 (:file "medium"   :depends-on ("design" "text-style" "colors"))
                 (:file "graphics" :depends-on ("design" "text-style" "medium"))
                 (:file "with-output-to-drawing-stream")))
   (:module "extended-streams"
    :pathname "clim-basic/extended-streams"
    :depends-on ("system" "protocols" "geometry" "windowing" "drawing")
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
