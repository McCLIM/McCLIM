(in-package #:asdf-user)

(defsystem "silex"
  :description "Silex is a library that provides a low level access to hardware."
  :license "LGPL-2.1+"
  :depends-on ("closer-mop" "bordeaux-threads" "trivial-garbage")
  :serial t
  :components
  ((:module "system"
    :components ((:file "packages")
                 (:file "utilities")
                 (:file "multiprocessing")
                 (:file "resources")
                 (:file "setf-star")
                 (:file "protocol")))
   (:module "geometry"
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
   (:module "drawing"
    :components ((:file "design")
                 (:file "text-style")
                 (:file "pattern"  :depends-on ("design"))
                 (:file "medium"   :depends-on ("design" "text-style"))
                 (:file "graphics" :depends-on ("design" "text-style" "medium"))))
   (:module "windowing"
    :components ((:file "events")
                 (:file "output")
                 (:file "pointer")
                 (:file "sheets")
                 (:file "pixmaps" :depends-on ("output"))
                 (:file "mirrors" :depends-on ("sheets"))
                 (:file "ports"   :depends-on ("sheets" "events"))
                 (:file "fonts"   :depends-on ("ports"))
                 (:file "input"   :depends-on ("sheets" "ports"))
                 (:file "grafts"  :depends-on ("sheets" "ports"))
                 (:file "repaint" :depends-on ("sheets" "ports" "grafts" "events" "output"))
                 (:file "with-output-to-drawing-stream" :depends-on ("ports"))))))
