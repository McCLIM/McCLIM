(in-package #:asdf-user)

(defsystem "clim-basic"
  :depends-on ("clim-lisp" "spatial-trees" "bordeaux-threads" "trivial-garbage")
  :pathname "clim-basic"
  :components
  ((:module "system"
    :components ((:file "utilities")
                 (:file "protocol")
                 (:file "multiprocessing")
                 (:file "resources")
                 (:file "setf-star")
                 (:file "encapsulating-streams" :depends-on ("protocol"))))
   (:module "geometry"
    :depends-on ("system")
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
    :depends-on ("system" "geometry")
    :components ((:file "design")
                 (:file "text-style")
                 (:file "colors"   :depends-on ("design"))
                 (:file "pattern"  :depends-on ("design"))
                 (:file "medium"   :depends-on ("design" "text-style" "colors"))
                 (:file "graphics" :depends-on ("design" "text-style" "medium"))))
   (:module "windowing"
    :depends-on ("system" "geometry" "drawing")
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
                 (:file "with-output-to-drawing-stream" :depends-on ("ports"))))
   (:module "extended-output"
    :depends-on ("system" "geometry" "drawing")
    :components ((:file "protocol")
                 (:file "extra-colors")
                 (:file "utilities"        :depends-on ("protocol"))
                 (:file "views"            :depends-on ("protocol"))
                 (:file "text-formatting"  :depends-on ("protocol" "utilities"))
                 (:file "stream-output"    :depends-on ("protocol" "utilities" "views" "text-formatting"))
                 (:file "recording"        :depends-on ("protocol" "text-formatting"))
                 (:file "graph-formatting" :depends-on ("protocol" "stream-output" "recording"))
                 (:file "bordered-output"  :depends-on ("utilities" "extra-colors" "protocol" "recording"))
                 (:file "table-formatting" :depends-on ("utilities" "protocol" "recording"))
                 (:module "incremental-redisplay"
                  :serial t
                  :components ((:file "cache")
                               (:file "updating-stream")
                               (:file "updating-record")
                               (:file "redisplay")
                               (:file "propagate")))))))
