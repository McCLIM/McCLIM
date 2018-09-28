(defsystem #:clim-basic
  :depends-on (#:clim-lisp
               #:spatial-trees
               (:version "flexichain" "1.5.1")
               #:bordeaux-threads
               #:trivial-garbage
               #:trivial-features
               #:babel)
  :components
  ((:file "setf-star")
   (:file "decls" :depends-on ("setf-star"))
   (:file "protocol-classes" :depends-on ("decls"))
   (:file "multiprocessing" :depends-on ("decls")) ; legacy mp backends are in Lisp-Dep/mp-*.lisp
   (:file "utils" :depends-on ("decls" "multiprocessing"))
   (:file "X11-colors" :depends-on ("decls" "protocol-classes" "multiprocessing" "design" "regions"))
   (:file "coordinates" :depends-on ("decls" "protocol-classes" "multiprocessing"))
   (:file "transforms" :depends-on ("decls" "protocol-classes" "multiprocessing" "coordinates" "utils"))
   (:file "design" :depends-on ("decls" "protocol-classes" "utils" "transforms"))
   (:file "dead-keys" :depends-on ("decls"))
   (:file "regions" :depends-on ("decls" "protocol-classes" "multiprocessing" "coordinates" "utils" "transforms" "setf-star" "design"))
   (:file "pattern" :depends-on ("decls" "protocol-classes" "utils" "design"))
   (:file "sheets" :depends-on ("decls" "protocol-classes" "multiprocessing" "utils" "transforms" "regions"))
   (:file "mirrors" :depends-on ("sheets"))
   (:file "pixmap" :depends-on ("decls" "protocol-classes" "multiprocessing" "sheets" "transforms" "regions"))
   (:file "events" :depends-on ("decls" "protocol-classes" "multiprocessing" "transforms" "sheets" "utils"))
   (:file "ports" :depends-on ("decls" "protocol-classes" "multiprocessing" "events" "sheets" "pixmap" "utils"))
   (:file "grafts" :depends-on ("decls" "protocol-classes" "multiprocessing" "sheets" "ports" "transforms" "regions"))
   (:file "medium" :depends-on ("decls" "protocol-classes" "multiprocessing" "ports" "X11-colors" "utils" "pixmap" "regions" "transforms" "design"))
   (:file "output" :depends-on ("decls" "protocol-classes" "multiprocessing" "medium"))
   (:file "input" :depends-on ("decls" "protocol-classes" "multiprocessing" "events" "regions" "sheets"))
   (:file "repaint" :depends-on ("decls" "protocol-classes" "multiprocessing" "sheets" "events" "graphics"))
   (:file "graphics" :depends-on ("decls" "protocol-classes" "multiprocessing" "output" "utils" "medium" "sheets" "pixmap" "regions" "design" "transforms"))
   (:file "views" :depends-on ("utils" "protocol-classes"))
   (:file "stream-output" :depends-on ("decls" "protocol-classes" "multiprocessing" "design" "utils" "X11-colors" "views" "output" "sheets" "regions" "graphics" "medium" "setf-star"))
   (:file "recording" :depends-on ("decls" "protocol-classes" "multiprocessing" "output" "coordinates" "graphics" "design" "medium" "transforms" "regions" "sheets" "utils" "stream-output"))
   (:file "encapsulate" :depends-on ("decls" "protocol-classes" "multiprocessing" "sheets" "graphics" "utils" "medium" "input" "stream-output" "recording"))
   (:file "stream-input" :depends-on ("decls" "protocol-classes" "multiprocessing" "input" "ports" "sheets" "events" "encapsulate" "transforms" "utils" "dead-keys"))
   (:file "text-selection" :depends-on ("decls" "protocol-classes" "multiprocessing" "X11-colors" "medium" "output" "transforms" "sheets" "stream-output" "ports" "recording" "regions" "events"))))
