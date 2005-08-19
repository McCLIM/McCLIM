(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :clim-lisp-patch)
    (make-package :clim-lisp-patch :use nil)))

(export '(clim-lisp-patch::describe 
          clim-lisp-patch::describe-object 
          clim-lisp-patch::interactive-stream-p) 
        :clim-lisp-patch)
