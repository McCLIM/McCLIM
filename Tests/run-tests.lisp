(defpackage :clim-tests
  (:use :clim-lisp :clim))

(let ((files (remove *load-pathname*
                     (directory (merge-pathnames (make-pathname :name :wild)
                                                 *load-pathname*))
                     :test #'equal)))
  (dolist (f files)
    (handler-case (load f)
      (error (c) (format *error-output* "~@<Error in file ~S:~2I~_~A~@:>" f c)))))
