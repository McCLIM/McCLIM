(defpackage #:clim-mop
  (:use #:clos))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (loop for sym being the symbols of :clim-mop
     do (export sym :clim-mop)))

;; CLIM expects INPUT-STREAM-P to be a generic function.
(unless (typep #'input-stream-p 'generic-function)
  (setf (fdefinition 'gray::original-input-stream-p) #'input-stream-p)
  (fmakunbound 'input-stream-p)
  (defgeneric input-stream-p (stream)
    (:method ((stream stream)) (gray::original-input-stream-p stream))))

;; CLIM expects OUTPUT-STREAM-P to be a generic function.
(unless (typep #'output-stream-p 'generic-function)
  (setf (fdefinition 'gray::original-output-stream-p) #'output-stream-p)
  (fmakunbound 'output-stream-p)
  (defgeneric output-stream-p (stream)
    (:method ((stream stream)) (gray::original-output-stream-p stream))))
