(defmacro defconstant (var val) `(defparameter ,var ,val))

(unexport 'ext::e :ext)

(setf *compile-print* nil)
(setf *gc-verbose* nil)



