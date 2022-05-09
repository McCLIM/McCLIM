(in-package #:clim-mezzano)

(defclass mezzano-frame-manager (standard-frame-manager)
  ((class-gensym :initarg :class-gensym
                 :initform (gensym "MEZZANO-")
                 :reader class-gensym)))

;;; if the pane is a subclass of basic-pane and it is not mirrored we
;;; create a new class.

(defun maybe-mirroring (fm concrete-pane-class)
  (when (subtypep concrete-pane-class
                  '(and (not mirrored-sheet-mixin) clime:top-level-sheet-mixin))
    (let ((concrete-pane-class-symbol
            (if (typep concrete-pane-class 'class)
                (class-name concrete-pane-class)
                concrete-pane-class)))
      (multiple-value-bind (class-symbol foundp)
          (alexandria:ensure-symbol
           (alexandria:symbolicate (class-gensym fm) "-"
                                   (symbol-name concrete-pane-class-symbol))
           :clim-mezzano)
        (unless foundp
          (let ((superclasses
                  (if (subtypep concrete-pane-class 'sheet-with-medium-mixin)
                      (list 'mirrored-sheet-mixin
                            concrete-pane-class-symbol
                            'climi::always-repaint-background-mixin)
                      (list 'mirrored-sheet-mixin
                            'permanent-medium-sheet-output-mixin
                            concrete-pane-class-symbol
                            'climi::always-repaint-background-mixin))))
            (eval
             `(defclass ,class-symbol ,superclasses ()))))
        (setf concrete-pane-class (find-class class-symbol)))))
  concrete-pane-class)

(defmethod find-concrete-pane-class ((frame-manager mezzano-frame-manager)
                                     pane-type &optional (errorp t))
  (maybe-mirroring frame-manager (find-concrete-pane-class t pane-type errorp)))

(defmethod adopt-frame :before ((fm mezzano-frame-manager) (frame menu-frame))
  (multiple-value-bind (buttons mouse-x mouse-y)
      (mos:global-mouse-state)
    (declare (ignore buttons))
    (setf (slot-value frame 'climi::left) (+ mouse-x 5)
          (slot-value frame 'climi::top) (+ mouse-y 5))))
