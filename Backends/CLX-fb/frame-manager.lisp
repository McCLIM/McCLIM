(in-package :clim-clx-fb)


(defclass clx-fb-frame-manager (clim-clx::clx-frame-manager)
  ()
  (:default-initargs :mirroring (clim-clx::mirror-factory :single)
                     :class-gensym (gensym "CLXFB")))

;;; if the pane is a subclass of basic-pane and it is not mirrored we create a new class.
(defun maybe-mirroring (fm concrete-pane-class)
  (when (and (not (subtypep concrete-pane-class 'mirrored-sheet-mixin))
	     (funcall (clim-clx::mirroring-p fm) concrete-pane-class))
    (let* ((concrete-pane-class-symbol (if (typep concrete-pane-class 'class)
                                          (class-name concrete-pane-class)
                                          concrete-pane-class)))
      (multiple-value-bind (class-symbol foundp)
          (alexandria:ensure-symbol
           (alexandria:symbolicate (clim-clx::class-gensym fm) "-"
                                   (symbol-name concrete-pane-class-symbol))
           :clim-clx-fb)
	(unless foundp
          (let ((superclasses (if (subtypep concrete-pane-class 'sheet-with-medium-mixin)
                                  (list 'clx-fb-mirrored-sheet-mixin
                                        'climi::always-repaint-background-mixin
                                        concrete-pane-class-symbol)
                                  (list 'clx-fb-mirrored-sheet-mixin
                                        'climi::always-repaint-background-mixin
                                        ;;'temporary-medium-sheet-output-mixin
                                        'permanent-medium-sheet-output-mixin
                                        concrete-pane-class-symbol))))
            (eval
             `(defclass ,class-symbol
                  ,superclasses
                ()
                (:metaclass ,(type-of (find-class concrete-pane-class-symbol)))))))
        (format *debug-io* "dummy class mirror ~A: ~A~%" concrete-pane-class-symbol class-symbol)
        (setf concrete-pane-class (find-class class-symbol)))))
  concrete-pane-class)

(defmethod make-pane-1 ((fm clx-fb-frame-manager) (frame application-frame) type &rest args)
  (apply #'make-instance
	 (maybe-mirroring fm (clim-clx::find-concrete-pane-class type))
	 :frame frame
	 :manager fm
	 :port (port frame)
	 args))
