(in-package :clim-mezzano)

(defclass mezzano-frame-manager (frame-manager)
  ((mirroring-fn :initarg :mirroring
                 :initform (clim-mezzano::mirror-factory :single)
                 :reader mirroring-p)
   (class-gensym :initarg :class-gensym
                 :initform (gensym "MEZZANO-")
                 :reader class-gensym)))

;;; Default mirroring predicates
(defun mirror-factory (kind)
  (etypecase kind
    (null nil)
    (function kind)
    ((eql :single)
     #'(lambda (class)
         (and (not (subtypep class 'mirrored-sheet-mixin))
              (subtypep class 'top-level-sheet-pane))))
    ((eql :full)
     #'(lambda (class)
         (and (not (subtypep class 'mirrored-sheet-mixin))
              (subtypep class 'basic-pane))))
    ((eql :random) ;; for testing
     #'(lambda (class)
         (and (not (subtypep class 'mirrored-sheet-mixin))
              (or (subtypep class 'top-level-sheet-pane)
                  (zerop (random 2))))))))

;;; if the pane is a subclass of basic-pane and it is not mirrored we
;;; create a new class.

(defun maybe-mirroring (fm concrete-pane-class)
  (when (and (not (subtypep concrete-pane-class 'mirrored-sheet-mixin))
	     (funcall (mirroring-p fm) concrete-pane-class))
    (let* ((concrete-pane-class-symbol (if (typep concrete-pane-class 'class)
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
                     (list 'mezzano-mirrored-sheet-mixin
                           'climi::always-repaint-background-mixin
                           concrete-pane-class-symbol)
                     (list 'mezzano-mirrored-sheet-mixin
                           'climi::always-repaint-background-mixin
                           ;;'temporary-medium-sheet-output-mixin
                           'permanent-medium-sheet-output-mixin
                           concrete-pane-class-symbol))))
            (eval
             `(defclass ,class-symbol
                  ,superclasses
                ()
                (:metaclass
                 ,(type-of (find-class concrete-pane-class-symbol)))))))
        (setf concrete-pane-class (find-class class-symbol)))))
  concrete-pane-class)

(defun find-first-defined-class (types)
  (first
   (remove-if #'null
              (mapcar (lambda (class-name)
                        (find-class class-name nil))
                      types))))

(defun find-symbol-from-spec (package-spec name-components)
  (flet ((coerce-name-element (name-elt)
           (typecase name-elt
             (symbol (symbol-name name-elt))
             (sequence (coerce name-elt 'string))
             (t (princ-to-string name-elt)))))
  (find-symbol
   (apply #'concatenate 'string (mapcar #'coerce-name-element name-components))
   package-spec)))

(defun find-symbols (name-specs)
  (remove-if #'null
             (mapcar #'(lambda (x)
                         (find-symbol-from-spec (first x) (rest x)))
                     name-specs)))


(defun generate-standard-pane-specs (type)
  (let ((mapping (get type 'climi::concrete-pane-class-name)))
    `((,(symbol-package mapping) ,mapping)
      (:climi ,mapping)
      (:climi ,type #:-pane)
      (:climi ,type))))

(defun generate-mezzano-pane-specs (type)
  (append
   `((:clim-mezzano #:mezzano- ,type #:-pane)
     (:clim-mezzano #:mezzano- ,type)
     (:climi #:mezzano- ,type #:-pane)
     (:climi #:mezzano- ,type))
   (generate-standard-pane-specs type)))

(defun find-concrete-pane-class (type)
  (if (or (eql (symbol-package type)
               (find-package '#:clim))
          (eql (symbol-package type)
               (find-package '#:climi))
          (eql (symbol-package type)
               (find-package '#:keyword))
	  (get type 'climi::concrete-pane-class-name))
      (find-first-defined-class
       (find-symbols (generate-mezzano-pane-specs type)))
      type))

(defmethod make-pane-1 ((fm mezzano-frame-manager)
                        (frame application-frame) type &rest args)
  (apply #'make-instance
	 (maybe-mirroring fm (find-concrete-pane-class type))
	 :frame frame
	 :manager fm
	 :port (port frame)
	 args))

(defmethod adopt-frame :before ((fm mezzano-frame-manager) (frame menu-frame))
  (multiple-value-bind (buttons mouse-x mouse-y)
      (mos:global-mouse-state)
    (declare (ignore buttons))
    (setf (slot-value frame 'climi::left) (+ mouse-x 5)
          (slot-value frame 'climi::top) (+ mouse-y 5))))

  ;; CLX code for adopt-frame :before
  ;; Temporary kludge.
  ;; (when (eq (slot-value frame 'climi::top) nil)
  ;;   (multiple-value-bind (x y)
  ;;       (xlib:query-pointer (clx-port-window (port fm)))
  ;;     (incf x 10)
  ;;     (setf (slot-value frame 'climi::left) x
  ;;           (slot-value frame 'climi::top) y)))


(defmethod adopt-frame :after ((fm mezzano-frame-manager) (frame menu-frame))
  ;; TODO not sure what to do here - maybe draw frame should be moved
  ;; here from create-mezzano-mirror? Then need additional cases:
  ;; application-frame
  ;; others?

  ;; (when (sheet-enabled-p (slot-value frame 'top-level-sheet))
  ;;   (xlib:map-window (sheet-direct-xmirror (slot-value frame 'top-level-sheet))))
  )
