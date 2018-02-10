(in-package :clim-mezzano)


(defclass mezzano-frame-manager (frame-manager)
  ())


;; the panes to be mirrored
(defun get-mirroring-fn (port)
  #'(lambda (pane-class)
      (subtypep pane-class 'top-level-sheet-pane)))

;;; if the pane is a subclass of basic-pane and it is not mirrored we
;;; create a new class.

(defun maybe-mirroring (port concrete-pane-class)
  (when (and (not (subtypep concrete-pane-class 'mirrored-sheet-mixin))
	     (funcall (get-mirroring-fn port) concrete-pane-class))
    (let* ((concrete-pane-class-symbol
            (if (typep concrete-pane-class 'class)
                (class-name concrete-pane-class)
                concrete-pane-class))
	   (concrete-mirrored-pane-class
            (concatenate 'string
                         "Mezzano-"
                         (symbol-name concrete-pane-class-symbol)
                         "-DUMMY"))
	   (concrete-mirrored-pane-class-symbol
            (find-symbol concrete-mirrored-pane-class :clim-mezzano))
	   (superclasses
            (if (subtypep concrete-pane-class 'sheet-with-medium-mixin)
                (list 'mezzano-mirrored-sheet-mixin
                      concrete-pane-class-symbol)
                (list 'mezzano-mirrored-sheet-mixin
                      ;;'temporary-medium-sheet-output-mixin
                      'permanent-medium-sheet-output-mixin
                      concrete-pane-class-symbol))))
      (unless concrete-mirrored-pane-class-symbol
	(setf concrete-mirrored-pane-class-symbol
	      (intern concrete-mirrored-pane-class :clim-mezzano))
	(eval
	 `(defclass ,concrete-mirrored-pane-class-symbol
	      ,superclasses
	    ()
	    (:metaclass ,(type-of (find-class concrete-pane-class-symbol)))))
	(format *debug-io*
                "create class ~A~%" concrete-mirrored-pane-class-symbol))
      (setf concrete-pane-class
            (find-class concrete-mirrored-pane-class-symbol))))
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
	 (maybe-mirroring (port fm)
                          (clim-mezzano::find-concrete-pane-class type))
	 :frame frame
	 :manager fm
	 :port (port frame)
	 args))

(defmethod adopt-frame :before ((fm mezzano-frame-manager) (frame menu-frame))
  (multiple-value-bind (buttons mouse-x mouse-y)
      (mezzano.gui.compositor::global-mouse-state)
    (declare (ignore buttons))
    (setf (slot-value frame 'climi::left) (+ mouse-x 10)
          (slot-value frame 'climi::top) mouse-y)))

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
