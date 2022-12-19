(in-package #:mcclim-sdl2)

(defclass resource-manager-mixin ()
  ((clim->resource                      ; clim (obj) -> resource
    :allocation :class                  ; #'equal to allow (:type object)
    :initform (make-hash-table :test #'equal))
   (sdl2->resource                      ; sdl2 (int) -> resource
    :allocation :class
    :initform (make-hash-table))))

(defmacro ensure-resource ((manager clim-object) &body body)
  (alx:once-only (manager)
    `(let ((^clim-object ,clim-object))
       (or (find-clim-resource ,manager ^clim-object)
           (register-resource ,manager (progn ,@body))))))

(defun register-resource (manager object)
  (with-slots (clim->resource sdl2->resource) manager
    (let ((resource-id (sdl2-resource-id object))
          (clim-object (sdl2-resource-clim-object object)))
      (assert (and clim-object (null (gethash clim-object clim->resource))))
      (when resource-id
        (setf (gethash resource-id sdl2->resource) object))
      (setf (gethash clim-object clim->resource) object))))

(defun deregister-resource (manager object)
  (with-slots (clim->resource sdl2->resource) manager
    (remhash (sdl2-resource-id object) sdl2->resource)
    (remhash (sdl2-resource-clim-object object) clim->resource))
  nil)

(defun find-clim-resource (manager clim-object)
  (let ((object (gethash clim-object (slot-value manager 'clim->resource))))
    (if (and object (sdl2-resource-deallocated-p object))
        (deregister-resource object manager)
        object)))

(defun find-sdl2-resource (manager resource-id)
  (let ((object (gethash resource-id (slot-value manager 'sdl2->resource))))
    (if (and object (sdl2-resource-deallocated-p object))
        (deregister-resource object manager)
        object)))

(defun free-resources (manager)
  (loop with clim->resource = (slot-value manager 'clim->resource)
        with sdl2->resource = (slot-value manager 'sdl2->resource)
        for resource being each hash-value of clim->resource
        do (free-sdl2-resource resource)
        finally (clrhash clim->resource)
                (clrhash sdl2->resource)))


;;; An abstract resource class.

(defclass sdl2-resource ()
  ((deallocated-p :initform nil :accessor sdl2-resource-deallocated-p)
   (resource-id :initarg :resource-id :reader sdl2-resource-id) ;may be null
   (clim-object :initarg :clim-object :reader sdl2-resource-clim-object))
  (:default-initargs
   :resource-id nil
   :clim-object (error ":clim-object is a required argument.")))

(defgeneric free-sdl2-resource (object)
  (:method :around ((object sdl2-resource))
    (unless (sdl2-resource-deallocated-p object)
      (call-next-method)
      (setf (sdl2-resource-deallocated-p object) t))
    nil))


;;; Mirror stands for a connection between the SDL2 window and McCLIM sheet.
(defclass sdl2-mirror (sdl2-resource)
  ((window :initarg :window :reader sdl2-mirror-window)
   (clim-object :reader sdl2-mirror-sheet)))

(defun make-sdl2-mirror (&key window-id window sheet)
  (make-instance 'sdl2-mirror :resource-id window-id
                              :clim-object sheet
                              :window window))

(define-sdl2-request sdl2-create-mirror (sheet title x y width height flags)
  (let* ((flags (autowrap:mask-apply 'sdl-window-flags flags))
         (window (sdl2-ffi.functions:sdl-create-window
                  title x y width height flags))
         (id (sdl2-ffi.functions:sdl-get-window-id window)))
    (make-sdl2-mirror :window-id id :window window :sheet sheet)))

(defmethod free-sdl2-resource ((object sdl2-mirror))
  (let ((window (sdl2-mirror-window object)))
    (sdl2-ffi.functions:sdl-destroy-window window)
    (autowrap:invalidate window)))


;;; Conveting designs to SDL2 surfaces is an operation that is required to
;;; produce the window icon, customize the pointer cursor, and in the case of
;;; a software renderer it may be used to draw on the sheet.
;;;
;;; This operation allocates foreign memory that must be freed afterwards. The
;;; caller is responsible for retaining the surface and freeing it when no
;;; longer necessary (i.e when the port or the window is destroyed).
(defclass sdl2-image (sdl2-resource)
  ((clim-object :reader sdl2-image-design)
   (carray :initarg :carray :reader sdl2-image-carray)
   (surface :initarg :surface :reader sdl2-image-surface)))

(defun make-sdl2-image (&key design surface carray)
  (make-instance 'sdl2-image :clim-object design :surface surface :carray carray))

;;; FIXME McCLIM should and will have more image formats ranging from
;;; black/white to full RGBA (like here). When we get to it then tthis
;;; function will need to account for other pattern types too.
(define-sdl2-request sdl2-create-rgb-surface-from-image (design)
  (check-type design image-pattern)
  (let* ((array (pattern-array design))
         (width (pattern-width design))
         (height (pattern-height design))
         (depth 32)                     ; ^ pattern-depth
         (pitch (* 4 width))
         (array-type `(:array :uint32 ,height ,width)))
    (let* ((c-array (cffi:foreign-array-alloc array array-type))
           (surface (sdl2-ffi.functions:sdl-create-rgb-surface-from
                     c-array width height depth pitch
                     #x00ff0000 #x0000ff00 #x000000ff #xff000000)))
      (make-sdl2-image :design design :surface surface :carray c-array))))

(defmethod free-sdl2-resource ((object sdl2-image))
  (let ((surface (sdl2-image-surface object))
        (carray (sdl2-image-carray object)))
    (sdl2-ffi.functions:sdl-free-surface surface)
    (cffi:foreign-array-free carray)
    (autowrap:invalidate surface)))


;;; Cursor maintains a image. We could reuse the design cache but it will be
;;; cleaner to have cursors in a separate collection.

(defclass sdl2-cursor (sdl2-resource)
  ((clim-object :reader sdl2-cursor-design)
   (image :initarg :image :reader sdl2-cursor-image)
   (cursor :initarg :cursor :reader sdl2-cursor-ptr)))

(defun make-sdl2-cursor (&key object image cursor)
  (make-instance 'sdl2-cursor :clim-object object :image image :cursor cursor))

;;; SDL2 (unlike CLIM) allows specifying "hot-x" and "hot-y" as a tip of the
;;; pointer. Perhaps this is a good opportunity for adding an extension.
(define-sdl2-request sdl2-create-color-cursor (design)
  (let* ((image (sdl2-create-rgb-surface-from-image design))
         (surface (sdl2-image-surface image)) ;                      x y
         (cursor (sdl2-ffi.functions:sdl-create-color-cursor surface 0 0)))
    (make-sdl2-cursor :object (list :cursor design) :image image :cursor cursor)))

(define-sdl2-request sdl2-create-system-cursor (cursor-name)
  (let* ((cur-id (map-system-cursor cursor-name))
         (cursor (sdl2-ffi.functions:sdl-create-system-cursor cur-id)))
    (sdl2-ffi.functions:sdl-set-cursor cursor)
    (make-sdl2-cursor :object (list :cursor cursor-name) :image nil :cursor cursor)))

#+ (or) ;; black-white-transparent-inverted cursor.
(define-sdl2-request sdl2-create-cursor (data mask w h hot-x hot-y)
  (error "ENOTIMLEMENTED"))

(defmethod free-sdl2-resource ((object sdl2-cursor))
  (let ((cursor (sdl2-cursor-ptr object)))
    (alx:when-let ((image (sdl2-cursor-image object)))
      (free-sdl2-resource image))
    (sdl2-ffi.functions:sdl-free-cursor cursor)
    (autowrap:invalidate cursor)))
