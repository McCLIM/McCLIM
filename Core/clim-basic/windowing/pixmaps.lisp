;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;; 12.6 Pixmaps
;;;

(in-package #:clim-internals)

(defmethod allocate-pixmap ((sheet sheet) width height)
  (with-sheet-medium (medium sheet)
    (allocate-pixmap medium width height)))

(defmethod allocate-pixmap ((medium medium) width height)
  (declare (ignore medium width height))
  (error "Don't know how to allocate a pixmap for a generic medium"))

(defmethod deallocate-pixmap (pixmap)
  (error "Don't know how to deallocate a pixmap of class ~s" (class-of pixmap)))


(defmethod copy-to-pixmap ((medium medium) medium-x medium-y width height
                           &optional pixmap (pixmap-x 0) (pixmap-y 0))
  (unless pixmap
    (setq pixmap (allocate-pixmap medium (+ pixmap-x width) (+ pixmap-y height))))
  (medium-copy-area medium medium-x medium-y width height
                    pixmap pixmap-x pixmap-y)
  pixmap)

(defmethod copy-to-pixmap ((sheet sheet) sheet-x sheet-y width height
                           &optional pixmap (pixmap-x 0) (pixmap-y 0))
  (copy-to-pixmap (sheet-medium sheet) sheet-x sheet-y width height
                  pixmap pixmap-x pixmap-y))

(defmethod copy-from-pixmap (pixmap pixmap-x pixmap-y width height
                             (medium medium) medium-x medium-y)
  (medium-copy-area pixmap pixmap-x pixmap-y width height
                    medium medium-x medium-y)
  pixmap)

(defmethod copy-from-pixmap (pixmap pixmap-x pixmap-y width height
                             (sheet sheet) sheet-x sheet-y)
  (medium-copy-area pixmap pixmap-x pixmap-y width height
                    (sheet-medium sheet) sheet-x sheet-y))

(defmethod copy-area ((medium medium) from-x from-y width height to-x to-y)
  (medium-copy-area medium from-x from-y width height
                    medium to-x to-y))

(defmethod copy-area ((sheet sheet) from-x from-y width height to-x to-y)
  (copy-area (sheet-medium sheet) from-x from-y width height to-x to-y))

(defmethod copy-area ((stream stream) from-x from-y width height to-x to-y)
  (if (sheetp stream)
      (copy-area (sheet-medium stream) from-x from-y width height to-x to-y)
      (error "COPY-AREA on a stream is not implemented")))

(defmacro with-output-to-pixmap ((medium-var sheet &key width height) &body body)
  (once-only (sheet width height)
    (if (and width height)
        (alexandria:with-gensyms (pixmap port)
          `(let* ((,pixmap (allocate-pixmap ,sheet ,width ,height))
                  (,port (port ,sheet))
                  (,medium-var (make-medium ,port ,sheet)))
             (degraft-medium ,medium-var ,port ,sheet)
             (letf (((medium-drawable ,medium-var) ,pixmap)
                    ((medium-clipping-region ,medium-var)
                     (make-rectangle* 0 0 ,width ,height)))
               ,@body)
             ,pixmap))
        (let ((record (gensym "OUTPUT-RECORD-")))
          ;; What to do when only width or height are given?  And what's the
          ;; meaning of medium-var? -- rudi 2005-09-05
          `(let* ((,medium-var ,sheet)
                  (,record (with-output-to-output-record (,medium-var)
                             ,@body)))
             (with-output-to-pixmap
                 (,medium-var
                  ,sheet
                  :width ,(or width `(bounding-rectangle-width ,record))
                  :height ,(or height `(bounding-rectangle-height ,record)))
               (replay-output-record ,record ,medium-var)))))))
