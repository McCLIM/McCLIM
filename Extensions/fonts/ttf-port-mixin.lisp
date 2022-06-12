;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) Copyright 2022 by Daniel Kochmański <daniel@turtleware.eu>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; TTF-PORT-MIXIN implements loading of fonts and mapping them to text styles.
;;; It is possible to preload fonts to keep them in the image.
;;;

(in-package #:mcclim-truetype)

(defclass ttf-port-mixin ()
  ((back-memory-cache :initform (make-hash-table :test #'equal) :allocation :class)
   ;; source -> loader (the source may be a filename or a memory block)
   (font-loader-cache :initform (make-hash-table :test #'equal))
   (font-family-cache :initform (make-hash-table :test #'equal))
   ;; Cache loader -> (face fonts) - fonts is a ht keyed wit hthe size.
   (font-direct-cache :initform (make-hash-table))
   ;; Cache for the extended standard text styles (see the manual).
   (text-style-cache  :initform (make-hash-table))
   ;; All registered families. Populated by ensure-truetype-font.
   (font-families :initform '() :accessor font-families)
   ;; DPI (for font scaling)
   (font-dpi :initform *dpi* :initarg :dpi :reader font-dpi)))

(defun invalidate-port-font-cache (port)
  (with-slots (font-loader-cache font-family-cache font-direct-cache text-style-cache) port
    (maphash-values (lambda (val) (zpb-ttf:close-font-loader val)) font-loader-cache)
    (clrhash font-loader-cache)
    (clrhash font-family-cache)
    (clrhash font-direct-cache)
    (clrhash text-style-cache))
  (setf (font-families port) nil))

(defmethod destroy-port :after ((port ttf-port-mixin))
  (invalidate-port-font-cache port))

(defmethod port-all-font-families ((port ttf-port-mixin) &key invalidate-cache preload)
  (when invalidate-cache
    (invalidate-port-font-cache port)
    (register-all-ttf-fonts port :preload preload)
    (register-standard-fonts port :preload preload))
  (font-families port))

(defun ensure-truetype-font (port filename source size &optional preload)
  (setf size (climb:normalize-font-size size))
  (with-slots (font-loader-cache font-family-cache font-direct-cache text-style-cache) port
    (multiple-value-bind (loader loader-foundp) (gethash filename font-loader-cache)
      (let* ((loader (or loader (zpb-ttf:open-font-loader source)))
             (f1-name (zpb-ttf:family-name loader))
             (f2-name (zpb-ttf:subfamily-name loader))
             (text-style (make-text-style f1-name f2-name size)))
        (flet ((make-family ()
                 (make-instance 'truetype-font-family :name f1-name :port port))
               (make-face (family)
                 (make-instance 'truetype-face :family family :name f2-name
                                               :loader loader :preloaded preload))
               (make-font (face size)
                 (let ((*dpi* (font-dpi port)))
                   (make-instance 'cached-truetype-font :face face :size size))))
          (when loader-foundp
            (return-from ensure-truetype-font
              (destructuring-bind (face fonts) (gethash loader font-direct-cache)
                (setf (gethash text-style text-style-cache)
                      (ensure-gethash size fonts (make-font face size))))))
          (let* ((family (ensure-gethash f1-name font-family-cache (make-family)))
                 (face   (make-face family))
                 (fonts  (make-hash-table :test #'eql))
                 (font   (make-font face size)))
            (setf (gethash filename font-loader-cache) loader
                  (gethash loader font-direct-cache) (list face fonts)
                  (gethash size fonts) font
                  (gethash text-style text-style-cache) font)
            (pushnew family (font-families port))
            font))))))

(defun register-ttf-font (port filename preload)
  (clim-sys:with-lock-held (*zpb-font-lock*)
    (let* ((vector (gethash filename (slot-value port 'back-memory-cache)))
           (source (if (and (not preload) (not vector))
                       filename
                       (flexi-streams:make-in-memory-input-stream
                        (ensure-gethash filename
                                        (slot-value port 'back-memory-cache)
                                        (read-file-into-byte-vector filename))))))
      (handler-case (dolist (size '(8 10 12 14 18 24 48 72))
                      (ensure-truetype-font port filename source size preload))
        (error ()
          (ignore-errors (and (streamp source) (close source)))
          (remhash filename (slot-value port 'back-memory-cache)))))))

(defun register-all-ttf-fonts (port &key (dir *truetype-font-path*) (preload nil))
  (with-port-locked (port)
    (dolist (source (directory (merge-pathnames "*.ttf" dir)))
      (register-ttf-font port source preload))))

(defun register-standard-fonts (port &key (preload nil))
  (with-port-locked (port)
    (dolist (source (mapcar #'cdr *families/faces*))
      (register-ttf-font port source preload))))

(defmethod text-style-mapping ((port ttf-port-mixin) (text-style standard-text-style) &optional charset)
  (declare (ignore charset))
  (setf text-style (parse-text-style* text-style))
  (or (gethash text-style (slot-value port 'text-style-cache))
      (multiple-value-bind (family face size) (text-style-components text-style)
        (when-let ((source (assoc-value *families/faces* (list family face) :test #'equal)))
          (clim-sys:with-lock-held (*zpb-font-lock*)
            (ensure-truetype-font port source source size))))
      (error "~s can't map the text style ~s." port text-style)))

(defmethod text-style-mapping ((port ttf-port-mixin)
                               (text-style climi::device-font-text-style)
                               &optional charset)
  (declare (ignore charset))
  (if-let ((font-name (probe-file (climi::device-font-name text-style))))
    (clim-sys:with-lock-held (*zpb-font-lock*)
      (ensure-truetype-font port font-name font-name :normal))
    (error "~s can't map the text style ~s." port text-style)))
