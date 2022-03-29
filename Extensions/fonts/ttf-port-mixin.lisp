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

(defmethod destroy-port :after ((port ttf-port-mixin))
  (with-slots (font-loader-cache font-family-cache font-direct-cache text-style-cache) port
    (maphash-values (lambda (val) (zpb-ttf:close-font-loader val)) font-loader-cache)
    (clrhash font-loader-cache)
    (clrhash font-family-cache)
    (clrhash font-direct-cache)
    (clrhash text-style-cache))
  (setf (font-families port) nil))

(defmethod port-all-font-families ((port ttf-port-mixin) &key invalidate-cache)
  (when invalidate-cache
    (setf (font-families port) nil)
    (register-all-ttf-fonts port))
  (font-families port))

(defun ensure-truetype-font (port source size)
  (setf size (climb:normalize-font-size size))
  (with-slots (font-loader-cache font-family-cache font-direct-cache text-style-cache) port
    (flet ((ensure-family (loader)
             (let ((name (zpb-ttf:family-name loader)))
               (ensure-gethash name font-family-cache
                               (make-instance 'truetype-font-family :name name :port port))))
           (make-face (family loader)
             (let ((name (zpb-ttf:subfamily-name loader)))
               (make-instance 'truetype-face :family family :name name :loader loader)))
           (make-font (face)
             (let ((*dpi* (font-dpi port)))
               (make-instance 'cached-truetype-font :face face :size size))))
      (if-let ((loader (gethash source font-loader-cache)))
        (destructuring-bind (face fonts) (gethash loader font-direct-cache)
          (ensure-gethash size fonts (make-font face)))
        (clim-sys:with-lock-held (*zpb-font-lock*)
          (let* ((loader (zpb-ttf:open-font-loader source))
                 (family (ensure-family loader))
                 (face   (make-face family loader))
                 (fonts  (make-hash-table :test #'eql))
                 (font   (make-font face))
                 (style  (make-text-style (clime:font-family-name family)
                                          (clime:font-face-name face)
                                          size)))
            (setf (gethash source font-loader-cache) loader
                  (gethash loader font-direct-cache) (list face fonts)
                  (gethash size fonts) font
                  (gethash style text-style-cache) font)
            (pushnew family (font-families port))
            font))))))

(defun preload-font (port filename)
  (let* ((hash (slot-value port 'back-memory-cache))
         (file (or (gethash filename hash)
                   (clim-sys:with-lock-held (*zpb-font-lock*)
                     (setf (gethash filename hash)
                           (read-file-into-byte-vector filename))))))
    (flexi-streams:make-in-memory-input-stream file)))

(defun register-ttf-font (port filename preload)
  (with-slots (back-memory-cache) port
    (let ((source (if (not preload)
                      filename
                      (preload-font port filename))))
      (handler-case (dolist (size '(8 10 12 14 18 24 48 72))
                      (ensure-truetype-font port source size))
        (error ()
          (when preload
            (ignore-errors (close source))
            (remhash filename back-memory-cache)))))))

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
          (ensure-truetype-font port source size)))
      (error "~s can't map the text style ~s." port text-style)))

(defmethod text-style-mapping ((port ttf-port-mixin)
                               (text-style climi::device-font-text-style)
                               &optional charset)
  (declare (ignore charset))
  (if-let ((font-name (probe-file (climi::device-font-name text-style))))
    (ensure-truetype-font port font-name :normal)
    (error "~s can't map the text style ~s." port text-style)))
