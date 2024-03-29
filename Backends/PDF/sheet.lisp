;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) Copyright 2001 by Arnaud Rouanet <rouanet@emi.u-bordeaux.fr>
;;;  (c) Copyright 2001 by Lionel Salabartan <salabart@emi.u-bordeaux.fr>
;;;  (c) Copyright 2002 by Alexey Dejneka <adejneka@comail.ru>
;;;  (c) Copyright 2002 by Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;  (c) Copyright 2017 by Cyrus Harmon <cyrus@bobobeach.com>
;;;
;;; ---------------------------------------------------------------------------
;;;

(in-package #:clim-pdf)

(defmacro with-output-to-pdf-stream
    ((stream-var file-stream &rest options) &body body)
  `(with-output-to-drawing-stream (,stream-var :pdf ,file-stream ,@options)
     ,@body))

(defmethod invoke-with-output-to-drawing-stream
    (continuation (port (eql :pdf)) file-stream &rest args
     &key (device-type :a4) (orientation :portrait) &allow-other-keys)
  (flet ((make-it (file-stream)
           (with-port (port :pdf :stream file-stream
                                 :device-type device-type
                                 :page-orientation orientation)
             (apply #'invoke-with-output-to-drawing-stream
                    continuation port file-stream args))))
    (typecase file-stream
      ((or pathname string)
       (with-open-file (stream file-stream :direction :output
                                           :if-does-not-exist :create
                                           :if-exists :supersede
                                           :element-type '(unsigned-byte 8))
         (make-it stream)))
      (t (make-it file-stream)))))

(defmethod invoke-with-output-to-drawing-stream
    (continuation (port pdf-port) file-stream
     &key (device-type :a4) multi-page scale-to-fit trim-page-to-output-size
          (orientation :portrait) (units :device) header-comments)
  (let* ((stream (make-clim-pdf-stream port device-type
                                       multi-page scale-to-fit
                                       orientation header-comments)))
    (sheet-adopt-child (find-graft :port port :units units) stream)
    (with-output-recording-options (stream :record t :draw nil)
      (funcall continuation stream)
      (new-page stream))
    (with-slots (title author subject) stream
      (let ((output
              (typecase file-stream
                (stream (flexi-streams:make-flexi-stream
                         file-stream
                         :external-format :latin-1))
                (t file-stream)))
            (pdf:*compress-streams* nil))
        (with-output-recording-options (stream :draw t :record nil)
          (pdf:with-document (:title title :author author :subject subject)
            (dolist (page (reverse (pdf-pages stream)))
              (when trim-page-to-output-size
                (change-page-dimensions port
                                        (+ (bounding-rectangle-width page) *pdf-left-margin* *pdf-right-margin*)
                                        (+ (bounding-rectangle-height page) *pdf-top-margin* *pdf-bottom-margin*)))
              (let* ((page-region (sheet-native-region (graft stream)))
                     (transform (make-pdf-transformation
                                 page-region
                                 page
                                 scale-to-fit
                                 trim-page-to-output-size)))
                (with-bounding-rectangle* (left top right bottom) page-region
                  (pdf:with-page (:bounds (vector left top right bottom))
                    (climi::letf (((sheet-transformation stream)
                                   transform))
                      (replay page stream
                              (if (or scale-to-fit trim-page-to-output-size)
                                  nil
                                  (make-rectangle*
                                   0 0
                                   (- right *pdf-left-margin* *pdf-right-margin*)
                                   (- bottom *pdf-top-margin* *pdf-bottom-margin*)))))))))
            (pdf:write-document output)))))))

(defmethod new-page ((stream clim-pdf-stream))
  (push (stream-output-history stream) (pdf-pages stream))
  (let ((history (make-instance 'standard-tree-output-history :stream stream)))
    (setf (slot-value stream 'climi::output-history) history
          (stream-current-output-record stream) history))
  (setf (stream-cursor-position stream)
        (stream-cursor-initial-position stream)))

;;; Output Protocol

(defmethod make-medium ((port pdf-port) (sheet clim-pdf-stream))
  (make-instance 'pdf-medium :port port :sheet sheet))

(defmethod medium-miter-limit ((medium pdf-medium))
  #.(* pi (/ 11 180))) ; ?

;;; Some strange functions

(defmethod pane-viewport ((pane clim-pdf-stream))
  nil)

(defmethod scroll-extent ((pane clim-pdf-stream) x y)
  (declare (ignore x y))
  (values))

;;; PDF-GRAFT

(defclass pdf-graft (graft)
  ())

(defmethod initialize-instance :after ((graft pdf-graft) &key)
  (setf (slot-value graft 'native-transformation) nil)
  (setf (slot-value graft 'native-region) nil))

(defun graft-length (length units)
  (* length (ecase units
              (:device       1)
              (:inches       (/ 72))
              (:millimeters  (/ 254 720))
              (:screen-sized (/ length)))))

(defmethod graft-width ((graft pdf-graft) &key (units :device))
  (graft-length (bounding-rectangle-width (sheet-native-region graft)) units))

(defmethod graft-height ((graft pdf-graft) &key (units :device))
  (graft-length (bounding-rectangle-height (sheet-native-region graft)) units))

(defmethod sheet-region ((sheet pdf-graft))
  (let ((units (graft-units sheet)))
    (make-rectangle* 0 0
                     (graft-width sheet :units units)
                     (graft-height sheet :units units))))


(defmethod sheet-native-region ((sheet pdf-graft))
  (with-slots (native-region) sheet
    (unless native-region
      (setf native-region
            (paper-region (device-type (port sheet))
                          (page-orientation (port sheet)))))
    native-region))

;; This is necessary because McCLIM didn't reset the
;; native-transformation for basic-sheet. -- admich 2020-01-30
(defmethod invalidate-cached-transformations ((sheet pdf-graft))
  (with-slots (native-transformation device-transformation) sheet
    (setf native-transformation nil
     device-transformation nil))
  (loop for child in (sheet-children sheet)
     do (invalidate-cached-transformations child)))

(defun graft-units-transformation (graft)
  (ecase (graft-units graft)
    (:device +identity-transformation+)
    (:inches (make-scaling-transformation* 72 72))
    (:millimeters (make-scaling-transformation* (/ 720 254) (/ 720 254)))
    (:screen-sized (make-scaling-transformation* (graft-width graft) (graft-height graft)))))

(defun graft-orientation-transformation (graft)
  (ecase (graft-orientation graft)
    (:graphics +identity-transformation+)
    (:default (compose-transformations
               (make-translation-transformation
                0
                (bounding-rectangle-height (sheet-native-region graft)))
               (make-reflection-transformation* 0 0 1 0)))))

(defmethod sheet-native-transformation ((sheet pdf-graft))
  (with-slots (native-transformation) sheet
    (unless native-transformation
      (setf native-transformation
            (compose-transformations
             (graft-orientation-transformation sheet)
             (graft-units-transformation sheet))))
    native-transformation))

(defun change-page-dimensions (port width height)
  (setf (device-type port) (list width height)
        (page-orientation port) :portrait)
  (mapc #'invalidate-cached-regions (port-grafts port))
  (mapc #'invalidate-cached-transformations (port-grafts port)))

;;; Port

(defmethod find-port-type ((type (eql :pdf)))
  (values 'pdf-port 'identity))
