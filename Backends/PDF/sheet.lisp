;;; -*- Mode: Lisp; Package: CLIM-PDF -*-

;;;  (c) copyright 2001 by
;;;           Arnaud Rouanet (rouanet@emi.u-bordeaux.fr)
;;;           Lionel Salabartan (salabart@emi.u-bordeaux.fr)
;;;  (c) copyright 2002 by
;;;           Alexey Dejneka (adejneka@comail.ru)
;;;           Gilbert Baumann (unk6@rz.uni-karlsruhe.de)
;;;  (c) copyright 2017 by
;;;           Cyrus Harmon (cyrus@bobobeach.com)

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA  02111-1307  USA.


(in-package :clim-pdf)

(defmacro with-output-to-pdf-stream ((stream-var file-stream
                                             &rest options)
                                            &body body)
  (let ((cont (gensym)))
    `(flet ((,cont (,stream-var)
              ,@body))
       (declare (dynamic-extent #',cont))
       (invoke-with-output-to-pdf-stream #',cont
                                                ,file-stream ,@options))))

(defun invoke-with-output-to-pdf-stream (continuation
                                         file-stream
                                         &key device-type
                                              multi-page scale-to-fit
                                              trim-page-to-output-size
                                              (orientation :portrait)
                                              header-comments)
  (let* ((port (find-port :server-path `(:pdf :stream ,file-stream)))
         (stream (make-clim-pdf-stream file-stream port device-type
                                  multi-page scale-to-fit
                                  orientation header-comments))
         translate-x translate-y)
    (declare (ignore translate-x translate-y))
    (unwind-protect
         (progn
           (with-output-recording-options (stream :record t :draw nil)
             (funcall continuation stream)
             (new-page stream))
           (with-slots (file-stream title for orientation paper) stream
             (let ((flexi-stream
                    (flexi-streams:make-flexi-stream
                     file-stream
                     :external-format :latin-1)))
               (with-output-recording-options (stream :draw t :record nil)
                 (let ((pdf:*compress-streams* nil))
                   (pdf:with-document ()
                     (let ((last-page (first (pdf-pages stream))))
                       (dolist (page (reverse (pdf-pages stream)))
                         (let ((page-region (if trim-page-to-output-size
                                                page
                                                (sheet-region stream))))
                           (let ((transform (make-pdf-transformation page-region
                                                                     (orientation stream))))
                             (multiple-value-bind (left top right bottom)
                                 (bounding-rectangle* page-region)
                               (pdf:with-page (:bounds
                                               (if (eq orientation :landscape)
                                                   (vector top left bottom right)
                                                   (vector left top right bottom)))
                                 (climi::letf (((sheet-native-transformation stream)
                                                transform))
                                   (replay page stream))))))
                         (unless (eql page last-page)
                           (emit-new-page stream))))
                     (pdf:write-document flexi-stream)))))))
      (destroy-port port))))

;; FIXME! Not yet implemented.
(defun start-page (stream)
  (with-slots (file-stream current-page transformation) stream
    ))

(defmethod new-page ((stream clim-pdf-stream))
  (push (stream-output-history stream) (pdf-pages stream))
  (let ((history (make-instance 'standard-tree-output-history :stream stream)))
    (setf (slot-value stream 'climi::output-history) history
          (stream-current-output-record stream) history))
  (setf (stream-cursor-position stream) (values 0 0)))

(defun emit-new-page (stream)
  (error "not yet! ~S" stream))

;;;; Output Protocol
(defmethod medium-drawable ((medium pdf-medium))
  (pdf-medium-file-stream medium))

(defmethod make-medium ((port pdf-port) (sheet clim-pdf-stream))
  (make-instance 'pdf-medium :sheet sheet :port port))

(defmethod medium-miter-limit ((medium pdf-medium))
  #.(* pi (/ 11 180))) ; ?


(defmethod sheet-direct-mirror ((sheet clim-pdf-stream))
  (clim-pdf-stream-file-stream sheet))

(defmethod sheet-mirrored-ancestor ((sheet clim-pdf-stream))
  sheet)

(defmethod sheet-mirror ((sheet clim-pdf-stream))
  (sheet-direct-mirror sheet))

(defmethod realize-mirror ((port pdf-port) (sheet clim-pdf-stream))
  (sheet-direct-mirror sheet))

(defmethod destroy-mirror ((port pdf-port) (sheet clim-pdf-stream))
  (error "Can't destroy mirror for the pdf stream ~S." sheet))

;;; Some strange functions

(defmethod pane-viewport ((pane clim-pdf-stream))
  nil)

(defmethod scroll-extent ((pane clim-pdf-stream) x y)
  (declare (ignore x y))
  (values))

;;;;
;;;; PDF-GRAFT
;;;;

(defclass pdf-graft (basic-sheet sheet-leaf-mixin)
  ((width  :initform 210 :reader pdf-graft-width)
   (height :initform 297 :reader pdf-graft-height)))

(defmethod graft-orientation ((graft pdf-graft))
  :graphics)

(defmethod graft-units ((graft pdf-graft))
  :device)

(defmethod graft-width ((graft pdf-graft) &key (units :device))
  (* (pdf-graft-width graft)
     (ecase units
       (:device         (/ 720 254))
       (:inches         (/ 10 254))
       (:millimeters    1)
       (:screen-sized   (/ (pdf-graft-width graft))))))

(defmethod graft-height ((graft pdf-graft) &key (units :device))
  (* (pdf-graft-height graft)
     (ecase units
       (:device         (/ 720 254))
       (:inches         (/ 10 254))
       (:millimeters    1)
       (:screen-sized   (/ (pdf-graft-height graft))))))

(defun make-pdf-graft ()
  (make-instance 'pdf-graft))

(defmethod sheet-region ((sheet pdf-graft))
  (make-rectangle* 0 0
                   (graft-width sheet :units (graft-units sheet))
                   (graft-height sheet :units (graft-units sheet))))

(defmethod graft ((sheet pdf-graft))
  sheet)

;;; Port

(setf (get :pdf :port-type) 'pdf-port)
(setf (get :pdf :server-path-parser) 'parse-pdf-server-path)

(defun parse-pdf-server-path (path)
  path)
