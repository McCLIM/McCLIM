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

(cl:in-package #:clim-pdf)

(defmacro with-output-to-pdf-stream ((stream-var file-stream &rest options)
                                     &body body)
  (let ((cont (gensym)))
    `(flet ((,cont (,stream-var)
              ,@body))
       (declare (dynamic-extent #',cont))
       (invoke-with-output-to-pdf-stream #',cont ,file-stream ,@options))))

(defun invoke-with-output-to-pdf-stream (continuation
                                         file-stream
                                         &key device-type
                                              multi-page scale-to-fit
                                              trim-page-to-output-size
                                              (orientation :portrait)
                                              header-comments)
  (climb:with-port (port :pdf :stream file-stream)
    (let* ((stream (make-clim-pdf-stream file-stream port device-type
                                         multi-page scale-to-fit
                                         orientation header-comments)))
      (with-output-recording-options (stream :record t :draw nil)
        (funcall continuation stream)
        (new-page stream))
      (with-slots (file-stream title author subject) stream
        (let ((output
               (typecase file-stream
                 (stream (flexi-streams:make-flexi-stream
                          file-stream
                          :external-format :latin-1))
                 (t file-stream)))
              (pdf:*compress-streams* nil))
          (with-output-recording-options (stream :draw t :record nil)
            (pdf:with-document (:title title :author author :subject subject)
              (let ((last-page (first (pdf-pages stream))))
                (dolist (page (reverse (pdf-pages stream)))
                  (let* ((page-region (if trim-page-to-output-size
                                          (make-rectangle* 0 0
                                                          (+ (bounding-rectangle-width page) *pdf-left-margin* *pdf-right-margin*)
                                                          (+ (bounding-rectangle-height page) *pdf-top-margin* *pdf-bottom-margin*))
                                          (sheet-region stream)))
                         (transform (make-pdf-transformation
                                     page-region
                                     page
                                     scale-to-fit)))
                    (with-bounding-rectangle* (left top right bottom) page-region
                      (pdf:with-page (:bounds (vector left top right bottom))
                        (climi::letf (((sheet-native-transformation stream)
                                       transform))
                          (replay page stream)))))))
              (pdf:write-document output))))))))

(defmethod new-page ((stream clim-pdf-stream))
  (push (stream-output-history stream) (pdf-pages stream))
  (let ((history (make-instance 'standard-tree-output-history :stream stream)))
    (setf (slot-value stream 'climi::output-history) history
          (stream-current-output-record stream) history))
  (setf (stream-cursor-position stream)
        (stream-cursor-initial-position stream)))

;;; Output Protocol

(defmethod medium-drawable ((medium pdf-medium))
  (pdf-medium-file-stream medium))

(defmethod make-medium ((port pdf-port) (sheet clim-pdf-stream))
  (make-instance 'pdf-medium :sheet sheet))

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

;;; PDF-GRAFT

(defclass pdf-graft (sheet-leaf-mixin basic-sheet)
  ((width  :initform 210 :reader pdf-graft-width)
   (height :initform 297 :reader pdf-graft-height)))

(defmethod graft-orientation ((graft pdf-graft))
  :graphics)

(defmethod graft-units ((graft pdf-graft))
  :device)

(defun graft-length (length units)
  (* length (ecase units
              (:device       (/ 720 254))
              (:inches       (/ 10 254))
              (:millimeters  1)
              (:screen-sized (/ length)))))

(defmethod graft-width ((graft pdf-graft) &key (units :device))
  (graft-length (pdf-graft-width graft) units))

(defmethod graft-height ((graft pdf-graft) &key (units :device))
  (graft-length (pdf-graft-height graft) units))

(defun make-pdf-graft ()
  (make-instance 'pdf-graft))

(defmethod sheet-region ((sheet pdf-graft))
  (let ((units (graft-units sheet)))
    (make-rectangle* 0 0
                     (graft-width sheet :units units)
                     (graft-height sheet :units units))))

(defmethod graft ((sheet pdf-graft))
  sheet)

;;; Port

(setf (get :pdf :port-type) 'pdf-port)
(setf (get :pdf :server-path-parser) 'parse-pdf-server-path)

(defun parse-pdf-server-path (path)
  path)
