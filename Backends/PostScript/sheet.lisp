;;; -*- Mode: Lisp; Package: CLIM-POSTSCRIPT -*-

;;;  (c) copyright 2001 by
;;;           Arnaud Rouanet (rouanet@emi.u-bordeaux.fr)
;;;           Lionel Salabartan (salabart@emi.u-bordeaux.fr)
;;;  (c) copyright 2002 by
;;;           Alexey Dejneka (adejneka@comail.ru)
;;;           Gilbert Baumann (unk6@rz.uni-karlsruhe.de)

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

;;; TODO:
;;;
;;; - do smth with POSTSCRIPT-GRAFT.

;;; Also missing IMO:
;;;
;;; - WITH-OUTPUT-TO-POSTSCRIPT-STREAM should offer a :PAPER-SIZE option.
;;; - NEW-PAGE should also offer to specify the page name.
;;; - device fonts are missing
;;; - font metrics are missing
;;;
;;;--GB

(in-package :CLIM-POSTSCRIPT)

(defmacro with-output-to-postscript-stream ((stream-var file-stream
                                             &rest options)
                                            &body body)
  (let ((cont (gensym)))
    `(flet ((,cont (,stream-var)
              ,@body))
       (declare (dynamic-extent #',cont))
       (invoke-with-output-to-postscript-stream #',cont
                                                ,file-stream ,@options))))

(defun invoke-with-output-to-postscript-stream (continuation
                                                file-stream &key device-type
                                                multi-page scale-to-fit
                                                (orientation :portrait)
                                                header-comments)
  (let ((stream (make-postscript-stream file-stream device-type
                                        multi-page scale-to-fit
                                        orientation header-comments)))
    (prog2
        (with-slots (file-stream title for orientation paper) stream
          (format file-stream "%!PS-Adobe-3.0~%")
          (format file-stream "%%Creator: McCLIM~%")
          (format file-stream "%%Title: ~A~%" title)
          (format file-stream "%%For: ~A~%" for)
          (format file-stream "%%LanguageLevel: 2~%")
          (format file-stream "%%DocumentMedia: ~A~%" paper)
          (format file-stream "%%Orientation: ~A~%"
                  (ecase orientation
                    (:portrait "Portrait")
                    (:landscape "Landscape")))
          (format file-stream "%%Pages: (atend)~%")
          (format file-stream "%%DocumentNeededResources: (atend)~%")
          (format file-stream "%%EndComments~%~%")
          (start-page stream))
        (with-graphics-state ((sheet-medium stream))
          ;; we need at least one level of saving -- APD, 2002-02-11
          (funcall continuation stream))
      (with-slots (file-stream current-page) stream
        (format file-stream "showpage~%~%")
        (format file-stream "%%Trailer~%")
        (format file-stream "%%Pages: ~D~%" current-page)
        (format file-stream "%%DocumentNeededResources: ~{font ~A~%~^%%+ ~}~%"
                (reverse (slot-value (sheet-medium stream) 'document-fonts)))
        (format file-stream "%%EOF~%")
        (finish-output file-stream)))))

(defun start-page (stream)
  (with-slots (file-stream current-page transformation) stream
      (format file-stream "%%Page: ~D ~:*~D~%" (incf current-page))))

(defun new-page (stream)
  ;; FIXME: it is necessary to do smth with GS -- APD, 2002-02-11
  (let ((medium (sheet-medium stream)))
    (postscript-restore-graphics-state medium)
    (format (postscript-stream-file-stream stream) "showpage~%")
    (start-page stream)
    (postscript-save-graphics-state medium))
  (clear-output-record (stream-output-history stream)))


;;;; Output Protocol

(defmethod medium-drawable ((medium postscript-medium))
  (postscript-medium-file-stream medium))

(defmethod port ((medium postscript-medium))
  ;; FIXME
  nil)

(defmethod make-medium (port (sheet postscript-stream))
  (declare (ignorable port))
  (make-postscript-medium (postscript-stream-file-stream sheet) sheet))


(defmethod sheet-direct-mirror ((sheet postscript-stream))
  (postscript-stream-file-stream sheet))

(defmethod sheet-mirrored-ancestor ((sheet postscript-stream))
  sheet)

(defmethod sheet-mirror ((sheet postscript-stream))
  (sheet-direct-mirror sheet))

(defmethod realize-mirror (port (sheet postscript-stream))
  (sheet-direct-mirror sheet))

(defmethod destroy-mirror (port (sheet postscript-stream))
  (error "Can't destroy mirror for the postscript stream ~S." sheet))

(defmethod port ((sheet postscript-stream))
  ;; FIXME
  nil)

;;;;
;;;; POSTSCRIPT-GRAFT
;;;;

(defclass postscript-graft (basic-sheet sheet-leaf-mixin)
  ((width  :initform 210 :reader postscript-graft-width)
   (height :initform 297 :reader postscript-graft-height)))

(defmethod graft-orientation ((graft postscript-graft))
  :graphics)

(defmethod graft-units ((graft postscript-graft))
  :device)

(defmethod graft-width ((graft postscript-graft) &key (units :device))
  (* (postscript-graft-width graft)
     (ecase units
       (:device         (/ 720 254))
       (:inches         (/ 10 254))
       (:millimeters    1)
       (:screen-sized   (/ (postscript-graft-width graft))))))

(defmethod graft-height ((graft postscript-graft) &key (units :device))
  (* (postscript-graft-height graft)
     (ecase units
       (:device         (/ 720 254))
       (:inches         (/ 10 254))
       (:millimeters    1)
       (:screen-sized   (/ (postscript-graft-height graft))))))

(defun make-postscript-graft ()
  (make-instance 'postscript-graft))

(defmethod sheet-region ((sheet postscript-graft))
  (make-rectangle* 0 0
                   (graft-width sheet :units (graft-units sheet))
                   (graft-height sheet :units (graft-units sheet))))

(defmethod graft ((sheet postscript-graft))
  sheet)
