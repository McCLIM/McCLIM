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

(in-package :clim-postscript)

(defun write-font-to-postscript-stream (stream text-style)
  (with-open-file (font-stream
		   (clim-postscript-font:postscript-device-font-name-font-file
                    (clim-internals::device-font-name text-style))
		   :direction :input
		   :external-format :latin-1)
    (let ((font (make-string (file-length font-stream))))
      (read-sequence font font-stream)
      (write-string font (postscript-medium-file-stream stream)))))

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
  (let* ((port (find-port :server-path `(:ps :stream ,file-stream)))
         (stream (make-postscript-stream file-stream port device-type
                                         multi-page scale-to-fit
                                         orientation header-comments))
         translate-x translate-y)
    (unwind-protect
         (progn
           (with-output-recording-options (stream :record t :draw nil)
             (with-graphics-state (stream)
               ;; we need at least one level of saving -- APD, 2002-02-11
               (funcall continuation stream)
               (unless (eql (slot-value stream 'paper) :eps)
                 (new-page stream))))	; Close final page.
           (with-slots (file-stream title for orientation paper) stream
             (format file-stream "%!PS-Adobe-3.0~@[ EPSF-3.0~*~]~%"
                     (eq device-type :eps))
             (format file-stream "%%Creator: McCLIM~%")
             (format file-stream "%%Title: ~A~%" title)
             (format file-stream "%%For: ~A~%" for)
             (format file-stream "%%LanguageLevel: 2~%")
             (case paper
               ((:eps)
                (let ((record (stream-output-history stream)))
                  (multiple-value-bind (lx ly ux uy) (bounding-rectangle* record)
                    (setf translate-x (- (floor lx))
                          translate-y (ceiling uy))
                    (format file-stream "%%BoundingBox: ~A ~A ~A ~A~%" 
                            0 0
                            (+ translate-x (ceiling ux))
                            (- translate-y (floor ly))))))
               (t
                (multiple-value-bind (width height)
                    (paper-size paper)
                  (format file-stream "%%BoundingBox: 0 0 ~A ~A~%" width height)
                  (format file-stream "%%DocumentMedia: ~A ~A ~A 0 () ()~%"
                          paper width height))
                (format file-stream "%%Orientation: ~A~%"
                        (ecase orientation
                          (:portrait "Portrait")
                          (:landscape "Landscape")))
                (format file-stream "%%Pages: (atend)~%")))
             (format file-stream "%%DocumentNeededResources: (atend)~%")
             (format file-stream "%%EndComments~%~%")
             (write-postscript-dictionary file-stream)
             (dolist (text-style (clim-postscript-font:device-fonts (sheet-medium stream)))
               (write-font-to-postscript-stream (sheet-medium stream) text-style))
             (start-page stream)
             (format file-stream "~@[~A ~]~@[~A translate~%~]" translate-x translate-y)
	     
	     (with-output-recording-options (stream :draw t :record nil)
	       (with-graphics-state (stream)
                 (case paper
                   ((:eps) (replay (stream-output-history stream) stream))
                   (t (let ((last-page (first (postscript-pages stream))))
                        (dolist (page (reverse (postscript-pages stream)))
                          (replay page stream)
                          (unless (eql page last-page)
                            (emit-new-page stream))))))))))

      (with-slots (file-stream current-page) stream
        (format file-stream "end~%showpage~%~%")
        (format file-stream "%%Trailer~%")
        (format file-stream "%%Pages: ~D~%" current-page)
        (format file-stream "%%DocumentNeededResources: ~{font ~A~%~^%%+ ~}~%"
                (reverse (slot-value stream 'document-fonts)))
        (format file-stream "%%EOF~%")
        (finish-output file-stream))
      (destroy-port port))))


(defun start-page (stream)
  (with-slots (file-stream current-page transformation) stream
    (format file-stream "%%Page: ~D ~:*~D~%" (incf current-page))
    (format file-stream "~A begin~%" *dictionary-name*)))

(defmethod new-page ((stream postscript-stream))
  (push (stream-output-history stream) (postscript-pages stream))
  (let ((history (make-instance 'standard-tree-output-history :stream stream)))
    (setf (slot-value stream 'climi::output-history) history
	  (stream-current-output-record stream) history))    
  (setf (stream-cursor-position stream) (values 0 0)))

(defun emit-new-page (stream)
  ;; FIXME: it is necessary to do smth with GS -- APD, 2002-02-11
  ;; FIXME^2:  what do you mean by that? -- TPD, 2005-12-23
  (postscript-restore-graphics-state stream)
  (format (postscript-stream-file-stream stream) "end~%showpage~%")
  (start-page stream)
  (postscript-save-graphics-state stream))


;;;; Output Protocol
(defmethod medium-drawable ((medium postscript-medium))
  (postscript-medium-file-stream medium))

(defmethod make-medium ((port postscript-port) (sheet postscript-stream))
  (make-instance 'postscript-medium :sheet sheet :port port))

(defmethod medium-miter-limit ((medium postscript-medium))
  #.(* pi (/ 11 180))) ; ?


(defmethod sheet-direct-mirror ((sheet postscript-stream))
  (postscript-stream-file-stream sheet))

(defmethod sheet-mirrored-ancestor ((sheet postscript-stream))
  sheet)

(defmethod sheet-mirror ((sheet postscript-stream))
  (sheet-direct-mirror sheet))

(defmethod realize-mirror ((port postscript-port) (sheet postscript-stream))
  (sheet-direct-mirror sheet))

(defmethod destroy-mirror ((port postscript-port) (sheet postscript-stream))
  (error "Can't destroy mirror for the postscript stream ~S." sheet))

;;; Some strange functions

(defmethod pane-viewport ((pane postscript-stream))
  nil)

(defmethod scroll-extent ((pane postscript-stream) x y)
  (declare (ignore x y))
  (values))

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

;;; Port

(setf (get :ps :port-type) 'postscript-port)
(setf (get :ps :server-path-parser) 'parse-postscript-server-path)

(defun parse-postscript-server-path (path)
  path)
