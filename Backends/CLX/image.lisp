;;   (c) copyright 2002 by
;;;           Joachim Pouderoux (pixel@pixeledena.com)
;;;  (c) copyright 2001 by 
;;;           Robert Strandh (strandh@labri.u-bordeaux.fr)

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

(defpackage :image
; (:use #:clim-lisp)
  (:use :clim-clx :common-lisp)
  (:export
   #:write-pnm #:read-image-file
   #:image #:image-color #:image-gadget #:image-height
   #:image-pixel #:image-pixels #:image-width
   #:rgb-image
   #:gray-level-image #:256-gray-level-image #:make-256-gray-level-image
   #:gray-image-min-level #:gray-image-min-level
   #:truecolor-image #:make-truecolor-image #:make-3x256-color-image
   #:color-image-min-level #:color-image-max-level
   #:binary-image #:make-binary-image
   #:red-component #:green-component #:blue-component
   #:colormap-image
   #:spectral-image))

(in-package :image)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; image

(defclass image () ())

(defgeneric image-width (image))
(defgeneric image-height (image))
(defgeneric image-pixels (image))
(defgeneric image-pixel (image x y))
(defgeneric (setf image-pixel) (x y pixel image))
(defgeneric image-color (image x y))
(defgeneric (setf image-color) (x y pixel color image))
(defgeneric write-pnm (image filename output-format))

(defmethod image-width ((image image))
  (cadr (array-dimensions (image-pixels image))))

(defmethod image-height ((image image))
  (car (array-dimensions (image-pixels image))))

(defmethod image-pixel ((image image) x y)
  (aref (image-pixels image) y x))

(defmethod (setf image-pixel) (x y pixel image)
  (setf (aref (image-pixels image) y x) pixel))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; spectral image

(defclass spectral-image (image) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; rgb image

(defclass rgb-image (image) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; truecolor image

(defclass truecolor-image (rgb-image)
  ((pixels :initarg :pixels
           :type (simple-array (unsigned-byte 24) (* *))
           :reader image-pixels)
   (max-level :initarg :max-level :type xlib:card8 :reader image-max-level)))

(defun make-truecolor-image (pixels max-value)
  (make-instance 'truecolor-image :pixels pixels :max-level max-value))

(defmethod color-image-max-level ((image truecolor-image))
  (image-max-level image))

(defmethod color-image-min-level ((image truecolor-image))
  0)

(defun make-3x256-color-image (pixels)
  (make-instance 'truecolor-image :pixels pixels :max-level 255))

(defmacro red-component (pixel)
  `(the (unsigned-byte 8) (logand (ash ,pixel -16) 255)))

(defmacro green-component (pixel)
  `(the (unsigned-byte 8) (logand (ash ,pixel -8) 255)))

(defmacro blue-component (pixel)
  `(the (unsigned-byte 8) (logand ,pixel 255)))

(defmethod write-pnm ((image truecolor-image) filename output-format)
  (with-open-file (stream filename
		   :direction :output :if-exists :supersede
		   :element-type '(unsigned-byte 8))
    (if (eq output-format :ascii)
	(write-ppm-p3 stream (image-pixels image))
	(write-ppm-p6 stream (image-pixels image)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; colormap image

(defclass colormap-image (rgb-image) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; gray-level image

(defclass gray-level-image (colormap-image)
  ())

(defgeneric gray-image-max-level (gray-level-image))
(defgeneric gray-image-min-level (gray-level-image))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 256 gray-level image

(defclass 256-gray-level-image (gray-level-image)
  ((pixels :initarg :pixels
           :type (simple-array (unsigned-byte 8) (* *))
           :reader image-pixels)))

(defun make-256-gray-level-image (pixels)
  (make-instance '256-gray-level-image :pixels pixels))

(defmethod gray-image-max-level ((image 256-gray-level-image))
  255)

(defmethod gray-image-min-level ((image 256-gray-level-image))
  0)

(defmethod write-pnm ((image 256-gray-level-image) filename output-format)
  (with-open-file (stream filename
		   :direction :output :if-exists :supersede
		   :element-type '(unsigned-byte 8))
    (if (eq output-format :ascii)
	(write-pgm-p2 stream (image-pixels image))
	(write-pgm-p5 stream (image-pixels image)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; binary image

(defclass binary-image (gray-level-image)
  ((pixels :initarg :pixels
           :type (simple-array bit (* *))
           :reader image-pixels)))

(defun make-binary-image (pixels)
  (make-instance 'binary-image :pixels pixels))

(defmethod write-pnm ((image binary-image) filename output-format)
  (with-open-file (stream filename
		   :direction :output :if-exists :supersede
		   :element-type '(unsigned-byte 8))
    (if (eq output-format :ascii)
	(write-pbm-p1 stream (image-pixels image))
	(write-pbm-p4 stream (image-pixels image)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; PNM Image writers

(defmacro with-write-pnm-loop ((magic-number max-value) &body body)
  `(let ((height (car (array-dimensions picture)))
         (width (cadr (array-dimensions picture))))
     (map nil (lambda (x) (write-byte (char-code x) stream))
          (format nil "P~A~%~A~%~A~%~@[~A~%~]"
                  ,magic-number width height ,max-value))
     (loop for r from 0 below height do
          (loop for c from 0 below width do
               ,@body))
     nil))

(defun write-pbm-p1 (stream picture)
  (with-write-pnm-loop (1 nil)
    (map nil (lambda (x) (write-byte (char-code x) stream))
	 (format nil "~A~%" (aref picture r c)))))

(defun write-pbm-p4 (stream picture) ; bad!
  (with-write-pnm-loop (4 nil)
    (write-byte (aref picture r c) stream)))

(defun write-pgm-p2 (stream picture)
  (with-write-pnm-loop (2 255)
    (map nil (lambda (x) (write-byte (char-code x) stream))
	 (format nil "~A~%" (aref picture r c)))))

(defun write-pgm-p5 (stream picture)
  (with-write-pnm-loop (5 255)
    (write-byte (aref picture r c) stream)))

(defun write-ppm-p3 (stream picture)
  (with-write-pnm-loop (3 255)
    (let ((rgb (aref picture r c)))
      (map nil (lambda (x) (write-byte (char-code x) stream))
	   (format nil "~A ~A ~A~%"
		   (red-component rgb)
		   (green-component rgb)
		   (blue-component rgb))))))

(defun write-ppm-p6 (stream picture)
  (with-write-pnm-loop (6 255)
    (let ((rgb (aref picture r c)))
      (write-byte (red-component rgb) stream)
      (write-byte (green-component rgb) stream)
      (write-byte (blue-component rgb) stream))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; PNM Image readers

(defun skip-line (stream)
  (loop while (/= (read-byte stream) #.(char-code #\newline))))

(defmacro skip-whitespace-and-comments ()
  `(loop while (member byte '#.(mapcar #'char-code
                                       '(#\Space #\Tab #\Newline #\#)))
         do (if (= byte #.(char-code #\#))
                (loop while (/= byte #.(char-code #\Newline))
                      do (setf byte (read-byte stream)))
                (setf byte (read-byte stream)))))

(defmacro read-number (var)
  `(progn (assert (<= 48 byte 57))
          (setf ,var (- byte 48))
          (loop while (<= 48 (setf byte (read-byte stream)) 57)
                do (setf ,var (+ (* ,var 10) (- byte 48))))))

(defmacro with-pnm-header (read-max-value &body body)
  `(let ((byte (read-byte stream))
	 (width 0)
	 (height 0)
	 (max-value 0))
     (declare (type fixnum width height max-value))
     (skip-whitespace-and-comments)
     (read-number width)
     (skip-whitespace-and-comments)
     (read-number height)
     (when ,read-max-value
       (skip-whitespace-and-comments)
       (read-number max-value))
     ,@body))

(defmacro with-pnm-ascii-reader (read-max-value element-type &body body)
  (let ((result (gensym)))
    `(with-pnm-header ,read-max-value
       (loop with size of-type fixnum = (* width height)
	     with ,result = (make-array `(,height ,width) :element-type ',element-type)
	     with vec = (make-array `(,size)
				    :element-type ',element-type
				    :displaced-to ,result)
	     for offset of-type fixnum from 0 below size
	     do ,@body
	     finally (return ,result)))))

(defun read-pbm-p1 (stream)
  (declare (optimize (speed 3)))
  (with-pnm-ascii-reader nil bit
    (let ((color 0))
      (skip-whitespace-and-comments)
      (read-number color)
      (setf (aref vec offset) color))))

(defun read-pgm-p2 (stream)
  (declare (optimize (speed 3)))
  (with-pnm-ascii-reader nil (unsigned-byte 8)
    (let ((color 0))
      (skip-whitespace-and-comments)
      (read-number color)
      (setf (aref vec offset) color))))

(defun read-ppm-p3 (stream)
  (declare (optimize (speed 3)))
  (with-pnm-ascii-reader nil (unsigned-byte 24)
    (let ((r 0)
	  (g 0)
	  (b 0))
      (skip-whitespace-and-comments)
      (read-number r)
      (skip-whitespace-and-comments)
      (read-number g)
      (skip-whitespace-and-comments)
      (read-number b)
      (setf (aref vec offset) (the (unsigned-byte 24)
				(+ (ash (the (unsigned-byte 8) r) 16)
				   (ash (the (unsigned-byte 8) g) 8)
				   (the (unsigned-byte 8) b)))))))

(defun read-pbm-p4 (stream)
  (with-pnm-header nil
    (loop with result = (make-array `(,height ,width) :element-type 'bit)
	  with bytes-per-row = (ceiling width 8)
	  for r from 0 below height
	  do (loop for cr from 0 below bytes-per-row
		   do (loop with byte = (read-byte stream)
			    for pos from 7 downto 0
			    for c from (* cr 8) below (min (* 8 (1+ cr)) width)
			    do (setf (aref result r c) (ldb (byte 1 pos) byte))))
	  finally (return result))))

(defun read-pgm-p5 (stream)
  (with-pnm-header t
    (loop with size of-type fixnum = (* width height)
	  with result = (make-array `(,height ,width) :element-type '(unsigned-byte 8))
	  with vec = (make-array `(,size)
				 :element-type '(unsigned-byte 8)
				 :displaced-to result)
	  with offset of-type fixnum = 0
	  while (< offset size)
	  do (setf offset (read-sequence vec stream :start offset))
	  finally (return result))))

(defun read-ppm-p6 (stream)
  (declare (optimize (speed 3)))
  (with-pnm-header t
    (loop with size of-type fixnum = (* width height)
	  with cache-size of-type fixnum = (the fixnum (min size 21000))
	  with aux = (make-array (* 3 cache-size) :element-type '(unsigned-byte 8))
	  for start of-type fixnum from 0 by cache-size below size
	  for end of-type fixnum = (min (+ start cache-size) size)
	  with result = (make-array `(,height ,width) :element-type `(unsigned-byte 24))
	  with vec = (make-array size :element-type `(unsigned-byte 24)
				 :displaced-to result)
	  do (loop with offset = 0
		   while (< offset (* 3 (- end start)))
		   do (setf offset (read-sequence aux stream :start offset)))
	  (loop for i of-type fixnum from start below end
		for j of-type fixnum from 0 by 3
		do (setf (aref vec i)
			 (the (unsigned-byte 24)
			   (+ (ash (the (unsigned-byte 8) (aref aux j)) 16)
			      (ash (the (unsigned-byte 8) (aref aux (1+ j))) 8)
			      (the (unsigned-byte 8) (aref aux (+ 2 j)))))))
	  finally (return result))))

(defun read-pnm-file (filename)
  (with-open-file (stream filename :direction :input :element-type '(unsigned-byte 8))
    (let ((byte1 (read-byte stream)))
      (cond ((= byte1 (char-code #\P))
	     ;; probably a PNM file
	     (let ((byte2 (read-byte stream)))
	       (case byte2
		 ((#.(char-code #\1)) (read-pbm-p1 stream))
		 ((#.(char-code #\4)) (read-pbm-p4 stream))
		 ((#.(char-code #\2)) (read-pgm-p2 stream))
		 ((#.(char-code #\5)) (read-pgm-p5 stream))
		 ((#.(char-code #\3)) (read-ppm-p3 stream)) ; ASCII
		 ((#.(char-code #\6)) (read-ppm-p6 stream)) ; Binary
		 (t (error "unknown file format ~A ~A" byte1 byte2)))))
	    (t (error "unknown file format ~A" byte1))))))

(defun read-image-file (filename &key (format :pnm))
  (declare (ignore format))
  (read-pnm-file filename))
