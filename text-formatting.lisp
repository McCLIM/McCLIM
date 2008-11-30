;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 2002 by Alexey Dejneka (adejneka@comail.ru)

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

(in-package :clim-internals)

(defun format-textual-list (sequence printer
                            &key stream separator conjunction
                            suppress-separator-before-conjunction
                            suppress-space-after-conjunction)
  "Outputs the SEQUENCE of items as a \"textual list\" into
STREAM. PRINTER is a function of an item and a stream. Between each
two items the string SEPARATOR is placed. If the string CONJUCTION is
supplied, it is placed before the last item.

SUPPRESS-SEPARATOR-BEFORE-CONJUNCTION and
SUPPRESS-SPACE-AFTER-CONJUNCTION are non-standard."
  (orf stream *standard-output*)
  (orf separator ", ")
  (let* ((length (length sequence))
         (n-rest length))
    (map-repeated-sequence nil 1
                           (lambda (item)
                             (funcall printer item stream)
                             (decf n-rest)
                             (cond ((> n-rest 1)
                                    (princ separator stream))
                                   ((= n-rest 1)
                                    (if conjunction
                                        (progn
                                          (unless suppress-separator-before-conjunction
                                            (princ separator stream))
                                          (princ conjunction stream)
                                          (unless suppress-space-after-conjunction
                                            (princ #\space stream)))
                                        (princ separator stream)))))
                           sequence)))

;;; filling-output support

(defclass filling-stream (standard-encapsulating-stream
			  extended-output-stream
			  output-recording-stream)
  ((fill-width :accessor fill-width :initarg :fill-width)
   (break-characters :accessor break-characters :initarg :break-characters
		     :initform '(#\Space))
   (after-line-break :accessor after-line-break :initarg :after-line-break)))

;;; parse-space is from table-formatting.lisp

(defmethod initialize-instance :after ((obj filling-stream)
				       &key (fill-width '(80 :character)))
  (setf (fill-width obj) (parse-space (encapsulating-stream-stream obj)
				      fill-width
				      :horizontal)))

(defmethod stream-write-char :around ((stream filling-stream) char)
  (let ((under-stream (encapsulating-stream-stream stream)))
    (if (and (member char (break-characters stream) :test #'char=)
	     (> (stream-cursor-position under-stream) (fill-width stream)))
	(progn
	  (stream-write-char under-stream #\newline)
	  (when (slot-boundp stream 'after-line-break)
	    (write-string (after-line-break stream)
			  (encapsulating-stream-stream stream))))
	(call-next-method))))

(defmethod stream-write-string :around ((stream filling-stream) string
                                        &optional (start 0) end)
  (dotimes (i (- (or end (length string)) start))
    (stream-write-char stream (aref string (+ i start)))))

;;; All the monkey business with the lambda form has to do with capturing the
;;; keyword arguments of the macro while preserving the user's evaluation order.

(defmacro filling-output ((stream &rest args &key fill-width break-characters
				  after-line-break after-line-break-initially)
			  &body body)
  (when (eq stream t)
    (setq stream '*standard-output*))
  (with-gensyms (fill-var break-var after-var initially-var)
    `((lambda (&key ((:fill-width ,fill-var))
	       ((:break-characters ,break-var))
	       ((:after-line-break ,after-var))
	       ((:after-line-break-initially ,initially-var)))
        (declare (ignorable ,fill-var ,break-var ,after-var ,initially-var))
	(let ((,stream (make-instance
			'filling-stream
			:stream ,stream
			,@(and fill-width `(:fill-width ,fill-var))
			,@(and break-characters
			       `(:break-characters ,break-var))
			,@(and after-line-break
			       `(:after-line-break ,after-var)))))
	  ,(unless (null after-line-break-initially)
             `(when ,initially-var
                (write-string ,after-var ,stream)))
	  ,@body))
      ,@args)))

;;; indenting-output

(defclass indenting-output-stream (standard-encapsulating-stream
				   extended-output-stream
				   output-recording-stream)
  ((indentation :accessor indentation)))

(defmethod initialize-instance :after ((obj indenting-output-stream)
				       &key (indent-spec 0) &allow-other-keys)
  (setf (indentation obj) (parse-space (encapsulating-stream-stream obj)
				       indent-spec
				       :horizontal)))

(defmethod stream-write-char :around ((stream indenting-output-stream) char)
  (let ((under-stream (encapsulating-stream-stream stream)))
    (when (stream-start-line-p under-stream)
      (stream-increment-cursor-position under-stream (indentation stream) nil))
    (call-next-method)))

(defmethod stream-write-string :around ((stream indenting-output-stream)
				string &optional (start 0) end)
  (let ((under-stream (encapsulating-stream-stream stream))
	(end (or end (length string))))
    (flet ((foo (start end)
	     (when (stream-start-line-p under-stream)
	       (stream-increment-cursor-position under-stream (indentation stream) nil))
	     (stream-write-string under-stream string start end)))
      (let ((seg-start start))
	(loop for i from start below end do
	  (when (char= #\Newline
		       (char string i))
	    (foo seg-start (1+ i))
	    (setq seg-start (1+ i))))
	(foo seg-start end)))))
		     
(defmacro indenting-output ((stream indent &key (move-cursor t)) &body body)
  (when (eq stream t)
    (setq stream '*standard-output*))
  (with-gensyms (old-x old-y)
     `(multiple-value-bind (,old-x ,old-y)
	  (stream-cursor-position ,stream)	
	(let ((,stream (make-instance
		       'indenting-output-stream
		       :stream ,stream
		       :indent-spec ,indent)))
	  ,@body)
	(unless ,move-cursor
	  (setf (stream-cursor-position ,stream)
		(values ,old-x ,old-y))))))
