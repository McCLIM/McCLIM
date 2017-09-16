;;; -*- Mode: Lisp; Package: COMMON-LISP -*-

;;;  (c) copyright 2002 by Michael McDonald (mikemac@mikemac.com)

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

(in-package :clim-lisp)

(defun describe (object &optional stream)
  (let ((stream (case stream
                  ((nil) *standard-output*)
                  ((t)   *terminal-io*)
                  (t     stream))))
    (describe-object object stream))
  (values))

(defgeneric describe-object (object stream))

;;; For these methods, stream should be of type
;;; (or EXTENDED-OUTPUT-STREAM OUTPUT-RECORDING-STREAM)
;;; but CLIM-STREAM-PANE is used instead.

(clim-internals::with-system-redefinition-allowed
    (defmethod describe-object ((object t) stream)
      (let ((*print-array* nil))
        (clim:present object (clim:presentation-type-of object)
                      :stream stream)
        (format stream " is of type ")
        (clim:present (type-of object) (clim:presentation-type-of (type-of object))
                      :stream stream)
        (terpri stream))))

(defmethod describe-object ((object symbol) stream)
  (clim:present object (clim:presentation-type-of object)
                :stream stream)
  (format stream " is of type ")
  (clim:present (type-of object) (clim:presentation-type-of (type-of object))
                :stream stream)
  (terpri stream)
  (cond
   ((not (boundp object))
    (format stream "   it is unbound~%"))
   (t
    (format stream "   it has a value of ")
    (clim:present (symbol-value object) (clim:presentation-type-of (symbol-value object))
                  :stream stream)
    (terpri)))
  (format stream "   it is in the ")
  (clim:present (symbol-package object) (clim:presentation-type-of (symbol-package object))
                :stream stream)
  (format stream " package~%")
  (when (fboundp object)
    (format stream "   it has a function definition of ~S~%" (symbol-function object))
    (format stream "      which has the argument list ")
    (let ((arglist #+excl (excl:arglist (symbol-function object))
                   #+cmu (kernel:%function-arglist (symbol-function object))
                   #+sbcl (sb-introspect:function-lambda-list (symbol-function object))
                   #+clisp (ext:arglist (symbol-function object))
                   #-(or excl cmu sbcl clisp) "( ??? )"))
      (when arglist
        (clim:present arglist
                      (clim:presentation-type-of arglist)
                      :stream stream)))
    (terpri))
  (format stream "   it has a property list of ~S~%" (symbol-plist object)))

(defmethod describe-object ((object number) stream)
  (clim:present object (clim:presentation-type-of object)
                :stream stream)
  (format stream " is a number of type ")
  (clim:present (type-of object) (clim:presentation-type-of (type-of object))
                :stream stream)
  (terpri stream))

(defmethod describe-object ((object string) stream)
  (clim:present object (clim:presentation-type-of object)
                :stream stream)
  (format stream " is of type ")
  (clim:present (type-of object) (clim:presentation-type-of (type-of object))
                :stream stream)
  (format stream " with a length of ")
  (clim:present (length object) 'clim:integer
                :stream stream)
  (terpri stream))

(defmethod describe-object ((object package) stream)
  (clim:present object (clim:presentation-type-of object)
                :stream stream)
  (format stream " is a package named ")
  (clim:present (package-name object) (clim:presentation-type-of (package-name object))
                :stream stream)
  (terpri stream)
  (format stream "   it has the nicknames of ")
  (clim:present (package-nicknames object) 'clim:expression
                :stream stream)
  (terpri stream)
  (format stream "   it uses these packages: ")
  (clim:present (package-use-list object) 'clim:expression
                :stream stream)
  (terpri stream)
  (format stream "   it is used by the packages: ")
  (clim:present (package-used-by-list object) 'clim:expression
                :stream stream)
  (terpri stream))

(labels ((present-instance-slots-text (object stream)
           (let* ((slots (c2mop:class-slots (class-of object)))
                  (width (loop for slot in slots
                               maximizing (length (symbol-name (c2mop:slot-definition-name slot))))))
             (loop for slot in slots
                   do (cond
                        ((slot-boundp object (c2mop:slot-definition-name slot))
                         (format stream "      ~v@A: " width
                                 (c2mop:slot-definition-name slot))
                         (clim:present (slot-value object (c2mop:slot-definition-name slot))
                                       'clim:expression
                                       :stream stream)
                         (terpri stream))
                        (t
                         (format stream "      ~v@A: <unbound>~%" width
                                 (c2mop:slot-definition-name slot)))))))

         (present-instance-slots-clim (object stream)
           (let ((slots (c2mop:class-slots (class-of object))))
             (clim:formatting-table (stream)
               (dolist (slot slots)
                 (clim:formatting-row (stream)
                   (clim:formatting-cell (stream :align-x :right)
                     (clim:present (c2mop:slot-definition-name slot)
                                   'clim:symbol
                                   :stream stream)
                     (write-char #\: stream))
                   (clim:formatting-cell (stream)
                     (if (slot-boundp object (c2mop:slot-definition-name slot))
                         (clim:present (slot-value object (c2mop:slot-definition-name slot))
                                       'clim:expression
                                       :stream stream)
                         (format stream "<unbound>"))))))))

         (describe-instance (object a-what stream)
           (clim:present object (clim:presentation-type-of object)
                         :stream stream)
           (format stream " is ~A of type " a-what)
           (clim:present (type-of object) (clim:presentation-type-of (type-of object))
                         :stream stream)
           (terpri stream)
           (format stream "   it has the following slots:~%")
           (if (typep stream 'clim:output-recording-stream)
               (present-instance-slots-clim object stream)
               (present-instance-slots-text object stream))))

  (defmethod describe-object ((object standard-object) stream)
    (describe-instance object "an instance" stream))

  (defmethod describe-object ((object structure-object) stream)
    (describe-instance object "a structure" stream)))
