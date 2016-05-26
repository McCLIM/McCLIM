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

(defun describe (thing &optional stream)
  (if (null stream)
      (setq stream *standard-output*)
      (if  (eq stream t)
           (setq stream *terminal-io*)))
  (describe-object thing stream)
  (values))


(defgeneric describe-object (thing stream))

;;; For these methods, stream should be of type
;;; (or EXTENDED-OUTPUT-STREAM OUTPUT-RECORDING-STREAM)
;;; but CLIM-STREAM-PANE is used instead.

(clim-internals::with-system-redefinition-allowed
  (defmethod describe-object ((thing t) stream)
    (let ((*print-array* nil))
      (clim:present thing (clim:presentation-type-of thing)
		    :stream stream)
      (format stream " is of type ")
      (clim:present (type-of thing) (clim:presentation-type-of (type-of thing))
		    :stream stream)
      (terpri stream)))
)

(defmethod describe-object ((thing symbol) stream)
  (clim:present thing (clim:presentation-type-of thing)
		:stream stream)
  (format stream " is of type ")
  (clim:present (type-of thing) (clim:presentation-type-of (type-of thing))
		:stream stream)
  (terpri stream)
  (cond
   ((not (boundp thing))
    (format stream "   it is unbound~%"))
   (t
    (format stream "   it has a value of ")
    (clim:present (symbol-value thing) (clim:presentation-type-of (symbol-value thing))
		  :stream stream)
    (terpri)))
  (format stream "   it is in the ")
  (clim:present (symbol-package thing) (clim:presentation-type-of (symbol-package thing))
		:stream stream)
  (format stream " package~%")
  (when (fboundp thing)
    (format stream "   it has a function definition of ~S~%" (symbol-function thing))
    (format stream "      which has the argument list ")
    (let ((arglist #+excl (excl:arglist (symbol-function thing))
                   #+cmu (kernel:%function-arglist (symbol-function thing))
                   #+sbcl (sb-kernel:%simple-fun-arglist (symbol-function thing))
                   #+clisp (ext:arglist (symbol-function thing))
                   #-(or excl cmu sbcl clisp) "( ??? )"))
      (when arglist
        (clim:present arglist
                      (clim:presentation-type-of arglist)
                      :stream stream)))
    (terpri))
  (format stream "   it has a property list of ~S~%" (symbol-plist thing)))

(defmethod describe-object ((thing number) stream)
  (clim:present thing (clim:presentation-type-of thing)
		:stream stream)
  (format stream " is a number of type ")
  (clim:present (type-of thing) (clim:presentation-type-of (type-of thing))
		:stream stream)
  (terpri stream))

(defmethod describe-object ((thing string) stream)
  (clim:present thing (clim:presentation-type-of thing)
		:stream stream)
  (format stream " is of type ")
  (clim:present (type-of thing) (clim:presentation-type-of (type-of thing))
		:stream stream)
  (format stream " with a length of ")
  (clim:present (length thing) 'clim:integer
		:stream stream)
  (terpri stream))

(defmethod describe-object ((thing package) stream)
  (clim:present thing (clim:presentation-type-of thing)
		:stream stream)
  (format stream " is a package named ")
  (clim:present (package-name thing) (clim:presentation-type-of (package-name thing))
		:stream stream)
  (terpri stream)
  (format stream "   it has the nicknames of ")
  (clim:present (package-nicknames thing) 'clim:expression
		:stream stream)
  (terpri stream)
  (format stream "   it uses these packages: ")
  (clim:present (package-use-list thing) 'clim:expression
		:stream stream)
  (terpri stream)
  (format stream "   it is used by the packages: ")
  (clim:present (package-used-by-list thing) 'clim:expression
		:stream stream)
  (terpri stream))


(labels ((present-instance-slots-text (thing stream)
           (let* ((slots (clim-mop:class-slots (class-of thing)))
                  (width (loop for slot in slots
                               maximizing (length (symbol-name (clim-mop:slot-definition-name slot))))))
             (loop for slot in slots
                   do (cond
                        ((slot-boundp thing (clim-mop:slot-definition-name slot))
                         (format stream "      ~v@A: " width
                                 (clim-mop:slot-definition-name slot))
                         (clim:present (slot-value thing (clim-mop:slot-definition-name slot))
                                       'clim:expression
                                       :stream stream)
                         (terpri stream))
                        (t
                         (format stream "      ~v@A: <unbound>~%" width
                                 (clim-mop:slot-definition-name slot)))))))
       
         (present-instance-slots-clim (thing stream)
           (let ((slots (clim-mop:class-slots (class-of thing))))
             (clim:formatting-table (stream)
               (dolist (slot slots)
                 (clim:formatting-row (stream)
                   (clim:formatting-cell (stream :align-x :right)
                     (clim:present (clim-mop:slot-definition-name slot)
                                   'clim:symbol
                                   :stream stream)
                     (write-char #\: stream))
                   (clim:formatting-cell (stream)
                     (if (slot-boundp thing (clim-mop:slot-definition-name slot))
                         (clim:present (slot-value thing (clim-mop:slot-definition-name slot))
                                       'clim:expression
                                       :stream stream)
                         (format stream "<unbound>"))))))))
         
         (describe-instance (thing a-what stream)  
           (clim:present thing (clim:presentation-type-of thing)
                         :stream stream)
           (format stream " is ~A of type " a-what)          
           (clim:present (type-of thing) (clim:presentation-type-of (type-of thing))
                         :stream stream)
           (terpri stream)
           (format stream "   it has the following slots:~%")
           (if (typep stream 'clim:output-recording-stream)
               (present-instance-slots-clim thing stream)
               (present-instance-slots-text thing stream))))
  
  (defmethod describe-object ((thing standard-object) stream)
    (describe-instance thing "an instance" stream))
  
  (defmethod describe-object ((thing structure-object) stream)
    (describe-instance thing "a structure" stream)))
