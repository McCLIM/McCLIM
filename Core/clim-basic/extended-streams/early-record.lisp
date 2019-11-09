;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 1998,1999,2000,2001 by Michael McDonald (mikemac@mikemac.com)
;;;  (c) copyright 2000, 2014, 2016 by
;;;           Robert Strandh (robert.strandh@gmail.com)
;;;  (c) copyright 2001 by
;;;           Arnaud Rouanet (rouanet@emi.u-bordeaux.fr)
;;;           Lionel Salabartan (salabart@emi.u-bordeaux.fr)
;;;  (c) copyright 2001, 2002 by Alexey Dejneka (adejneka@comail.ru)
;;;  (c) copyright 2003 by Gilbert Baumann <unk6@rz.uni-karlsruhe.de>

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

;;; 16.2.1. The Basic Output Record Protocol (extras)

(defgeneric (setf output-record-parent) (parent record)
  (:documentation "Additional protocol generic function. PARENT may be
an output record or NIL."))

;;; 16.2.2. Output Record "Database" Protocol (extras)
;;; From the Franz CLIM user's guide but not in the spec... clearly necessary.

(defgeneric map-over-output-records-1
    (continuation record continuation-args))

(defun map-over-output-records
    (function record &optional (x-offset 0) (y-offset 0) &rest function-args)
  "Maps over all of the children of the RECORD, calling FUNCTION on
each one. It is a function of one or more arguments and called with
all of FUNCTION-ARGS as APPLY arguments."
  (declare (ignore x-offset y-offset))
  (map-over-output-records-1 function record function-args))

;;; Forward definition
(defclass stream-output-history-mixin ()
  ((stream :initarg :stream :reader output-history-stream)))

;;; 21.3 Incremental Redisplay Protocol.  These generic functions need
;;; to be implemented for all the basic displayed-output-records, so
;;; they are defined in this file.
;;;
;;; MATCH-OUTPUT-RECORDS and FIND-CHILD-OUTPUT-RECORD, as defined in
;;; the CLIM spec, are pretty silly.  How does incremental redisplay
;;; know what keyword arguments to supply to FIND-CHILD-OUTPUT-RECORD?
;;; Through a gf specialized on the type of the record it needs to
;;; match... why not define the search function and the predicate on
;;; two records then!
;;;
;;; We'll implement MATCH-OUTPUT-RECORDS and FIND-CHILD-OUTPUT-RECORD,
;;; but we won't actually use them.  Instead, output-record-equal will
;;; match two records, and find-child-record-equal will search for the
;;; equivalent record.

(defgeneric match-output-records (record &rest args))

;;; These gf's use :MOST-SPECIFIC-LAST because one of the least
;;; specific methods will check the bounding boxes of the records,
;;; which should cause an early out most of the time.

(defgeneric match-output-records-1 (record &key)
  (:method-combination and :most-specific-last))

(defgeneric output-record-equal (record1 record2)
  (:method-combination and :most-specific-last))

(defmethod output-record-equal :around (record1 record2)
  (cond ((eq record1 record2)
         ;; Some unusual record -- like a Goatee screen line -- might
         ;; exist in two trees at once
         t)
        ((eq (class-of record1) (class-of record2))
         (let ((result (call-next-method)))
           (if (eq result 'maybe)
               nil
               result)))
        (t nil)))

;;; A fallback method so that something's always applicable.

(defmethod output-record-equal and (record1 record2)
  (declare (ignore record1 record2))
  'maybe)

;;; The code for MATCH-OUTPUT-RECORDS-1 and OUTPUT-RECORD-EQUAL
;;; methods are very similar, hence this macro.  In order to exploit
;;; the similarities, it's necessary to treat the slots of the second
;;; record like variables, so for convenience the macro will use
;;; SLOT-VALUE on both records.

(defmacro defrecord-predicate (record-type slots &body body)
  "Each element of SLOTS is either a symbol or (:initarg-name slot-name)."
  (let* ((slot-names (mapcar #'(lambda (slot-spec)
                                 (if (consp slot-spec)
                                     (cadr slot-spec)
                                     slot-spec))
                             slots))
         (supplied-vars (mapcar #'(lambda (slot)
                                    (gensym (symbol-name
                                             (symbol-concat slot '#:-p))))
                                slot-names))
         (key-args (mapcar #'(lambda (slot-spec supplied)
                               `(,slot-spec nil ,supplied))
                           slots supplied-vars))
         (key-arg-alist (mapcar #'cons slot-names supplied-vars)))
    `(progn
       (defmethod output-record-equal and ((record ,record-type)
                                           (record2 ,record-type))
         (macrolet ((if-supplied ((var &optional (type t)) &body supplied-body)
                      (declare (ignore var type))
                      `(progn ,@supplied-body)))
           (with-slots ,slot-names record2
             ,@body)))
       (defmethod match-output-records-1 and ((record ,record-type)
                                              &key ,@key-args)
         (macrolet ((if-supplied ((var &optional (type t)) &body supplied-body)
                      (let ((supplied-var (cdr (assoc var ',key-arg-alist))))
                        (unless supplied-var
                          (error "Unknown slot ~S" var))
                        `(or (null ,supplied-var)
                             ,@(if (eq type t)
                                   `((progn ,@supplied-body))
                                   `((if (typep ,var ',type)
                                         (progn ,@supplied-body)
                                         (error 'type-error
                                                :datum ,var
                                                :expected-type ',type))))))))
           ,@body)))))

(defmacro with-output-recording-options ((stream
                                          &key (record nil record-supplied-p)
                                               (draw nil draw-supplied-p))
                                         &body body)
  (setq stream (stream-designator-symbol stream '*standard-output*))
  (with-gensyms (continuation)
    `(flet ((,continuation  (,stream)
              ,(declare-ignorable-form* stream)
              ,@body))
       (declare (dynamic-extent #',continuation))
       (with-drawing-options (,stream)
         (invoke-with-output-recording-options
          ,stream #',continuation
          ,(if record-supplied-p record `(stream-recording-p ,stream))
          ,(if draw-supplied-p draw `(stream-drawing-p ,stream)))))))

;;; Macro masturbation...

(defmacro define-invoke-with (macro-name func-name record-type doc-string)
  `(defmacro ,macro-name ((stream
                           &optional
                           (record-type '',record-type)
                           (record (gensym))
                           &rest initargs)
                          &body body)
     ,doc-string
     (setq stream (stream-designator-symbol stream '*standard-output*))
     (with-gensyms (continuation)
       (multiple-value-bind (bindings m-i-args)
           (rebind-arguments initargs)
         `(let ,bindings
            (flet ((,continuation (,stream ,record)
                     ,(declare-ignorable-form* stream record)
                     ,@body))
              (declare (dynamic-extent #',continuation))
              (,',func-name ,stream #',continuation ,record-type ,@m-i-args)))))))

(define-invoke-with with-new-output-record invoke-with-new-output-record
  standard-sequence-output-record
  "Creates a new output record of type RECORD-TYPE and then captures
the output of BODY into the new output record, and inserts the new
record into the current \"open\" output record assotiated with STREAM.
    If RECORD is supplied, it is the name of a variable that will be
lexically bound to the new output record inside the body. INITARGS are
CLOS initargs that are passed to MAKE-INSTANCE when the new output
record is created.
    It returns the created output record.
    The STREAM argument is a symbol that is bound to an output
recording stream. If it is T, *STANDARD-OUTPUT* is used.")

(define-invoke-with with-output-to-output-record
    invoke-with-output-to-output-record
  standard-sequence-output-record
  "Creates a new output record of type RECORD-TYPE and then captures
the output of BODY into the new output record. The cursor position of
STREAM is initially bound to (0,0)
    If RECORD is supplied, it is the name of a variable that will be
lexically bound to the new output record inside the body. INITARGS are
CLOS initargs that are passed to MAKE-INSTANCE when the new output
record is created.
    It returns the created output record.
    The STREAM argument is a symbol that is bound to an output
recording stream. If it is T, *STANDARD-OUTPUT* is used.")
