;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;; (c) copyright 2005 Aleksandar Bakic <a_bakic@yahoo.com>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Part of the Undo protocol that works with persistent buffers

(in-package #:drei-undo)

(defclass p-undo-mixin ()
  ((tree :initform (make-instance 'standard-undo-tree) :reader undo-tree)
   (undo-accumulate :initform '() :accessor undo-accumulate)
   (performing-undo :initform nil :accessor performing-undo)))

(defclass p-undo-record (climacs-undo-record)
  ((contents :initarg :contents)))

(defun save-p-undo-record (buffer)
  (unless (performing-undo buffer)
    (push (make-instance
           'p-undo-record
           :buffer buffer
           :contents (slot-value buffer 'drei-buffer::contents))
     (undo-accumulate buffer))))

(defmethod insert-buffer-object :before ((buffer p-undo-mixin) offset object)
  (declare (ignore offset object))
  (save-p-undo-record buffer))

(defmethod insert-buffer-sequence :before ((buffer p-undo-mixin) offset seq)
  (declare (ignore offset seq))
  (save-p-undo-record buffer))

(defmethod delete-buffer-range :before ((buffer p-undo-mixin) offset n)
  (declare (ignore offset n))
  (save-p-undo-record buffer))

(defmethod (setf buffer-object) :before (object (buffer p-undo-mixin) offset)
  (declare (ignore object offset))
  (save-p-undo-record buffer))

(defmethod flip-undo-record ((record p-undo-record))
  (with-slots (buffer contents) record
    (setf (slot-value buffer 'drei-buffer::contents) contents)
    (drei-buffer::filter-and-update
     (drei-buffer::cursors buffer)
     #'(lambda (c) (flexichain::weak-pointer-value c))
     #'(lambda (wpc)
         (setf (cursor-pos wpc)
               (max 0 (min (cursor-pos wpc) (1- (size buffer)))))))))
