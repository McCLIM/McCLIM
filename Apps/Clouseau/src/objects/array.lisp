;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2018-2021 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Places and inspection methods for vectors and other arrays.

(cl:in-package #:clouseau)

;;; Utilities

(defun adjust-keeping-displacement (array dimensions &rest args)
  (multiple-value-bind (to index-offset) (array-displacement array)
    (apply #'adjust-array array dimensions
           :displaced-to           to
           :displaced-index-offset index-offset
           args)))

;;; Places

;;; `array-dimensions-place'

(defclass array-dimensions-place (basic-place)
  ())

(defmethod supportsp ((place array-dimensions-place) (operation (eql 'setf)))
  (adjustable-array-p (container place)))

(defmethod value ((place array-dimensions-place))
  (array-dimensions (container place)))

(defmethod (setf value) ((new-value t) (place array-dimensions-place))
  (adjust-keeping-displacement (container place) new-value))

;;; `vector-bound-place'

(defclass vector-bound-place (basic-place)
  ())

(defmethod accepts-value-p ((place vector-bound-place) (value t))
  (typep value '(integer 0 (#.array-total-size-limit))))

;;; `vector-total-size-place'

(defclass vector-total-size-place (vector-bound-place)
  ())

(defmethod supportsp ((place vector-total-size-place) (operation (eql 'setf)))
  (adjustable-array-p (container place)))

(defmethod value ((place vector-total-size-place))
  (array-total-size (container place)))

(defmethod (setf value) ((new-value integer) (place vector-total-size-place))
  (let ((vector (container place)))
    (when (array-has-fill-pointer-p vector)
      (minf (fill-pointer vector) (max 0 (1- new-value))))
    (adjust-keeping-displacement vector new-value)))

;;; `vector-fill-pointer-place'

(defclass vector-fill-pointer-place (vector-bound-place)
  ())

(defmethod accepts-value-p ((place vector-fill-pointer-place) (value t))
  (and (call-next-method)
       (<= value (array-total-size (container place)))))

(defmethod value ((place vector-fill-pointer-place))
  (fill-pointer (container place)))

(defmethod (setf value) ((new-value integer) (place vector-fill-pointer-place))
  (setf (fill-pointer (container place)) new-value))

;;; `array-element-place'

(defclass array-element-place (sequence-element-place)
  ())

(defmethod accepts-value-p ((place array-element-place) (value t))
  (typep value (array-element-type (container place))))

(defmethod value ((place array-element-place))
  (row-major-aref (container place) (cell place)))

(defmethod (setf value) (new-value (place array-element-place))
  (setf (row-major-aref (container place) (cell place)) new-value))

;;; `vector-element-place'

(defclass vector-element-place (array-element-place)
  ())

(define-presentation-method present ((object vector-element-place)
                                     (type   place)
                                     (stream t)
                                     (view   inspector-view)
                                     &key)
  (prin1 (cell object) stream))

;;; `adjustable-vector-element-place'

(defclass adjustable-vector-element-place (vector-element-place)
  ())

(defmethod supportsp ((place     adjustable-vector-element-place)
                      (operation (eql 'remove-value)))
  t)

(defmethod remove-value ((place adjustable-vector-element-place))
  (let* ((container (container place))
         (length    (array-total-size container))
         (index     (cell place)))
    (when (array-has-fill-pointer-p container)
      (minf (fill-pointer container) (1- length)))
    (when (> length 1)
      (loop :for i :from index :to (- length 2)
            :do (setf (aref container i) (aref container (1+ i)))))
    (adjust-array container (1- length))))

;;; Object states

(defclass inspected-vector (inspected-array
                            inspected-sequence)
  ())

(defmethod object-state-class ((object vector) (place t))
  'inspected-vector)

(defclass inspected-array (inspected-identity-object-mixin
                           inspected-object)
  ())

(defmethod object-state-class ((object array) (place t))
  'inspected-array)

;;; Object inspection methods

;;; For all arrays

(defmethod inspect-object-using-state :after ((object array)
                                              (state  inspected-array)
                                              (style  (eql :badges))
                                              (stream t))
  (when (adjustable-array-p object)
    (write-char #\Space stream)
    (badge stream "adjustable")))

(defun inspect-element-type-and-total-size (object stream)
  (formatting-row (stream)
    (format-place-cells stream object 'reader-place 'array-element-type
                        :label "Element type")
    (multiple-value-bind (class cell)
        (if (vectorp object)
            (values 'vector-total-size-place nil)
            (values 'reader-place            'array-total-size))
      (format-place-cells stream object class cell :label "Total size"))))

(defun inspect-displacement (object stream)
  (multiple-value-bind (to index-offset) (array-displacement object)
    (when to
      (formatting-row (stream)
        (format-place-cells stream object 'pseudo-place to ; TODO writable if adjustable
                            :label "Displaced to")
        (format-place-cells stream object 'pseudo-place index-offset ; TODO writable if adjustable
                            :label "Displaced index offset")))))

;;; Vectors

(defmethod inspect-object-using-state ((object string)
                                       (state  inspected-array)
                                       (style  (eql :collapsed))
                                       (stream t))
  (print-string-compactly object stream))

(defmethod inspect-object-using-state ((object vector)
                                       (state  inspected-vector)
                                       (style  (eql :expanded-header))
                                       (stream t))
  (inspect-class-as-name (class-of object) stream)
  (write-char #\Space stream)
  (with-output-as-presentation (stream state 'sequence-range)
    (print-sequence-header stream (array-total-size object) (start state) (end state)))) ; TODO if inspect-sequence could take length from state, we wouldn't need this

(defmethod inspect-object-using-state ((object vector)
                                       (state  inspected-vector)
                                       (style  (eql :expanded-body))
                                       (stream t))
  (let ((length       (array-total-size object))
        (fill-pointer (when (array-has-fill-pointer-p object)
                        (fill-pointer object))))
    (with-preserved-cursor-x (stream)
      (formatting-table (stream)
        (inspect-element-type-and-total-size object stream)
        (inspect-displacement object stream)
        (when fill-pointer
          (format-place-row stream object 'vector-fill-pointer-place nil
                            :label "Fill pointer"))))
    (with-section (stream) "Elements"
      (with-placeholder-if-empty (stream)
        ((zerop length)
         "no elements")
        (t
         (let ((place-class (if (adjustable-array-p object)
                                'adjustable-vector-element-place
                                'vector-element-place)))
           (multiple-value-bind (start end truncated?)
               (effective-bounds state length)
             (with-preserved-cursor-x (stream)
               (formatting-table (stream)
                 (loop :for i :from start :below end
                       :do (formatting-row (stream)
                             (formatting-place
                                 (object place-class i
                                         present-place present-object)
                               (formatting-cell (stream :align-x :right)
                                 (with-style (stream :slot-like)
                                   (present-place stream)))
                               (formatting-cell (stream)
                                 (if (and fill-pointer (>= i fill-pointer))
                                     (with-style (stream :inactive)
                                       (present-object stream))
                                     (present-object stream))))))))
             (when truncated?
               (note-truncated stream length (- end start))))))))))

;;; Non-vector array

(defmethod inspect-object-using-state ((object array)
                                       (state  inspected-array)
                                       (style  (eql :expanded-header))
                                       (stream t))
  (inspect-class-as-name (class-of object) stream))

(defmethod inspect-object-using-state ((object array)
                                       (state  inspected-array)
                                       (styel  (eql :expanded-body))
                                       (stream t))
  (let ((length (array-total-size object)))
    (with-preserved-cursor-x (stream)
      (formatting-table (stream)
        (inspect-element-type-and-total-size object stream)
        (formatting-row (stream)
          (format-place-cells stream object 'reader-place 'array-rank
                              :label "Rank")
          (format-place-cells stream object 'array-dimensions-place nil
                              :label "Dimensions"))
        (inspect-displacement object stream)))

    (with-section (stream) "Elements"
      (with-placeholder-if-empty (stream)
        ((zerop length)
         "no elements")
        (t
         (case (array-rank object)
           (2
            (destructuring-bind (row-count column-count)
                (array-dimensions object)
              (formatting-table (stream)
                (loop :for row :from 0 :below row-count
                      :do (formatting-row (stream)
                            (loop :for column :from 0 :below column-count
                                  :for i = (array-row-major-index object row column)
                                  :do (formatting-cell (stream)
                                        (formatting-place
                                            (object 'array-element-place i
                                                    present-place present-object)
                                          (with-style (stream :slot-like)
                                            (present-place stream))
                                          (write-char #\Space stream)
                                          (present-object stream)))))))))))))))
