;;;; Copyright (C) 2018, 2019 Jan Moringen
;;;;
;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Library General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 2 of the License, or (at your option) any later version.
;;;;
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Library General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Library General Public
;;;; License along with this library; if not, write to the
;;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;;; Boston, MA  02111-1307  USA.

(cl:in-package #:clouseau)

#+example (clouseau:inspect (make-two-way-stream
			     (make-concatenated-stream (make-string-input-stream "foo") (make-string-input-stream "bar"))
			     (make-broadcast-stream (make-string-output-stream) (open "/tmp/out" :direction :output :if-does-not-exist :create :if-exists :supersede))))

;;; Utilities

(defmethod stream-file ((stream stream))
  nil)

(defmethod stream-content ((stream stream))
  (values nil nil))

(defmethod stream-length ((stream stream))
  (nth-value 1 (stream-content stream)))

(defmethod stream-targets ((stream stream))
  (values '() '())
  #+no (values (when (input-stream-p stream) (list stream))
          (when (output-stream-p stream) (list stream))))

(defmethod stream-flat-targets ((stream stream))
  (multiple-value-bind (input-streams output-streams) (stream-targets stream)
    (append (remove stream input-streams)
            (remove stream output-streams))))

;;; `string-stream'

#+sbcl (defmethod stream-content ((stream sb-impl::string-input-stream))
         (let ((string (sb-impl::string-input-stream-string stream)))
           (values string (length string))))

;;; `file-stream'

(defmethod stream-file ((stream file-stream))
  (ignore-errors (pathname stream)))

(defmethod stream-content ((stream file-stream))
  (when-let* ((pathname (ignore-errors (pathname stream)))
              (string   (alexandria:read-file-into-string pathname)))
    (values string (length string))))

;;; `broadcast-stream'

(defmethod stream-length ((stream broadcast-stream))
  (some #'stream-length (broadcast-stream-streams stream)))

(defmethod stream-targets ((stream broadcast-stream))
  (values '() (broadcast-stream-streams stream)))

;;; `two-way-stream'

(defmethod stream-content ((stream two-way-stream))
  (stream-content (two-way-stream-input-stream stream)))

(defmethod stream-targets ((stream two-way-stream))
  (values (list (two-way-stream-input-stream stream))
          (list (two-way-stream-output-stream stream))))

;;; `concatenated-stream'

(defmethod stream-content ((stream concatenated-stream))
  (loop :for target :in (stream-targets stream)
        :for content = (stream-content target)
        :when content
        :collect content :into contents
        :and :sum (length content) :into length
        :finally (return (values (when contents
                                   (apply #'concatenate (class-of (first contents)) contents))
                                 length))))

(defmethod stream-targets ((stream concatenated-stream))
  (values (concatenated-stream-streams stream) '()))

;; TODO synonym

;;; `pretty-stream'

#+sbcl (defmethod stream-targets ((stream sb-pretty:pretty-stream))
         (values '() (list (sb-pretty::pretty-stream-target stream))))

;;; Places

(defclass file-position-place (basic-place)
  ())

(defmethod supportsp ((place file-position-place) (operation (eql 'setf)))
  (typep (value place) 'non-negative-integer))

(defmethod accepts-value-p ((place file-position-place) (value t))
  (and (typep value 'non-negative-integer)
       (<= value (stream-length (container place)))))

(defmethod value ((place file-position-place))
  (file-position (container place)))

(defmethod (setf value) ((new-value t) (place file-position-place))
  (file-position (container place) new-value))

;;; Object

(defmethod inspect-object-using-state :after ((object stream)
                                              (state  inspected-instance)
                                              (style  (eql :badges))
                                              (stream extended-output-stream))
  (write-char #\Space stream)
  (badge stream (if (open-stream-p object) "open" "closed"))
  (when (input-stream-p object)
    (write-char #\Space stream)
    (badge stream "input"))
  (when (output-stream-p object)
    (write-char #\Space stream)
    (badge stream "output"))
  (let ((position (file-position object)))
    (when (eql position 0)
      (write-char #\Space stream)
      (badge stream "at-beginning-of-file"))
    (when (and position (eql position (stream-length object)))
      (write-char #\Space stream)
      (badge stream "at-end-of-file"))))

(defmethod inspect-object-using-state ((object stream)
                                       (state  inspected-instance)
                                       (style  (eql :expanded-body))
                                       (stream extended-output-stream))
  (with-preserved-cursor-x (stream)
    (formatting-table (stream)
      (formatting-row (stream)
        (format-place-row stream object 'reader-place 'stream-external-format
                          :label "External format")
        (format-place-row stream object 'reader-place 'stream-element-type
                          :label "Element type"))
      (formatting-row (stream)
        (format-place-row stream object 'file-position-place nil
                          :label "File position")
        (format-place-row stream object 'reader-place 'stream-length
                          :label "File length")) ; TODO as MB, GB, etc.
      (formatting-row (stream)
        (format-place-row stream object 'reader-place 'stream-file
                          :label "File"))))

  ;; TODO one for element-type string, one for binary
  ;; TODO can do the same for string streams?
  (when-let* ((position     (file-position object)))
    (multiple-value-bind (content length) (stream-content object)
      (when content
        (with-section (stream) "Content"
          (surrounding-output-with-border (stream)
            (clime:with-temporary-margins
                (stream :left `(:relative ,(stream-cursor-position stream)))
              (with-drawing-options (stream :text-family :fix)
                (write-string content stream
                              :start (max (- position 100) 0)
                              :end   position)
                (multiple-value-bind (x y) (stream-cursor-position stream)
                  (draw-line* stream x y x (+ y (nth-value 1 (text-size stream "M")))
                              :ink +red+ :line-thickness 2))
                (write-string content stream
                              :start position
                              :end   (min (+ position 100) length)))))))))

  (when (stream-flat-targets object)
    (with-section (stream) "Target streams"
      (flet ((classified-targets (stream)
               (multiple-value-bind (input-targets output-targets)
                   (stream-targets stream)
                 (append (map 'list (curry #'cons :input) input-targets)
                         (map 'list (curry #'cons :output) output-targets)))))
        (format-graph-from-roots
         (classified-targets object)
         (lambda (info stream)
           (destructuring-bind (role . target) info
             (format stream "~:(~A~)~%" role)
             (formatting-place (object 'pseudo-place target nil present-value)
               (present-value stream))))
         (compose #'classified-targets #'cdr)
         :graph-type :dag :merge-duplicates t
         :stream stream))))

  (call-next-method))

(defun test ()
  (let ((stream1))
    (let ((stream (open "/tmp/does-not-exist1" :direction :output)))
      (pprint-logical-block (stream (list :foo))
        (setf stream1 (make-broadcast-stream stream (make-string-output-stream)))))
    stream1)

  (let ((stream2))
    (let ((stream (open "/tmp/exists" :direction :input)))
      (pprint-logical-block (stream (list :foo))
        (setf stream2 (make-broadcast-stream stream stream))))
    stream2))
