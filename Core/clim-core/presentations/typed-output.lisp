;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 1998-2000 by Michael McDonald <mikemac@mikemac.com>
;;;  (c) copyright 2001-2002 by Tim Moore <moore@bricoworks.com>
;;;  (c) copyright 2020 by Daniel Kochmański <daniel@turtleware.eu>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Implementation of the typed output.
;;;

(in-package #:clim-internals)

;;; FIXME impelment invoke-with-output-as-presentation and expand to it.
(defmacro with-output-as-presentation ((stream object type
                                        &rest key-args
                                        &key modifier single-box parent
                                             (allow-sensitive-inferiors t)
                                             (record-type ''standard-presentation)
                                        &allow-other-keys)
                                       &body body)
  (declare (ignore parent single-box modifier))
  (with-stream-designator (stream '*standard-output*)
    (multiple-value-bind (decls with-body)
        (get-body-declarations body)
      (with-gensyms (record-arg continuation)
        (with-keywords-removed (key-args (:record-type :allow-sensitive-inferiors))
          `(flet ((,continuation () ,@decls ,@with-body))
             (declare (dynamic-extent #',continuation))
             (if (and (output-recording-stream-p ,stream)
                      *allow-sensitive-inferiors*)
                 (with-new-output-record
                     (,stream ,record-type ,record-arg
                              :object ,object
                              :type (expand-presentation-type-abbreviation ,type)
                              ,@key-args)
                   (let ((*allow-sensitive-inferiors* ,allow-sensitive-inferiors))
                     (,continuation)))
                 (,continuation))))))))

;;; XXX The spec calls out that the presentation generic function has
;;; keyword arguments acceptably and for-context-type, but the
;;; examples I've seen don't mention them at all in the methods
;;; defined for present.  So, leave them out of the generic function
;;; lambda list...
(define-presentation-generic-function %present present
  (type-key parameters options object type stream view
   &key &allow-other-keys))

(define-default-presentation-method present
    (object type stream (view textual-view) &key acceptably for-context-type)
  (declare (ignore for-context-type type))
  (if acceptably
      (let ((*print-readably* t))
        (prin1 object stream))
      (princ object stream)))

(defun present (object &optional (type (presentation-type-of object))
                &key
                  (stream *standard-output*)
                  (view (stream-default-view stream))
                  modifier
                  acceptably
                  (for-context-type type)
                  single-box
                  (allow-sensitive-inferiors t)
                  (sensitive t)
                  (record-type 'standard-presentation))
  (let* ((real-type (expand-presentation-type-abbreviation type))
         (context-type (if (eq for-context-type type)
                           real-type
                           (expand-presentation-type-abbreviation
                            for-context-type))))
    (stream-present stream object real-type
                    :view view :modifier modifier :acceptably acceptably
                    :for-context-type context-type :single-box single-box
                    :allow-sensitive-inferiors allow-sensitive-inferiors
                    :sensitive sensitive
                    :record-type record-type)))

(defun format-items (items &rest args
                     &key (stream *standard-output*)
                       (printer #'prin1) presentation-type
                       cell-align-x cell-align-y
                     &allow-other-keys)
  (let ((printer (if printer
                     (if presentation-type
                         (lambda (item stream)
                           (with-output-as-presentation (stream item presentation-type)
                             (funcall printer item stream)))
                         printer)
                     (if presentation-type
                         (lambda (item stream)
                           (present item presentation-type :stream stream))
                         #'prin1)))
        (args (alexandria:remove-from-plist
               args :stream :printer :presentation-type
               :cell-align-x :cell-align-y)))
    (apply #'invoke-formatting-item-list
           stream
           (lambda (stream)
             (map nil (lambda (item)
                        (formatting-cell (stream :align-x cell-align-x
                                                 :align-y cell-align-y)
                          (funcall printer item stream)))
                  items))
           args)))

(defmethod stream-present ((stream output-recording-stream) object type
                           &key
                             (view (stream-default-view stream))
                             modifier
                             acceptably
                             (for-context-type type)
                             single-box
                             (allow-sensitive-inferiors t)
                             (sensitive t)
                             (record-type 'standard-presentation))
  ;; *allow-sensitive-inferiors* controls whether or not
  ;; with-output-as-presentation will emit a presentation
  (let ((*allow-sensitive-inferiors* (and *allow-sensitive-inferiors*
                                          sensitive)))
    (with-output-as-presentation (stream object type
                                         :view view
                                         :modifier modifier
                                         :single-box single-box
                                         :allow-sensitive-inferiors
                                         allow-sensitive-inferiors
                                         :record-type record-type)
      (funcall-presentation-generic-function
       present object type stream view
       :acceptably acceptably :for-context-type for-context-type))))

;;; Should work well enough on non-CLIM streams...
(defmethod stream-present (stream object type
                           &key
                             (view +textual-view+)
                             modifier
                             acceptably
                             (for-context-type type)
                             single-box
                             (allow-sensitive-inferiors t)
                             (sensitive t)
                             (record-type 'standard-presentation))
  (declare (ignore modifier single-box allow-sensitive-inferiors sensitive
                   record-type))
  (funcall-presentation-generic-function
   present object type stream view
   :acceptably acceptably :for-context-type for-context-type)
  nil)

(defun present-to-string (object &optional (type (presentation-type-of object))
                          &key (view +textual-view+)
                            acceptably
                            (for-context-type type)
                            (string nil stringp)
                            (index 0 indexp))
  (let* ((real-type (expand-presentation-type-abbreviation type))
         (context-type (if (eq for-context-type type)
                           real-type
                           (expand-presentation-type-abbreviation
                            for-context-type))))
    (when (and stringp indexp)
      (setf (fill-pointer string) index))
    (flet ((do-present (s)
             (stream-present s object real-type
                             :view view :acceptably acceptably
                             :for-context-type context-type)))
      (declare (dynamic-extent #'do-present))
      (let ((result (if stringp
                        (with-output-to-string (stream string)
                          (do-present stream))
                        (with-output-to-string (stream)
                          (do-present stream)))))
        (if stringp
            (values string (fill-pointer string))
            result)))))

(define-presentation-generic-function %highlight-presentation
    highlight-presentation
  (type-key parameters options type record stream state))

(define-default-presentation-method highlight-presentation
    (type record stream state)
  (declare (ignore type))
  (if (or (eq (presentation-single-box record) t)
          (eq (presentation-single-box record) :highlighting))
      (highlight-output-record record stream state)
      (highlight-output-record-tree record stream state)))

;;; Internal function to highlight just one presentation

(defun highlight-presentation-1 (presentation stream state)
  (with-output-recording-options (stream :record nil)
    (funcall-presentation-generic-function highlight-presentation
                                           (presentation-type presentation)
                                           presentation
                                           stream
                                           state)))

(defmethod highlight-output-record-tree (record stream state)
  (declare (ignore record stream state))
  (values))

(defmethod highlight-output-record-tree ((record compound-output-record) stream state)
  (map-over-output-records
   (lambda (record)
     (highlight-output-record-tree record stream state))
   record))

(defmethod highlight-output-record-tree ((record displayed-output-record) stream state)
  (highlight-output-record record stream state))

#+nil ;; certainly interesting, but does it belong /here/? -- jd
(defmethod highlight-output-record ((record standard-presentation)
                                    stream state)
  (map-over-output-records
   (lambda (child)
     (highlight-output-record child stream state))
   record))
