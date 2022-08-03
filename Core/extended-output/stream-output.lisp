;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) Copyright 1998-2001 by Michael McDonald <mikemac@mikemac.com>
;;;  (c) Copyright 2000,2014 by Robert Strandh <robert.strandh@gmail.com>
;;;
;;; ---------------------------------------------------------------------------
;;;

(in-package #:clim-internals)

;;; Standard-Output-Stream class
(defclass standard-output-stream (output-stream) ())

(defmethod stream-recording-p ((stream output-stream)) nil)
(defmethod stream-drawing-p ((stream output-stream)) t)

;;; Standard-Extended-Output-Stream class

(defclass standard-extended-output-stream (extended-output-stream
                                           standard-output-stream
                                           standard-page-layout
                                           filling-output-mixin)
  ((cursor :accessor stream-text-cursor)
   (foreground :initarg :foreground :reader foreground)
   (background :initarg :background :reader background)
   (text-style :initarg :text-style :reader stream-text-style)
   (vspace :initarg :vertical-spacing :reader stream-vertical-spacing)
   (eol :initarg :end-of-line-action :accessor stream-end-of-line-action)
   (eop :initarg :end-of-page-action :accessor stream-end-of-page-action)
   (view :initarg :default-view :accessor stream-default-view)
   (baseline :initform 0 :reader stream-baseline)
   (height :accessor stream-cursor-height))
  (:default-initargs
   :foreground +black+ :background +white+ :text-style *default-text-style*
   :vertical-spacing 2 :end-of-page-action :scroll :end-of-line-action :wrap
   :default-view +textual-view+))

(defmethod stream-force-output :after
    ((stream standard-extended-output-stream))
  (with-sheet-medium (medium stream)
    (medium-force-output medium)))

(defmethod stream-finish-output :after
    ((stream standard-extended-output-stream))
  (with-sheet-medium (medium stream)
    (medium-finish-output medium)))

(defmethod initialize-instance :after
    ((stream standard-extended-output-stream) &rest initargs)
  (declare (ignore initargs))
  (multiple-value-bind (x-start y-start)
      (stream-cursor-initial-position stream)
    (setf (stream-text-cursor stream)
          (make-instance 'standard-text-cursor
                         :sheet stream
                         :x-position x-start
                         :y-position y-start)))
  (setf (cursor-active (stream-text-cursor stream)) t))

(defmethod slot-unbound (class (stream standard-extended-output-stream) (slot (eql 'height)))
  (declare (ignore class))
  (setf (stream-cursor-height stream)
        (text-style-height (stream-text-style stream) stream)))

(defmethod stream-cursor-position ((stream standard-extended-output-stream))
  (cursor-position (stream-text-cursor stream)))

(defmethod* (setf stream-cursor-position)
    (x y (stream standard-extended-output-stream))
  (setf (cursor-position (stream-text-cursor stream)) (values x y)))

(defmethod stream-set-cursor-position ((stream standard-extended-output-stream) x y)
  (setf (stream-cursor-position stream) (values x y)))

(defmethod stream-increment-cursor-position
    ((stream standard-extended-output-stream) dx dy)
  (multiple-value-bind (x y) (cursor-position (stream-text-cursor stream))
    (let ((dx (or dx 0))
          (dy (or dy 0)))
    (setf (cursor-position (stream-text-cursor stream))
          (values (+ x dx) (+ y dy))))))

;;;

(defmethod handle-repaint :around ((stream standard-extended-output-stream)
                                   region)
  (declare (ignorable region))
  (let ((cursor (stream-text-cursor stream)))
    (if (cursor-state cursor)
        ;; Erase the cursor so that the subsequent flip operation will
        ;; make a cursor, whether or not the next-method erases the
        ;; location of the cursor.
        ;; XXX clip to region?  No one else seems to...
        ;; Sure clip to region! --GB
        (letf (((cursor-state cursor) nil))
          (call-next-method))
        (call-next-method))))

;; In next few functions we can't call (setf stream-cursor-position) because
;; that would close the text-output-record unnecessarily. Using underlying
;; text-cursor with (setf cursor-position) is fine when the cursor is "off".
;; Otherwise output record would be closed anyway. -- jd 2019-01-07

(defmacro with-cursor-off (stream &body body)
  `(letf (((cursor-visibility (stream-text-cursor ,stream)) nil))
     ,@body))

(defmacro with-end-of-line-action ((stream action) &body body)
  (when (eq stream t)
    (setq stream '*standard-output*))
  (check-type stream symbol)
  `(letf (((stream-end-of-line-action ,stream) ,action))
     ,@body))

(defgeneric %note-stream-end-of-line (stream action new-width)
  (:method (stream action new-width)
    (declare (ignore stream action new-width))))

(defmacro with-end-of-page-action ((stream action) &body body)
  (when (eq stream t)
    (setq stream '*standard-output*))
  (check-type stream symbol)
  `(letf (((stream-end-of-page-action ,stream) ,action))
     ,@body))

(defgeneric maybe-end-of-page-action (stream y)
  (:method ((stream standard-extended-output-stream) y)
    ;; fixme: remove assumption about page vertical direction -- jd 2019-03-02
    (let ((bottom-margin (nth-value 1 (stream-cursor-final-position stream)))
          (end-of-page-action (stream-end-of-page-action stream)))
      (when (> y bottom-margin)
        (%note-stream-end-of-page stream end-of-page-action y)
        (ecase end-of-page-action
          ((:scroll :allow)  nil)
          ((:wrap :wrap*)
           (setf (cursor-position (stream-text-cursor stream))
                 (values (nth-value 0 (stream-cursor-position stream))
                         (nth-value 1 (stream-cursor-initial-position stream))))))))))

(defgeneric %note-stream-end-of-page (stream action new-height)
  (:method (stream action new-height)
    (declare (ignore stream action new-height))))

(defgeneric seos-write-newline (stream &optional soft-newline-p)
  (:method :after ((stream filling-output-mixin) &optional soft-newline-p)
    (when-let ((after-line-break-fn (after-line-break stream)))
      (funcall after-line-break-fn stream soft-newline-p)))
  (:method ((stream standard-extended-output-stream) &optional soft-newline-p)
    (declare (ignorable soft-newline-p))
    (let* ((current-cy       (nth-value 1 (stream-cursor-position stream)))
           (vertical-spacing (stream-vertical-spacing stream))
           (updated-cy       (+ current-cy
                                (stream-cursor-height stream)
                                vertical-spacing)))
      (setf (cursor-position (stream-text-cursor stream))
            (values (stream-cursor-initial-position stream)
                    updated-cy))
      ;; this will close the output record if recorded
      (unless nil ;soft-newline-p
        (finish-output stream))
      (let* ((medium       (sheet-medium stream))
             (text-style   (medium-text-style medium))
             (new-baseline (text-style-ascent text-style medium))
             (new-height   (text-style-height text-style medium)))
        ;; For new lines we reset the char height to 0 in case of the text
        ;; style change after the line break. -- jd 2020-08-07
        (maybe-end-of-page-action stream (+ updated-cy new-height))
        (setf (slot-value stream 'baseline) new-baseline
              (stream-cursor-height stream)  new-height)))))

(defun seos-write-string (stream string &optional (start 0) end)
  (setq end (or end (length string)))
  (when (= start end)
    (return-from seos-write-string))
  (with-bounding-rectangle* (left-margin top-margin right-margin bottom-margin)
      (stream-page-region stream)
    (declare (ignore top-margin bottom-margin))
    (multiple-value-bind (cx cy) (stream-cursor-position stream)
      (let* ((medium (sheet-medium stream))
             (text-style (medium-text-style medium))
             ;; fixme: remove assumption about the text direction (LTR).
             (text-style-height (text-style-height text-style medium))
             (text-style-ascent (text-style-ascent text-style medium))
             (text-width (stream-string-width stream string
                                              :start start :end end
                                              :text-style text-style))
             (text-height (stream-cursor-height stream)))
        (maxf (slot-value stream 'baseline) text-style-ascent)
        (maxf (stream-cursor-height stream) text-style-height)
        (maybe-end-of-page-action stream (+ cy text-height))
        (let* ((eol-action (stream-end-of-line-action stream))
               (eol-p (> (+ cx text-width) right-margin)))
          (when (or (null eol-p) (member eol-action '(:allow :scroll)))
            (stream-write-output stream string start end)
            (incf cx text-width)
            (setf (cursor-position (stream-text-cursor stream)) (values cx cy))
            (when (> cx right-margin)
              (%note-stream-end-of-line stream eol-action cx))
            (return-from seos-write-string))
          ;; All new lines from here on are soft new lines, we could skip
          ;; closing the text-output-record and have multiline records to
          ;; allow gimmics like a dynamic reflow). Also text-style doesn't
          ;; change until the end of this function. -- jd 2019-01-10
          ;;
          ;; Writing a newline may cause the cursor increment, so we
          ;; need to compute split for each line after the soft newline
          ;; has been written. -- jd 2020-03-01
          (loop with width = (if (text-style-fixed-width-p text-style medium)
                                 (text-style-width text-style medium)
                                 (lambda (string start end)
                                   (text-size medium string
                                              :text-style text-style
                                              :start start :end end)))
                with margin = (- right-margin left-margin)
                with cursor = (stream-text-cursor stream)
                with break  = (ecase eol-action
                                (:wrap nil)
                                (:wrap* t))
                for cursor-x = (cursor-position cursor)
                for offset   = (- cursor-x left-margin)
                for split    = (car (line-breaks string width
                                                 :count 1
                                                 :initial-offset offset
                                                 :margin margin
                                                 :break-strategy break
                                                 :start start :end end))
                do (maxf (stream-cursor-height stream) text-style-height)
                   (stream-write-output stream string start split)
                   (when (= split end)
                     ;; FIXME we don't know what will be the continuation, so
                     ;; until the text record is closed we should not be eager
                     ;; to print the last line. Otherwise we may be forced to
                     ;; break in a middle the word that would be wrapped to a
                     ;; new line otherwise. -- jd 2022-08-21
                     ;;
                     ;; XXX should we break after /all/ trailing spaces, even if
                     ;; they'd take more space than a line?
                     (setf (cursor-position cursor)
                           (values (+ cursor-x
                                      (stream-string-width stream string
                                                           :start start :end split
                                                           :text-style text-style))
                                   (nth-value 1 (cursor-position cursor))))
                     (return))
                   (seos-write-newline stream t)
                   (setf start split)))))))

(defgeneric stream-write-output (stream line &optional start end)
  (:documentation
   "Writes the character or string LINE to STREAM. This function
produces no more than one line of output i.e., doesn't wrap."))

;;; The cursor is in stream coordinates.
(defmethod stream-write-output ((stream standard-extended-output-stream) line
                                &optional (start 0) end)
  ;; Do not capture medium transformation - this is a stream operation and we
  ;; draw at the current cursor position. -- jd 2019-01-04
  (with-identity-transformation (stream)
    (multiple-value-bind (cx cy) (stream-cursor-position stream)
      (draw-text* stream line cx (+ cy (stream-baseline stream))
                  :start start :end end))))

(defmethod stream-write-char ((stream standard-extended-output-stream) char)
  (with-cursor-off stream
    (if (char= #\Newline char)
        (seos-write-newline stream)
        (seos-write-string stream (string char)))))

(defmethod stream-write-string ((stream standard-extended-output-stream) string
                                &optional (start 0) end)
  (let ((seg-start start)
        (end (or end (length string))))
    (with-cursor-off stream
      (loop for i from start below end do
        (when (char= #\Newline
                     (char string i))
          (seos-write-string stream string seg-start i)
          (seos-write-newline stream)
          (setq seg-start (1+ i))))
      (seos-write-string stream string seg-start end)))
  string)

(defmethod stream-character-width ((stream standard-extended-output-stream) char
                                   &key (text-style nil))
  (with-sheet-medium (medium stream)
    (text-style-character-width (or text-style (medium-text-style medium))
                                medium
                                char)))

(defmethod stream-string-width ((stream standard-extended-output-stream) string
                                &key (start 0) (end nil) (text-style nil))
  (with-sheet-medium (medium stream)
    (if (null text-style)
        (setq text-style (medium-text-style (sheet-medium stream))))
    (multiple-value-bind (total-width total-height final-x final-y baseline)
        (text-size medium string :text-style text-style
                   :start start :end end)
      (declare (ignore total-height final-y baseline))
      (values final-x total-width))))

(defmethod stream-text-margin ((stream standard-extended-output-stream))
  (bounding-rectangle-max-x (stream-page-region stream)))

(defmethod (setf stream-text-margin) (margin (stream standard-extended-output-stream))
  (setf (stream-text-margins stream)
        (if margin
            `(:right (:absolute ,margin))
            `(:right (:relative 0)))))

(defmethod stream-line-height ((stream standard-extended-output-stream)
                               &key (text-style nil))
  (+ (text-style-height (or text-style (medium-text-style (sheet-medium stream)))
                        (sheet-medium stream))
     (stream-vertical-spacing stream)))

(defmethod stream-line-width ((stream standard-extended-output-stream))
  (bounding-rectangle-width (stream-page-region stream)))

(defmethod stream-line-column ((stream standard-extended-output-stream))
  (let ((line-width (- (stream-cursor-position stream)
                       (stream-cursor-initial-position stream))))
    (if (minusp line-width)
        nil
        ;; Some PPRINT implemenations require STREAM-LINE-COLUMN to return an
        ;; integer here. May be worth revising in the future. -- jd 2021-12-18
        (floor (/ line-width (stream-character-width stream #\M))))))

(defmethod stream-start-line-p ((stream standard-extended-output-stream))
  (multiple-value-bind (x y) (stream-cursor-position stream)
    (declare (ignore y))
    (= x (stream-cursor-initial-position stream))))

(defmacro with-room-for-graphics ((&optional (stream t)
                                             &rest arguments
                                             &key (first-quadrant t)
                                             height
                                             (move-cursor t)
                                             (record-type ''standard-sequence-output-record))
                                  &body body)
  (declare (ignore first-quadrant height move-cursor record-type))
  (let ((cont (gensym "CONT.")))
    (with-stream-designator (stream '*standard-output*)
      `(labels ((,cont (,stream)
                  ,@body))
         (declare (dynamic-extent #',cont))
         (invoke-with-room-for-graphics #',cont ,stream ,@arguments)))))

(defmethod beep (&optional medium)
  (if medium
      (medium-beep medium)
      (when (sheetp *standard-output*)
        (medium-beep (sheet-medium *standard-output*)))))

(defmethod invoke-with-local-coordinates ((medium extended-output-stream) cont x y)
  ;; For now we do as real CLIM does.
  ;; Default seems to be the cursor position.
  ;; Moore suggests we use (0,0) if medium is no stream.
  ;;
  ;; Furthermore, the specification is vague about possible scalings ...
  (unless (and x y)
    (multiple-value-bind (cx cy) (stream-cursor-position medium)
      (orf x cx)
      (orf y cy)))
  (multiple-value-bind (mxx mxy myy myx tx ty)
      (get-transformation (medium-transformation medium))
    (declare (ignore tx ty))
    (with-identity-transformation (medium)
      (with-drawing-options
          (medium :transformation (make-transformation
                                   mxx mxy myy myx
                                   x y))
        (funcall cont medium)))))

(defmethod invoke-with-first-quadrant-coordinates ((medium extended-output-stream) cont x y)
  ;; First we do the same as invoke-with-local-coordinates but rotate and deskew
  ;; it so that it becomes first-quadrant. We do this by simply measuring the
  ;; length of the transformed x and y "unit vectors".  [That is (0,0)-(1,0) and
  ;; (0,0)-(0,1)] and setting up a transformation which features an upward
  ;; pointing y-axis and a right pointing x-axis with a length equal to above
  ;; measured vectors.
  (unless (and x y)
    (multiple-value-bind (cx cy) (stream-cursor-position medium)
      (orf x cx)
      (orf y cy)))
  (let* ((tr (medium-transformation medium))
         (xlen
          (multiple-value-bind (dx dy) (transform-distance tr 1 0)
            (sqrt (+ (expt dx 2) (expt dy 2)))))
         (ylen
          (multiple-value-bind (dx dy) (transform-distance tr 0 1)
            (sqrt (+ (expt dx 2) (expt dy 2))))))
    (with-identity-transformation (medium)
      (with-drawing-options
          (medium :transformation (make-transformation
                                   xlen 0 0 (- ylen)
                                   x y))
        (funcall cont medium)))))

;;; Backend part of the output destination mechanism
;;;
;;; See clim-core/commands.lisp for the "user interface" part.

(defgeneric invoke-with-standard-output (continuation destination)
  (:documentation
   "Call CONTINUATION (with no arguments) with *STANDARD-OUTPUT*
rebound according to DESTINATION."))

(defmethod invoke-with-standard-output (continuation (destination null))
  ;; Call CONTINUATION without rebinding *STANDARD-OUTPUT* at all.
  (funcall continuation))

(defclass output-destination ()
  ())

(defclass stream-destination (output-destination)
  ((destination-stream :accessor destination-stream
                       :initarg :destination-stream)))

(defmethod invoke-with-standard-output
    (continuation (destination stream-destination))
  (let ((*standard-output* (destination-stream destination)))
    (funcall continuation)))

(defclass file-destination (output-destination)
  ((file :reader destination-file :initarg :file)))

(defmethod destination-element-type ((destination file-destination))
  :default)

(defmethod invoke-with-standard-output
    (continuation (destination file-destination))
  (with-open-file (*standard-output* (destination-file destination)
                                     :element-type (destination-element-type
                                                    destination)
                                     :direction :output
                                     :if-exists :supersede)
    (funcall continuation)))

(defparameter *output-destination-types*
  '(("Stream" stream-destination)))

(defun register-output-destination-type (name class-name)
  (let ((class (find-class class-name nil)))
    (cond ((null class)
           (error "~@<~S is not the name of a class.~@:>" class-name))
          ((not (subtypep class #1='output-destination))
           (error "~@<~A is not a subclass of ~S.~@:>" class #1#))))
  (pushnew (list name class-name) *output-destination-types* :test #'equal))
