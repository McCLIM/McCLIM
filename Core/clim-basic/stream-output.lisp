;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 1998,1999,2000,2001 by Michael McDonald (mikemac@mikemac.com)
;;;  (c) copyright 2000, 2014 by
;;;           Robert Strandh (robert.strandh@gmail.com)

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

;;; Note: in the methods defined on output streams, I often use
;;;       the sheet's medium as the argument to the draw-* routines.
;;;       This is so that they don't get recorded if the stream also
;;;       happens to be an output-recording-stream. - MikeMac 1/7/99

;;; Standard-Output-Stream class
(defclass standard-output-stream (output-stream cut-and-paste-mixin) ())

(defmethod stream-recording-p ((stream output-stream)) nil)
(defmethod stream-drawing-p ((stream output-stream)) t)

(defgeneric* (setf cursor-position) (x y cursor))

;;; Cursor-Mixin class
(defclass cursor-mixin ()
  ((sheet :initarg :sheet
          :reader cursor-sheet)
   (x :initform 0 :initarg :x-position)
   (y :initform 0 :initarg :y-position)
   (width :initform 8)
   (appearance :type (member :solid :hollow)
               :initarg :appearance :initform :hollow
               :accessor cursor-appearance)
   ;; XXX what does "cursor is active" mean?
   ;; It means that the sheet (stream) updates the cursor, though
   ;; currently the cursor appears to be always updated after stream
   ;; text operations. -- moore
   (cursor-active :initform nil
                  :accessor cursor-active)
   (cursor-state :initform nil
                 :accessor cursor-state)))

(defgeneric cursor-height (cursor))

(defmethod print-object ((cursor cursor-mixin) stream)
  (with-slots (x y) cursor
    (print-unreadable-object (cursor stream :type t :identity t)
      (format stream "~D ~D " x y))))

(defgeneric flip-screen-cursor (cursor))

;;; XXX What to do when we can't draw the cursor immediately (like,
;;; we're not drawing?) The whole flip-screen-cursor idea breaks down.

(defmethod (setf cursor-state) :around (state (cursor cursor-mixin))
  (unless (eq state (slot-value cursor 'cursor-state))
    (flip-screen-cursor cursor))
  (call-next-method))

(defun decode-cursor-visibility (visibility)
  "Given :on, :off, or nil, returns the needed active and state attributes for the cursor."
  (ecase visibility
    ((:on t) (values t t))
    (:off    (values t nil))
    ((nil)   (values nil nil))))

(defmethod cursor-visibility ((cursor cursor-mixin))
  (let ((a (cursor-active cursor))
        (s (cursor-state cursor)))
    (cond ((and a s) :on)
          ((and a (not s)) :off)
          (t nil))))

(defmethod (setf cursor-visibility) (nv (cursor cursor-mixin))
  (multiple-value-bind (active state)
      (decode-cursor-visibility nv)
    (setf (cursor-state cursor)  state
          (cursor-active cursor) active)))

(defmethod cursor-position ((cursor cursor-mixin))
  (with-slots (x y) cursor
    (values x y)))

(defmethod* (setf cursor-position) (nx ny (cursor cursor-mixin))
  (with-slots (x y) cursor
    (letf (((cursor-state cursor) nil))
      (multiple-value-prog1
          (setf (values x y) (values nx ny))))
    (when (and (cursor-active cursor)
               (output-recording-stream-p (cursor-sheet cursor)))
      (stream-close-text-output-record (cursor-sheet cursor)))))

(defmethod flip-screen-cursor ((cursor cursor-mixin))
  (when (stream-drawing-p (cursor-sheet cursor))
    (with-slots (x y sheet width) cursor
      (let ((height (cursor-height cursor)))
        (draw-rectangle* (sheet-medium (cursor-sheet cursor))
                         x y
                         (+ x width) (+ y height)
                         :filled (ecase (cursor-appearance cursor)
                                   (:solid t) (:hollow nil))
                         :ink +flipping-ink+)))))

(defgeneric display-cursor (cursor state))

(defmethod display-cursor ((cursor cursor-mixin) state)
  (unless (stream-drawing-p (cursor-sheet cursor))
    (return-from display-cursor nil))
  (with-slots (x y sheet width) cursor
    (let ((height (cursor-height cursor)))
      (case state
        (:draw
         (draw-rectangle* (sheet-medium (cursor-sheet cursor))
                          x y
                          (+ x width) (+ y height)
                          :filled (ecase (cursor-appearance cursor)
                                    (:solid t) (:hollow nil))
                          :ink +foreground-ink+))
        (:erase
         ;; This is how I'd like this to work, as painting over with
         ;; the background ink is repugnant. I leave this disabled
         ;; because I'm concerned about infinite recursion if
         ;; replay-output-record calls here. --Hefner
         #+nil (repaint-sheet (cursor-sheet cursor)
                              (make-bounding-rectangle x y (+ 1 x width)
                                                       (+ 1 y height)))
         (draw-rectangle* (sheet-medium (cursor-sheet cursor))
                          x y
                          (+ x width) (+ y height)
                          :filled (ecase (cursor-appearance cursor)
                                    (:solid t) (:hollow nil))
                          :ink +background-ink+))))))

;;; Standard-Text-Cursor class

(defclass standard-text-cursor (cursor-mixin cursor)
  ())

(defmethod cursor-height ((cursor standard-text-cursor))
  (%stream-char-height (cursor-sheet cursor)))


;;; Extended-Output-Stream class

(defgeneric* (setf stream-cursor-position) (x y stream))

;;; Standard-Extended-Output-Stream class

(defclass standard-extended-output-stream (extended-output-stream
                                           standard-output-stream)
  ((cursor :accessor stream-text-cursor)
   (foreground :initarg :foreground :reader foreground)
   (background :initarg :background :reader background)
   (text-style :initarg :text-style :reader stream-text-style)
   (vspace :initarg :vertical-spacing :reader stream-vertical-spacing)
   (margin :initarg :text-margin :writer (setf stream-text-margin))
   (eol :initarg :end-of-line-action :accessor stream-end-of-line-action)
   (eop :initarg :end-of-page-action :accessor stream-end-of-page-action)
   (view :initarg :default-view :accessor stream-default-view)
   (baseline :initform 0 :reader stream-baseline)
   ;; the max char height of the current line
   (char-height :initform 0 :accessor %stream-char-height))
  (:default-initargs
   :foreground +black+ :background +white+ :text-style *default-text-style*
   :vertical-spacing 2 :text-margin nil :end-of-line-action :wrap
   :end-of-page-action :scroll :default-view +textual-view+))

(defmethod stream-force-output :after
    ((stream standard-extended-output-stream))
  (with-sheet-medium (medium stream)
    (medium-force-output medium)))

(defmethod stream-finish-output :after
    ((stream standard-extended-output-stream))
  (with-sheet-medium (medium stream)
    (medium-finish-output medium)))

;;; STREAM-EFFECTIVE-*-MARGIN methods may return NIL if there is no margin
;;; specified. If corresponding text wrap margin is NIL then text never wraps,
;;; because end of line is never reached. Similar thing applies to lines, pages
;;; and top/bottom margins.
;;;
;;; For left-to-right text starting cursor position is
;;;   [(or effective-left-margin 0) (or effective-top-margin 0)]
;;;
;;; For right-to-left text starting cursor position is
;;;   [(or effective-right-margin 0) (or effective-top-margin 0)]
;;;
;;; -- jd 2019-01-09

(defgeneric stream-effective-left-margin (stream)
  (:documentation "Absolute position of the left margin in stream coordinates.")
  (:method ((stream standard-extended-output-stream))
    0))

(defgeneric stream-effective-top-margin (stream)
  (:documentation "Absolute position of the top margin in stream coordinates.")
  (:method ((stream standard-extended-output-stream))
    0))

(defgeneric stream-effective-right-margin (stream)
  (:documentation "Absolute position of the right margin in stream coordinates.")
  (:method ((stream standard-extended-output-stream))
    (or (slot-value stream 'margin)
        (let ((sheet (or (pane-viewport stream) stream)))
          ;; Sheet region may not be bound (ie in PostScript backend).
          (if (region-equal (sheet-region sheet) +everywhere+)
              nil
              (bounding-rectangle-width sheet))))))

(defgeneric stream-effective-bottom-margin (stream)
  (:documentation "Absolute position of the bottom margin in stream coordinates.")
  (:method ((stream standard-extended-output-stream))
    (bounding-rectangle-height stream)))

(defmethod initialize-instance :after
    ((stream standard-extended-output-stream) &rest args)
  (declare (ignore args))
  (setf (stream-text-cursor stream)
        (make-instance 'standard-text-cursor
                       :sheet stream
                       :x-position (stream-effective-left-margin stream)
                       :y-position (stream-effective-top-margin stream)))
  (setf (cursor-active (stream-text-cursor stream)) t))


(defmethod stream-cursor-position ((stream standard-extended-output-stream))
  (cursor-position (stream-text-cursor stream)))

(defmethod* (setf stream-cursor-position)
    (x y (stream standard-extended-output-stream))
  (setf (cursor-position (stream-text-cursor stream)) (values x y)))

(defgeneric stream-set-cursor-position (stream x y))

(defmethod stream-set-cursor-position
    ((stream standard-extended-output-stream) x y)
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

;; In a next few functions we can't call (setf stream-cursor-position) because
;; that would close the text-output-record unnecessarily. Using underlying
;; text-cursor with (setf cursor-position) is fine as long as the cursor is
;; off. If it were on that would close the output record too. -- jd 2019-01-07

(defmacro with-cursor-off (stream &body body)
  `(letf (((cursor-visibility (stream-text-cursor ,stream)) nil))
     ,@body))

(defgeneric maybe-end-of-page-action (stream y)
  (:method ((stream standard-extended-output-stream) y)
    (let ((bottom-margin (stream-effective-bottom-margin stream))
          (end-of-page-action (stream-end-of-page-action stream)))
      (when (and bottom-margin (> y bottom-margin))
        (%note-stream-end-of-page stream end-of-page-action y)
        (ecase end-of-page-action
          ((:scroll :allow)  nil)
          ((:wrap :wrap*)
           (setf (cursor-position (stream-text-cursor stream))
                 (values (nth-value 0 (stream-cursor-position stream))
                         (stream-effective-top-margin stream)))))))))

(defgeneric %note-stream-end-of-page (stream action new-height)
  (:method (stream action new-height)
    nil))

(defun seos-write-string (stream string &optional (start 0) end)
  (setq end (or end (length string)))
  (when (= start end)
    (return-from seos-write-string))
  (let* ((medium (sheet-medium stream))
         (text-style (medium-text-style medium))
         ;; fixme: remove assumption about the text direction (LTR).
         (left-margin  (stream-effective-left-margin stream))
         (right-margin (stream-effective-right-margin stream)))
    (maxf (slot-value stream 'baseline) (text-style-ascent text-style medium))
    (maxf (%stream-char-height stream)  (text-style-height text-style medium))
    (multiple-value-bind (cx cy) (stream-cursor-position stream)
      (maybe-end-of-page-action stream (+ cy (%stream-char-height stream)))
      (let* ((width (stream-string-width stream string
                                         :start start :end end
                                         :text-style text-style))
             (eol-action (stream-end-of-line-action stream))
             (eol-p (and right-margin (> (+ cx width) right-margin))))
        (when (or (null eol-p) (member eol-action '(:allow :scroll)))
          (stream-write-output stream string nil start end)
          (incf cx width)
          (setf (cursor-position (stream-text-cursor stream)) (values cx cy))
          (when (and right-margin
                     (> (+ cx width) right-margin)
                     (eql eol-action :scroll))
            (multiple-value-bind (tx ty)
                (bounding-rectangle-position (sheet-region stream))
              (scroll-extent stream (+ tx width) ty)))
          (return-from seos-write-string))
        ;; All new lines from here on are soft new lines, we could skip
        ;; closing the text-output-record and have multiline records to
        ;; allow gimmics like a dynamic reflow). Also text-style doesn't
        ;; change until the end of this function. -- jd 2019-01-10
        (labels ((string-fits-p (delta string-index)
                   (<= (stream-string-width stream string
                                            :start start :end string-index
                                            :text-style text-style)
                       delta))
                 (find-split (delta)
                   ;; To prevent infinite recursion if there isn't room for even a
                   ;; single character we start from (1+ start). -- jd 2019-01-08
                   (when (not (string-fits-p delta start))
                     (if (= right-margin (+ (or left-margin 0) delta))
                         (return-from find-split (1+ start))
                         (return-from find-split start)))
                   (if (text-style-fixed-width-p text-style medium)
                       (min end (+ start (floor delta (text-style-width text-style medium))))
                       (bisect (1+ start) end (curry #'string-fits-p delta))))
                 (find-split-by-word (delta)
                   (let ((space-indexes (line-break-opportunities string start end)))
                     (when (not (string-fits-p delta (elt space-indexes 0)))
                       (if (or (= right-margin (+ (or left-margin 0) delta))
                               (= (length space-indexes) 1))
                           ;; word exceeds whole line length
                           (return-from find-split-by-word (find-split delta))
                           ;; word exceeds part of the line length
                           (return-from find-split-by-word start)))
                     (let ((split (elt space-indexes
                                       (bisect 0 (1- (length space-indexes))
                                               (lambda (guess)
                                                 (string-fits-p delta (elt space-indexes guess)))))))
                       (if (= (1+ split) end)
                           end
                           ;; trim leading spaces
                           (position #\space string
                                     :start split
                                     :end end
                                     :test-not #'char=))))))
          (ecase eol-action
            (:wrap
             (do ((split (find-split (- right-margin cx))
                         (find-split (- right-margin (or left-margin 0)))))
                 ((= start end)
                  (return-from seos-write-string))
               (unless (= start split)
                 (stream-write-output stream string nil start split))
               (if (/= split end)
                   ;; print a soft newline
                   (stream-write-char stream #\newline)
                   ;; adjust the cursor
                   (progn
                     (setf (cursor-position (stream-text-cursor stream))
                           (values (+ (stream-effective-left-margin stream)
                                      (stream-string-width stream string
                                                           :start start :end split
                                                           :text-style text-style))
                                   (nth-value 1 (stream-cursor-position stream))))
                     (return-from seos-write-string)))
               (setf start split)))
            (:wrap*
             (do ((split (find-split-by-word (- right-margin cx))
                         (find-split-by-word (- right-margin (or left-margin 0)))))
                 ((= start end)
                  (return-from seos-write-string))
               (unless (= start split)
                 (stream-write-output stream string nil start split))
               (if (/= split end)
                   ;; print a soft newline
                   (stream-write-char stream #\newline)
                   ;; adjust the cursor
                   (progn
                     (setf (cursor-position (stream-text-cursor stream))
                           (values (+ (stream-effective-left-margin stream)
                                      (stream-string-width stream string
                                                           :start start :end split
                                                           :text-style text-style))
                                   (nth-value 1 (stream-cursor-position stream))))
                     (return-from seos-write-string)))
               (setf start split)))))))))

(defun seos-write-newline (stream)
  (let* ((current-cy       (nth-value 1 (stream-cursor-position stream)))
         (vertical-spacing (stream-vertical-spacing stream))
         (updated-cy       (+ current-cy
                              (%stream-char-height stream)
                              vertical-spacing)))
    (setf (cursor-position (stream-text-cursor stream))
          (values (stream-effective-left-margin stream)
                  updated-cy))
    (finish-output stream)       ; this will close the output record if recorded)
    (let* ((medium       (sheet-medium stream))
           (text-style   (medium-text-style medium))
           (new-baseline (text-style-ascent text-style medium))
           (new-height   (text-style-height text-style medium)))
      (maybe-end-of-page-action stream (+ updated-cy new-height))
      (setf (slot-value stream 'baseline) new-baseline
            (%stream-char-height stream)  new-height))))

(defgeneric stream-write-output (stream line string-width &optional start end)
  (:documentation
   "Writes the character or string LINE to STREAM. This function produces no more
than one line of output i.e., doesn't wrap. If STRING-WIDTH is non-nil, that is
used as the width where needed; otherwise STREAM-STRING-WIDTH will be called."))

;;; The cursor is in stream coordinates.
(defmethod stream-write-output ((stream standard-extended-output-stream)
                                line string-width
                                &optional (start 0) end)
  (declare (ignore string-width))
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
      (seos-write-string stream string seg-start end))))

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
  (stream-effective-right-margin stream))


(defmethod stream-line-height ((stream standard-extended-output-stream)
                               &key (text-style nil))
  (+ (text-style-height (or text-style (medium-text-style (sheet-medium stream)))
                        (sheet-medium stream))
     (stream-vertical-spacing stream)))

(defmethod stream-line-column ((stream standard-extended-output-stream))
  (multiple-value-bind (x y) (stream-cursor-position stream)
    (declare (ignore y))
    (floor x (stream-string-width stream " "))))

(defmethod stream-start-line-p ((stream standard-extended-output-stream))
  (multiple-value-bind (x y) (stream-cursor-position stream)
    (declare (ignore y))
    (zerop x)))

(locally
    (declare #+sbcl (sb-ext:muffle-conditions style-warning))
  (defmacro with-room-for-graphics ((&optional (stream t)
                                               &rest arguments
                                               &key (first-quadrant t)
                                               height
                                               (move-cursor t)
                                               (record-type ''standard-sequence-output-record))
                                     &body body)
    (declare (ignore first-quadrant height move-cursor record-type))
    (let ((cont (gensym "CONT."))
          (stream (stream-designator-symbol stream '*standard-output*)))
      `(labels ((,cont (,stream)
                  ,@body))
         (declare (dynamic-extent #',cont))
         (invoke-with-room-for-graphics #',cont ,stream ,@arguments)))))

(defmacro with-end-of-line-action ((stream action) &body body)
  (when (eq stream t)
    (setq stream '*standard-output*))
  (check-type stream symbol)
  `(letf (((stream-end-of-line-action ,stream) ,action))
     ,@body))

(defmacro with-end-of-page-action ((stream action) &body body)
  (when (eq stream t)
    (setq stream '*standard-output*))
  (check-type stream symbol)
  `(letf (((stream-end-of-page-action ,stream) ,action))
     ,@body))

(defmethod beep (&optional medium)
  (if medium
      (medium-beep medium)
      (when (sheetp *standard-output*)
        (medium-beep (sheet-medium *standard-output*)))))

(defgeneric scroll-quantum (pane)
  (:documentation "Returns the number of pixels respresenting a 'line', used
to computed distance to scroll in response to mouse wheel events."))

(defmethod scroll-quantum ((sheet standard-extended-output-stream))
  (stream-line-height sheet))
