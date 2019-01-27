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

;;; STANDARD-MARGINS
;;;
;;; I don't think we support model where stream (hence sheet) starts at a
;;; different coordinate than (0,0) but we might want to that in the
;;; future. This code is resilent to such change by specifying separately
;;; top/left margin and absolute coordinates of said margins. -- jd 2019-01-09

(defclass standard-margins ()
  (;; relative margin positions
   (left-margin   :initarg :left-margin   :accessor left-margin   :writer set-left-margin)
   (top-margin    :initarg :top-margin    :accessor top-margin    :writer set-top-margin)
   (right-margin  :initarg :right-margin  :accessor right-margin  :writer set-right-margin)
   (bottom-margin :initarg :bottom-margin :accessor bottom-margin :writer set-bottom-margin)
   ;; absolute margin positions
   (margin-x1 :initarg :margin-x1 :initform nil :accessor margin-x1 :writer set-margin-x1)
   (margin-y1 :initarg :margin-y1 :initform nil :accessor margin-y1 :writer set-margin-y1)
   (margin-x2 :initarg :margin-x2 :initform nil :accessor margin-x2 :writer set-margin-x2)
   (margin-y2 :initarg :margin-y2 :initform nil :accessor margin-y2 :writer set-margin-y2))
  (:default-initargs))

;;; SETF :before method sets the counterpart margin to NIL.
(macrolet ((thunk (margin-accessor corresponding-margin-writer)
             `(defmethod (setf ,margin-accessor) :before (value (margin standard-margins))
                (if (null value)
                    (error "Margin can't be set to NIL.")
                    (,corresponding-margin-writer nil margin)))))
  (thunk left-margin   set-margin-x1)
  (thunk right-margin  set-margin-x2)
  (thunk top-margin    set-margin-y1)
  (thunk bottom-margin set-margin-y2)
  (thunk margin-x1     set-left-margin)
  (thunk margin-x2     set-right-margin)
  (thunk margin-y1     set-top-margin)
  (thunk margin-y2     set-bottom-margin))

(defmethod initialize-instance :after ((margin standard-margins)
                                       &key text-margin ;kludge
                                         margin-x1 margin-y1 margin-x2 margin-y2
                                         (left-margin 0 left-margin-p)
                                         (top-margin 0 top-margin-p)
                                         (right-margin 0 right-margin-p)
                                         (bottom-margin 0 bottom-margin-p)
                                       &allow-other-keys)
  ;; Kludge: text-margin is part of the seos protocol and it means margin-x2.
  (when text-margin
    (assert (null margin-x2) () "TEXT-MARGIN and MARGIN-X2 initargs are mutually exclusive.")
    (set-margin-x2 text-margin margin))
  ;; Check that each margin has either relative or absolute position (or none).
  (assert (null (or (and margin-x1 left-margin-p)
                    (and margin-y1 top-margin-p)
                    (and margin-x2 right-margin-p)
                    (and margin-y2 bottom-margin-p)))
          ()
          "Margin can't be specified to have both relative and absolute position.")
  ;; Captain? You seem a little defensive.
  (if (null margin-x1)
      (set-left-margin left-margin margin)
      (set-margin-x1 margin-x1 margin))
  (if (null margin-y1)
      (set-top-margin top-margin margin)
      (set-margin-y1 margin-y1 margin))
  (if (null margin-x2)
      (set-right-margin right-margin margin)
      (set-margin-x2 margin-x2 margin))
  (if (null margin-y2)
      (set-bottom-margin bottom-margin margin)
      (set-margin-y2 margin-y2 margin)))

(defun make-margins (margin &key text-margin ;kludge
                              margin-x1 margin-y1 margin-x2 margin-y2
                              (left-margin 0 left-margin-p)
                              (top-margin 0 top-margin-p)
                              (right-margin 0 right-margin-p)
                              (bottom-margin 0 bottom-margin-p))
  ;; Kludge: text-margin is part of the seos protocol and it means margin-x2.
  (when text-margin
    (assert (null margin-x2) () "TEXT-MARGIN and MARGIN-X2 initargs are mutually exclusive.")
    (set-margin-x2 text-margin margin))
  ;; Check that each margin has either relative or absolute position (or none).
  (assert (null (or (and margin-x1 left-margin-p)
                    (and margin-y1 top-margin-p)
                    (and margin-x2 right-margin-p)
                    (and margin-y2 bottom-margin-p)))
          ()
          "Margin can't be specified to have both relative and absolute position.")
  ;; Captain? You seem a little defensive.
  (cond (margin-x1   (set-margin-x1 margin-x1 margin))
        (left-margin (set-left-margin left-margin margin)))
  (cond (margin-y1  (set-margin-y1 margin-y1 margin))
        (top-margin (set-top-margin top-margin margin)))
  (cond (margin-x2    (set-margin-x2 margin-x2 margin))
        (right-margin (set-right-margin right-margin margin)))
  (cond (margin-y2     (set-margin-y2 margin-y2 margin))
        (bottom-margin (set-bottom-margin bottom-margin margin))))


;;; Standard-Extended-Output-Stream class

(defclass standard-extended-output-stream (extended-output-stream
                                           standard-output-stream)
  ((cursor :accessor stream-text-cursor)
   (margin :accessor stream-text-margins)
   (foreground :initarg :foreground :reader foreground)
   (background :initarg :background :reader background)
   (text-style :initarg :text-style :reader stream-text-style)
   (vspace :initarg :vertical-spacing :reader stream-vertical-spacing)
   (eol :initarg :end-of-line-action :accessor stream-end-of-line-action)
   (eop :initarg :end-of-page-action :accessor stream-end-of-page-action)
   (view :initarg :default-view :accessor stream-default-view)
   (baseline :initform 0 :reader stream-baseline)
   ;; the max char height of the current line
   (char-height :initform 0 :accessor %stream-char-height))
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


;;; STREAM-EFFECTIVE-*-MARGIN methods always return a number. If there is no
;;; margin specified [0,0] x [80,43] size is assumed (default text style
;;; character size unit).
;;;
;;; For left-to-right text starting cursor position is
;;;   [effective-left-margin effective-top-margin]
;;;
;;; For right-to-left text starting cursor position is
;;;   [effective-right-margin effective-top-margin]
;;;
;;; To abstract this away functions PAGE-INITIAL-CURSOR-POSITION and
;;; PAGE-FINAL-CURSOR-POSITION are defined. RTL is not supported yet.
;;;
;;; -- jd 2019-01-27

(defgeneric stream-effective-left-margin (stream)
  (:documentation "Absolute position of the left margin in stream coordinates.")
  (:method ((stream standard-extended-output-stream))
    (let ((margins (stream-text-margins stream)))
      (or (margin-x1 margins)
          (let ((sheet (or (pane-viewport stream) stream)))
            ;; Sheet region may be +everywhere+ (i.e. in PostScript's paper :eps).
            (if (region-equal (sheet-region sheet) +everywhere+)
                0
                (+ (bounding-rectangle-min-x sheet) (left-margin margins))))))))

(defgeneric stream-effective-top-margin (stream)
  (:documentation "Absolute position of the top margin in stream coordinates.")
  (:method ((stream standard-extended-output-stream))
    (let ((margins (stream-text-margins stream)))
      (or (margin-y1 margins)
          (let ((sheet (or (pane-viewport stream) stream)))
            ;; Sheet region may be +everywhere+ (i.e. in PostScript's paper :eps).
            (if (region-equal (sheet-region sheet) +everywhere+)
                0
                (+ (bounding-rectangle-min-y sheet) (top-margin margins))))))))

(defgeneric stream-effective-right-margin (stream)
  (:documentation "Absolute position of the right margin in stream coordinates.")
  (:method ((stream standard-extended-output-stream))
    (let ((margins (stream-text-margins stream)))
      (or (margin-x2 margins)
          (let ((sheet (or (pane-viewport stream) stream)))
            ;; Sheet region may be +everywhere+ (i.e. in PostScript's paper :eps).
            (- (if (region-equal (sheet-region sheet) +everywhere+)
                   (* 80 (text-style-width *default-text-style* stream))
                   (bounding-rectangle-max-x sheet))
               (right-margin margins)))))))

(defgeneric stream-effective-bottom-margin (stream)
  (:documentation "Absolute position of the bottom margin in stream coordinates.")
  (:method ((stream standard-extended-output-stream))
    (let ((margins (stream-text-margins stream)))
      (or (margin-y2 margins)
          (let ((sheet (or (pane-viewport stream) stream)))
            ;; Sheet region may be +everywhere+ (i.e. in PostScript's paper :eps).
            (- (if (region-equal (sheet-region sheet) +everywhere+)
                   (* 43 (text-style-height *default-text-style* stream))
                   (bounding-rectangle-max-y sheet))
               (bottom-margin margins)))))))

(defgeneric page-cursor-initial-position (stream)
  (:documentation "Returns two values: x and y initial position for a cursor on page.")
  (:method ((stream standard-extended-output-stream))
    (values (stream-effective-left-margin stream)
            (stream-effective-top-margin stream))))

(defgeneric page-cursor-final-position (stream)
  (:documentation "Returns two values: x and y final position for a cursor on page.")
  (:method ((stream standard-extended-output-stream))
    (values (stream-effective-right-margin stream)
            (stream-effective-bottom-margin stream))))

(defgeneric invoke-with-temporary-page (stream continuation margins &key move-cursor)
  (:method ((stream standard-extended-output-stream) continuation margins &key (move-cursor t))
    (letf (((stream-text-margins stream) margins))
      (multiple-value-bind (cx cy) (stream-cursor-position stream)
        (setf (stream-cursor-position stream) (page-cursor-initial-position stream))
        (funcall continuation stream)
        (unless move-cursor
          (setf (stream-cursor-position stream) (values cx cy)))))))

(defmacro with-temporary-page ((stream margin-spec
                               &rest args &key (move-cursor t))
                               &body body)
  (declare (ignore move-cursor))
  (setq stream (stream-designator-symbol stream '*standard-output*))
  (with-gensyms (continuation margins)
    `(flet ((,continuation (,stream) ,@body))
       (let ((,margins (shallow-copy-object (stream-text-margins ,stream))))
         (make-margins ,margins ,@margin-spec)
         (invoke-with-temporary-page ,stream #',continuation ,margins ,@args)))))



(defmethod initialize-instance :after
    ((stream standard-extended-output-stream) &rest args &key &allow-other-keys)
  (setf (stream-text-margins stream)
        (apply #'make-instance 'standard-margins args))
  (multiple-value-bind (x-start y-start)
      (page-cursor-initial-position stream)
    (setf (stream-text-cursor stream)
          (make-instance 'standard-text-cursor
                         :sheet stream
                         :x-position x-start
                         :y-position y-start)))
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
    (let ((bottom-margin (nth-value 1 (page-cursor-final-position stream)))
          (end-of-page-action (stream-end-of-page-action stream)))
      (when (> y bottom-margin)
        (%note-stream-end-of-page stream end-of-page-action y)
        (ecase end-of-page-action
          ((:scroll :allow)  nil)
          ((:wrap :wrap*)
           (setf (cursor-position (stream-text-cursor stream))
                 (values (nth-value 0 (stream-cursor-position stream))
                         (nth-value 1 (page-cursor-initial-position stream))))))))))

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
         (left-margin  (page-cursor-initial-position stream))
         (right-margin (page-cursor-final-position stream)))
    (maxf (slot-value stream 'baseline) (text-style-ascent text-style medium))
    (maxf (%stream-char-height stream)  (text-style-height text-style medium))
    (multiple-value-bind (cx cy) (stream-cursor-position stream)
      (maybe-end-of-page-action stream (+ cy (%stream-char-height stream)))
      (let* ((width (stream-string-width stream string
                                         :start start :end end
                                         :text-style text-style))
             (eol-action (stream-end-of-line-action stream))
             (eol-p (> (+ cx width) right-margin)))
        (when (or (null eol-p) (member eol-action '(:allow :scroll)))
          (stream-write-output stream string nil start end)
          (incf cx width)
          (setf (cursor-position (stream-text-cursor stream)) (values cx cy))
          (when (and (> (+ cx width) right-margin)
                     (eql eol-action :scroll))
            (multiple-value-bind (tx ty)
                (bounding-rectangle-position (sheet-region stream))
              (scroll-extent stream (+ tx width) ty)))
          (return-from seos-write-string))
        ;; All new lines from here on are soft new lines, we could skip
        ;; closing the text-output-record and have multiline records to
        ;; allow gimmics like a dynamic reflow). Also text-style doesn't
        ;; change until the end of this function. -- jd 2019-01-10
        (let* ((width (if (text-style-fixed-width-p text-style medium)
                          (text-style-width text-style medium)
                          (lambda (string start end)
                            (text-size medium string
                                       :text-style text-style
                                       :start start :end end))))
               (splits (line-breaks string width
                                    :initial-offset (- cx left-margin)
                                    :margin (- right-margin left-margin)
                                    :break-strategy (ecase eol-action
                                                      (:wrap NIL)
                                                      (:wrap* T))
                                    :start start :end end)))
          (do ((start start (car split))
               (split splits (rest split)))
              ((null split)
               (stream-write-output stream string nil start end)
               (setf (cursor-position (stream-text-cursor stream))
                     (values (+ left-margin
                                (stream-string-width stream string
                                                     :start start :end split
                                                     :text-style text-style))
                             (nth-value 1 (stream-cursor-position stream)))))
            (ecase eol-action
              (:wrap  (stream-write-output stream string nil start (car split)))
              (:wrap* (let ((pos (position #\space string
                                           :from-end t :start start :end (car split)
                                           :test-not #'char=)))
                        (when pos (incf pos))
                        (stream-write-output stream string nil start (or pos (car split))))))
            ;; print a soft newline
            (stream-write-char stream #\newline)))))))

(defun seos-write-newline (stream)
  (let* ((current-cy       (nth-value 1 (stream-cursor-position stream)))
         (vertical-spacing (stream-vertical-spacing stream))
         (updated-cy       (+ current-cy
                              (%stream-char-height stream)
                              vertical-spacing)))
    (setf (cursor-position (stream-text-cursor stream))
          (values (page-cursor-initial-position stream)
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

(defmethod* (setf stream-text-margin) (margin (stream standard-extended-output-stream))
  (setf (margin-x2 stream) margin))

(defmethod stream-line-height ((stream standard-extended-output-stream)
                               &key (text-style nil))
  (+ (text-style-height (or text-style (medium-text-style (sheet-medium stream)))
                        (sheet-medium stream))
     (stream-vertical-spacing stream)))

(defmethod stream-line-column ((stream standard-extended-output-stream))
  (multiple-value-bind (x y) (stream-cursor-position stream)
    (declare (ignore y))
    (floor x (stream-string-width stream "m"))))

(defmethod stream-start-line-p ((stream standard-extended-output-stream))
  (multiple-value-bind (x y) (stream-cursor-position stream)
    (declare (ignore y))
    (= x (page-cursor-initial-position stream))))

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
       (invoke-with-room-for-graphics #',cont ,stream ,@arguments))))

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
