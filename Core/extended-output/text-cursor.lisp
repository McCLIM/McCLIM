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

;;; Cursor-Mixin class
;;;
;;; FIXME why a mixin? The only subclass is the standard-text-cursor. Also
;;; instead of storing the character height in the output stream the height
;;; should be stored in the cursor instead. -- jd 2021-11-15
(defclass cursor-mixin ()
  ((sheet :initarg :sheet :reader cursor-sheet)
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
   (cursor-active :initform nil :accessor cursor-active)
   (cursor-state :initform nil :accessor cursor-state)))

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
  (stream-cursor-height (cursor-sheet cursor)))

(defmethod stream-cursor-height ((sheet sheet))
  (text-style-height (medium-text-style sheet) sheet))
