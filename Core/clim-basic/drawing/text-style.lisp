;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 1998-2000 by Michael McDonald <mikemac@mikemac.com>
;;;  (c) copyright 2002-2003 by Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;  (c) copyright 2014 by Robert Strandh <robert.strandh@gmail.com>
;;;  (c) copyright 2020 by Daniel Kochmański <daniel@turtleware.eu>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Implementation of the 11.1 Text Styles.
;;;

(in-package #:clim-internals)

;;; TODO

;;; - *UNDEFINED-TEXT-STYLE* is missing
;;; - Why is (EQ (MAKE-TEXT-STYLE NIL NIL 10) (MAKE-TEXT-STYLE NIL NIL 10.0005)) = T?
;;;   Does it matter?
;;; - Don't we want a weak hash-table for *TEXT-STYLE-HASH-TABLE*
;;;
;;; --GB 2002-02-26

;;; Notes

;;; The text-style protocol is kind of useless for now. How is an
;;; application programmer expected to implement new text-styles? I
;;; think we would need something like:
;;;
;;;  TEXT-STYLE-CHARACTER-METRICS text-style character[1]
;;;    -> width, ascent, descent, left-bearing, right-bearing
;;;
;;;  TEXT-STYLE-DRAW-TEXT text-style medium string x y
;;;  Or even better:
;;;  DESIGN-FROM-TEXT-STYLE-CHARACTER text-style character
;;;
;;;
;;; And when you start to think about it, text-styles are not fonts. So
;;; we need two protocols: A text style protocol and a font protocol.
;;;
;;; A text style is then something, which maps a sequence of characters
;;; into a couple of drawing commands, while probably using some font.
;;;
;;; While a font is something, which maps a _glyph index_ into a design.
;;;
;;; Example: Underlined with extra word spacing is a text style, while
;;;          Adobe Times Roman 12pt is a font.
;;;
;;; And [it can't be said too often] unicode is not a glyph encoding
;;; but more a kind of text formating.
;;;
;;; [1] or even a code position
;;; --GB

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defgeneric text-style-equalp (style1 style2)
    (:method ((style1 text-style) (style2 text-style))
      (eq style1 style2)))

  (defclass standard-text-style (text-style)
    ((family   :initarg :text-family
               :initform :fix
               :reader text-style-family)
     (face     :initarg :text-face
               :initform :roman
               :reader text-style-face)
     (size     :initarg :text-size
               :initform :normal
               :reader text-style-size)))

  (defmethod make-load-form ((obj standard-text-style) &optional env)
    (declare (ignore env))
    (with-slots (family face size) obj
      `(make-text-style ',family ',face ',size)))

  (defun family-key (family)
    (case family
      ((nil) 0)
      ((:fix) 1)
      ((:serif) 2)
      ((:sans-serif) 3)))

  (defun face-key (face)
    (typecase face
      (null 0)
      ((eql :roman) 1)
      ((eql :bold) 2)
      ((eql :italic) 3)
      ((cons (eql :bold) (cons (eql :italic))) 4)
      ((cons (eql :italic) (cons (eql :bold))) 4)))

  (defun size-key (size)
    (if (numberp size)
        (+ 10 (round (* 256 size)))
        (case size
          ((nil)         0)
          ((:tiny)       1)
          ((:very-small) 2)
          ((:small)      3)
          ((:normal)     4)
          ((:large)      5)
          ((:very-large) 6)
          ((:huge)       7)
          ((:smaller)    8)
          ((:larger)     9))))

  (defun text-style-key (family face size)
    (when-let ((size-key (size-key size))
               (face-key (face-key face))
               (family-key (family-key family)))
      (logior (ash size-key   8)
              (ash face-key   4)
              (ash family-key 0))))

  (eval-when (:compile-toplevel :load-toplevel :execute)
    (defvar *text-style-hash-table* (make-hash-table :test #'eql))
    (defvar *extended-text-style-hash-table* (make-hash-table :test #'equal)))

  (defun make-text-style (family face size)
    (if-let ((key (text-style-key family face size)))
      ;; Portable text styles have always been cached in McCLIM like
      ;; this: (as permitted by the CLIM spec for immutable objects,
      ;; section 2.4)
      ;; A 32-bit key has 24-bit for the output of `size-key'. That's
      ;; around font size 100000.
      (locally (declare (type (unsigned-byte 32) key))
        (ensure-gethash key *text-style-hash-table*
                        (make-text-style-1 family face size)))
      ;; Extended text styles using custom components is cached using
      ;; an appropriate hash table to ensure `EQL' of the same
      ;; extended text styles
      (ensure-gethash (list family face size) *extended-text-style-hash-table*
                      (make-text-style-1 family face size))))

  (defun make-text-style-1 (family face size)
    (make-instance 'standard-text-style
                   :text-family family
                   :text-face face
                   :text-size size)))

(defmethod print-object ((self standard-text-style) stream)
  (print-unreadable-object (self stream :type t :identity nil)
    (format stream "~{~S~^ ~}" (multiple-value-list (text-style-components self)))))

(defmethod text-style-equalp ((style1 standard-text-style)
                              (style2 standard-text-style))
  (and (equal (text-style-family style1) (text-style-family style2))
       (equal (text-style-face style1) (text-style-face style2))
       (eql (text-style-size style1) (text-style-size style2))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant *default-text-style* (make-text-style :sans-serif :roman :normal))
  (defconstant *undefined-text-style* *default-text-style*)

  (defconstant +smaller-sizes+ '(:huge :very-large :large :normal
                                 :small :very-small :tiny :tiny))

  (defconstant +larger-sizes+ '(:tiny :very-small :small :normal
                                :large :very-large :huge :huge))

  (defconstant +font-scaling-factor+ 4/3)
  (defconstant +font-min-size+ 6)
  (defconstant +font-max-size+ 48)

  (defconstant +font-sizes+
    '(:normal 14 :tiny 8 :very-small 10 :small 12 :large 18 :very-large 20 :huge 24)
    "Mapping between keyword and a font size."))

(declaim (inline normalize-font-size))
(defun normalize-font-size (size)
  (cond ((null size)
         (let ((size* (text-style-size *default-text-style*)))
           (etypecase size*
             (number size*)
             (symbol (getf +font-sizes+ size* nil)))))
        ((eq size :smaller)
         (getf +font-sizes+ (text-style-size *default-text-style*) nil))
        ((eq size :larger)
         (getf +font-sizes+ (text-style-size *default-text-style*) nil))
        ((realp size)
         (round (max size 2)))
        ((getf +font-sizes+ size nil))
        (t
         (error "~s is not a valid text style size!" size))))

(defun parse-text-style* (style)
  "Returns complete text-style without NIL components and with numeric size."
  (handler-case (flet ((new-text-style (family face size)
                         (let ((default *default-text-style*))
                           (make-text-style (or family (text-style-family default))
                                            (or face   (text-style-face   default))
                                            (normalize-font-size size)))))
                  (cond ((device-font-text-style-p style)
                         style)
                        ((text-style-p style)
                         (multiple-value-bind (family face size)
                             (text-style-components style)
                           (if (and (realp size)
                                    (text-style-components-fully-specified-p family face size))
                               style
                               (new-text-style family face size))))
                        ((null style)
                         (parse-text-style* *default-text-style*))
                        ((and (listp style) (alexandria:length= 3 style))
                         (destructuring-bind (family face size) style
                           (new-text-style family face size)))
                        (t (error "Invalid text style specification ~S." style))))
    (error (c)
      (warn (format nil "~a" c))
      *undefined-text-style*)))

(defun text-style-components-fully-specified-p (family face size)
  (and family
       face
       size
       (or (realp size)
           (member size (load-time-value
                         (remove-if-not #'keywordp +font-sizes+) t)))))

(defun text-style-fully-specified-p (text-style)
  (multiple-value-bind (family face size) (text-style-components text-style)
    (text-style-components-fully-specified-p family face size)))

(defun find-smaller-size (size)
  (if (numberp size)
      (max (round (/ size +font-scaling-factor+)) +font-min-size+)
      (cadr (member size +smaller-sizes+))))

(defun find-larger-size (size)
  (if (numberp size)
      (min (round (* size +font-scaling-factor+)) +font-max-size+)
      (cadr (member size +larger-sizes+))))

(defmethod text-style-components ((text-style standard-text-style))
  (values (text-style-family   text-style)
          (text-style-face     text-style)
          (text-style-size     text-style)))

;;; Device-Font-Text-Style class

(defclass device-font-text-style (text-style)
  ((display-device :initarg :display-device :accessor display-device)
   (device-font-name :initarg :device-font-name :accessor device-font-name)))

(defmethod print-object ((self device-font-text-style) stream)
  (print-unreadable-object (self stream :type t :identity nil)
    (format stream "~S on ~S" (device-font-name self) (display-device self))))

(defun device-font-text-style-p (s)
  (typep s 'device-font-text-style))

(defmethod text-style-components ((text-style device-font-text-style))
  (values :device :device :device))

(defmethod text-style-mapping ((port basic-port)
                               (text-style text-style)
                               &optional character-set)
  (declare (ignore port character-set))
  ;; INV the around method looks up the cache for the mapping. If we
  ;; end up here it is a game over. For consistency we return the
  ;; text-style like other methods. -- jd 2020-01-15
  text-style)

(defmethod text-style-mapping ((port basic-port)
                               text-style
                               &optional character-set)
  (text-style-mapping port (parse-text-style text-style) character-set))

(defmethod text-style-mapping :around ((port basic-port)
                                       (text-style text-style)
                                       &optional character-set)
  (declare (ignore character-set))
  ;; Cache mappings only for fully specified text styles.
  (let ((mappings (port-text-style-mappings port)))
    (if (or (device-font-text-style-p text-style)
            (text-style-fully-specified-p text-style))
        (ensure-gethash text-style mappings (call-next-method))
        (or (gethash text-style mappings) (call-next-method)))))

(defmethod (setf text-style-mapping) (mapping
                                      (port basic-port)
                                      text-style
                                      &optional character-set)
  (declare (ignore character-set))
  (setf (text-style-mapping port (parse-text-style text-style)) mapping))

(defmethod (setf text-style-mapping) (mapping
                                      (port basic-port)
                                      (text-style text-style)
                                      &optional character-set)
  (declare (ignore character-set))
  (when (listp mapping)
    (error "Delayed mapping is not supported.")) ; FIXME
  (setf (gethash text-style (port-text-style-mappings port))
        mapping))

(defmethod make-device-font-text-style (port font-name)
  (let ((text-style (make-instance 'device-font-text-style
                                   :display-device port
                                   :device-font-name font-name)))
    text-style))

;;; Text-style utilities

(defmethod merge-text-styles (s1 s2)
  (when (and (typep s1 'text-style)
             (eq s1 s2))
    (return-from merge-text-styles s1))
  (setq s1 (parse-text-style s1))
  (setq s2 (parse-text-style s2))
  (if (and (not (device-font-text-style-p s1))
           (not (device-font-text-style-p s2)))
      (let* ((family (or (text-style-family s1) (text-style-family s2)))
             (face1 (text-style-face s1))
             (face2 (text-style-face s2))
             (face (if (or (and (eq face1 :bold)   (eq face2 :italic))
                           (and (eq face1 :italic) (eq face2 :bold)))
                       '(:bold :italic)
                       (or face1 face2)))
             (size1 (text-style-size s1))
             (size2 (text-style-size s2))
             (size (case size1
                     ((nil) size2)
                     (:smaller (find-smaller-size size2))
                     (:larger (find-larger-size size2))
                     (t size1))))
        (make-text-style family face size))
      s1))

(defun parse-text-style (style)
  (cond ((text-style-p style) style)
        ((null style) (make-text-style nil nil nil)) ; ?
        ((and (listp style) (alexandria:length= 3 style))
         (destructuring-bind (family face size) style
           (make-text-style family face size)))
        (t (error "Invalid text style specification ~S." style))))

(defmacro with-text-style ((medium text-style) &body body)
  (when (eq medium t)
    (setq medium '*standard-output*))
  (check-type medium symbol)
  (with-gensyms (cont)
    `(flet ((,cont (,medium)
              ,(declare-ignorable-form* medium)
              ,@body))
       (declare (dynamic-extent #',cont))
       (invoke-with-text-style ,medium #',cont
                               (parse-text-style ,text-style)))))

(defmethod invoke-with-text-style ((sheet sheet) continuation text-style)
  (let ((medium (sheet-medium sheet))) ; FIXME: WITH-SHEET-MEDIUM
    (with-text-style (medium text-style)
      (funcall continuation sheet))))

(defmethod invoke-with-text-style ((medium medium) continuation text-style)
  (invoke-with-drawing-options
   medium continuation
   :text-style (merge-text-styles text-style (medium-merged-text-style medium))))

;;; For compatibility with real CLIM, which apparently lets you call this
;;; on non-CLIM streams.

(defmethod invoke-with-text-style ((medium t) continuation text-style)
  (declare (ignore text-style))
  (funcall continuation medium))

(defmacro with-text-family ((medium family) &body body)
  (declare (type symbol medium))
  (when (eq medium t)
    (setq medium '*standard-output*))
  (with-gensyms (cont)
    `(flet ((,cont (,medium)
              ,(declare-ignorable-form* medium)
              ,@body))
       (declare (dynamic-extent #',cont))
       (invoke-with-text-style ,medium #',cont
                               (make-text-style ,family nil nil)))))

(defmacro with-text-face ((medium face) &body body)
  (declare (type symbol medium))
  (when (eq medium t)
    (setq medium '*standard-output*))
  (with-gensyms (cont)
    `(flet ((,cont (,medium)
              ,(declare-ignorable-form* medium)
              ,@body))
       (declare (dynamic-extent #',cont))
       (invoke-with-text-style ,medium #',cont
                               (make-text-style nil ,face nil)))))

(defmacro with-text-size ((medium size) &body body)
  (declare (type symbol medium))
  (when (eq medium t) (setq medium '*standard-output*))
  (with-gensyms (cont)
    `(flet ((,cont (,medium)
              ,(declare-ignorable-form* medium)
              ,@body))
       (declare (dynamic-extent #',cont))
       (invoke-with-text-style ,medium #',cont
                               (make-text-style nil nil ,size)))))

;;; These below were just hot fixes, are there still needed? Are even
;;; half-way correct? --GB
;;;
;;; These are needed, and are correct.  "Implementations should also
;;; provide a ``trampoline'' for this generic function for output sheets; the
;;; trampoline will simply call the method for the medium. -- moore
;;;
;;; Thanks! --GB

(defmethod text-size ((sheet sheet) string &rest more)
  (apply #'text-size (sheet-medium sheet) string more))

(defmethod text-style-ascent (ts (sheet sheet))
  (text-style-ascent ts (sheet-medium sheet)))

(defmethod text-style-descent (ts (sheet sheet))
  (text-style-descent ts (sheet-medium sheet)))

(defmethod text-style-height (ts (sheet sheet))
  (text-style-height ts (sheet-medium sheet)))

(defmethod text-style-width (ts (sheet sheet))
  (text-style-width ts (sheet-medium sheet)))

;;; Fallback methods for the text style metrics.

(defmethod text-style-height (text-style medium)
  (+ (text-style-ascent text-style medium)
     (text-style-descent text-style medium)))

(defmethod text-style-width (text-style medium)
  (text-style-character-width text-style medium #\M))

(defmethod text-style-fixed-width-p (text-style medium)
  (eql (text-style-family text-style) :fix))
