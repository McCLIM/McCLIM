;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2020-2023 Daniel Kochmański <daniel@turtleware.eu>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; The implementation of the internal buffer. Includes a sub-editor, cursors,
;;; slides and the kill ring. This file contains no "visual" representation
;;; and is not concerned with sheets and streams.

(in-package #:clim-internals)

(defclass internal-buffer (cluffer-standard-buffer:buffer)
  ((timestamp
    :accessor internal-buffer-timestamp
    :initform -1)
   (string
    :reader %internal-buffer-string
    :initform (make-array 0 :element-type 'character
                            :adjustable t
                            :fill-pointer t))
   ;; For convenience each buffer has a "headless" edit cursor. Clients will
   ;; usually define their own cursors that are drawn.
   (headless-cursor
    :reader internal-buffer-cursor
    :initform (make-instance 'cluffer-standard-line:right-sticky-cursor)))
  (:default-initargs
   :initial-line (make-instance 'cluffer-standard-line:open-line)))

(defmethod initialize-instance :after ((buffer internal-buffer) &key)
  (let ((cursor (internal-buffer-cursor buffer)))
    (cluffer:attach-cursor cursor (cluffer:find-line buffer 0))
    (values buffer cursor)))

(defun make-internal-buffer ()
  (make-instance 'internal-buffer))

(defun internal-buffer-string (buffer)
  (let ((string (%internal-buffer-string buffer))
        (time (cluffer-standard-buffer::current-time buffer)))
    (unless (= (internal-buffer-timestamp buffer) time)
      (setf (internal-buffer-timestamp buffer) time)
      (setf (fill-pointer string) 0)
      (with-output-to-string (stream string)
        (flet ((add-line (line)
                 (princ (line-string line) stream)
                 (unless (cluffer:last-line-p line)
                   (terpri stream))))
          (declare (dynamic-extent (function add-line)))
          (map-over-lines #'add-line buffer))))
    string))

;;; The kill ring buffer has the same representation as the internal buffer so
;;; adding and getting the kill object is just a fancy name for inserting a
;;; fresh line and getting items from a specified line.

;;; This function adds the object in a fresh line of the buffer.
(defun smooth-add-kill-object (buffer object merge)
  ;; The cursor is always located at the end of the line.
  (let ((cursor (internal-buffer-cursor buffer)))
    (ecase merge
      ((nil)
       (cluffer:end-of-line cursor)
       (unless (cluffer:beginning-of-buffer-p cursor)
         (cluffer:split-line cursor)))
      (:front
       (cluffer:beginning-of-line cursor))
      (:back
       (cluffer:end-of-line cursor)))
    (smooth-insert-items cursor object)))

;;; This function moves the cursor in the buffer and returns the line items.
(defun smooth-get-kill-object (buffer &optional (offset 0))
  (let ((cursor (internal-buffer-cursor buffer)))
    (unless (zerop offset)
      (smooth-warp-line (cluffer:buffer cursor) cursor offset)
      (cluffer:end-of-line cursor))
    (cluffer:items cursor)))

#+ (or)
(defun print-killring-buffer (cursor offset)
  (format *debug-io* "---------------------~%")
  (map-over-lines (lambda (p)
                    (cond ((and (zerop offset)
                                (cursor= p cursor))
                           (format *debug-io* "= kill: ~a~%" (line-string p)))
                          ((cursor= p cursor)
                           (format *debug-io* "< kill: ~a~%" (line-string p)))
                          ((= (mod (+ (cluffer:line-number cursor) offset)
                                   (cluffer:line-count (cluffer:buffer cursor)))
                              (cluffer:line-number p))
                           (format *debug-io* "> kill: ~a~%" (line-string p)))
                          ((format *debug-io* "  kill: ~a~%" (line-string p)))))
                  (cluffer:buffer cursor)))

;;; Mark is a visual object (external to the buffer) that points at one or more
;;; of its elements. Specifically: the superclass of cursors and slides.
(defclass buffer-mark ()
  ((visibility :initarg :visibility :accessor mark-visibility)
   (properties :initarg :properties :accessor mark-properties))
  (:default-initargs :visibility t :properties '()))

(defgeneric mark-attached-p (mark))
(defgeneric mark-visibility (mark))
(defgeneric attach-mark (mark target))
(defgeneric detach-mark (mark))

(defgeneric mark-visible-p (mark)
  (:method ((mark buffer-mark))
    (and (mark-attached-p mark)
         (mark-visibility mark))))

;;; Cursors implement behavior of input cursor and a visual object.

(defclass buffer-cursor (buffer-mark cluffer:cursor) ())

(defmethod mark-attached-p ((mark buffer-cursor))
  (cluffer:cursor-attached-p mark))

(defmethod attach-mark ((cursor buffer-cursor) position)
  (smooth-set-position cursor position))

(defmethod detach-mark ((mark buffer-cursor))
  (when (mark-attached-p mark)
    (cluffer:detach-cursor mark)))

(defclass buffer-lsticky-cursor
    (buffer-cursor standard-text-cursor cluffer-standard-line:left-sticky-cursor)
  ())

(defclass buffer-rsticky-cursor
    (buffer-cursor standard-text-cursor cluffer-standard-line:right-sticky-cursor)
  ())

(defun make-buffer-cursor (stickiness)
  (ecase stickiness
    (:lsticky (make-instance 'buffer-lsticky-cursor))
    (:rsticky (make-instance 'buffer-rsticky-cursor))))

;;; Slides may represent a pointer selection, last yank, an annotation etc. They
;;; may overlap or span multiple lines.

(defclass buffer-slide (buffer-mark)
  ((lcursor :initarg :lcursor :reader lcursor)
   (rcursor :initarg :rcursor :reader rcursor)
   (%anchor :initarg :%anchor :accessor %anchor)))

(defun make-buffer-slide (&optional anchor)
  (let ((c1 (make-buffer-cursor :lsticky))
        (c2 (make-buffer-cursor :rsticky)))
    (when anchor
      (smooth-set-position c1 anchor)
      (smooth-set-position c2 anchor))
    (make-instance 'buffer-slide :%anchor c1 :lcursor c1 :rcursor c2)))

(defmethod mark-attached-p ((mark buffer-slide))
  (mark-attached-p (%anchor mark)))

(defmethod attach-mark ((slide buffer-slide) position)
  (smooth-set-position (lcursor slide) position)
  (smooth-set-position (rcursor slide) position))

(defmethod detach-mark ((slide buffer-slide))
  (detach-mark (lcursor slide))
  (detach-mark (rcursor slide)))

(defun move-buffer-slide (slide position &optional extension)
  (attach-mark slide position)
  (when extension
    (extend-buffer-slide slide extension)))

(defun extend-buffer-slide (slide position)
  (let ((anchor (%anchor slide))
        (lcursor (lcursor slide))
        (rcursor (rcursor slide)))
    (if (cursor<= position anchor)
        (progn
          (smooth-set-position rcursor anchor)
          (smooth-set-position lcursor position)
          (setf (%anchor slide) rcursor))
        (progn
          (smooth-set-position lcursor anchor)
          (smooth-set-position rcursor position)
          (setf (%anchor slide) lcursor)))))

;;; Cluffer "smooth" utilities - CLIM spec operates on positions treating the
;;; input buffer as a vector. On the other hand Cluffer keeps each line as a
;;; separate entity, so we need to make things transparent and allow smooth
;;; transitioning between lines and addressing positions with an integer.

(defun cursor-previous-line (cursor)
  (let ((buf (cluffer:buffer cursor))
        (pos (1- (cluffer:line-number cursor))))
    (cluffer:find-line buf pos)))

(defun cursor-next-line (cursor)
  (let ((buf (cluffer:buffer cursor))
        (pos (1+ (cluffer:line-number cursor))))
    (cluffer:find-line buf pos)))

(defun cursor-linear-position (cursor)
  (loop with buffer = (cluffer:buffer cursor)
        with newlines = (cluffer:line-number cursor)
        with position = (+ newlines (cluffer:cursor-position cursor))
        for linum from 0 below newlines
        for line = (cluffer:find-line buffer linum)
        for count = (cluffer:item-count line)
        do (incf position count)
        finally (return (values position linum count))))

(defun (setf cursor-linear-position) (new-position cursor)
  (loop with buffer = (cluffer:buffer cursor)
        with position = 0
        for linum from 0 below (cluffer:line-count buffer)
        for line = (cluffer:find-line buffer linum)
        when (<= new-position (+ position (cluffer:item-count line))) do
          (cluffer:detach-cursor cursor)
          (cluffer:attach-cursor cursor line (- new-position position))
          (return-from cursor-linear-position
            (cluffer:cursor-position cursor))
        do (incf position (1+ (cluffer:item-count line)))
        finally (error "~s points beyond the buffer!" new-position)))

;;; "Smooth" operations glide over line boundaries as if we had a linear buffer.

(defun smooth-peek-item (cursor)
  (cond ((cluffer:end-of-buffer-p cursor)
         nil)
        ((cluffer:end-of-line-p cursor)
         #\newline)
        (t
         (cluffer:item-after-cursor cursor))))

(defun smooth-forward-item (cursor)
  (cond ((cluffer:end-of-buffer-p cursor)
         (beep))
        ((cluffer:end-of-line-p cursor)
         (let ((next (cursor-next-line cursor)))
           (cluffer:detach-cursor cursor)
           (cluffer:attach-cursor cursor next)
           #\newline))
        (t
         (cluffer:forward-item cursor)
         (cluffer:item-before-cursor cursor))))

(defun smooth-backward-item (cursor)
  (cond ((cluffer:beginning-of-buffer-p cursor)
         (beep))
        ((cluffer:beginning-of-line-p cursor)
         (let ((prev (cursor-previous-line cursor)))
           (cluffer:detach-cursor cursor)
           (cluffer:attach-cursor cursor prev)
           (cluffer:end-of-line cursor)
           #\newline))
        (t
         (cluffer:backward-item cursor)
         (cluffer:item-after-cursor cursor))))

(defun smooth-insert-item (cursor item)
  (if (char= item #\newline)
      (cluffer:split-line cursor)
      (cluffer:insert-item cursor item)))

(defun smooth-delete-item (cursor)
  (cond ((cluffer:end-of-buffer-p cursor)
         (beep)
         nil)
        ((cluffer:end-of-line-p cursor)
         (cluffer:join-line cursor)
         #\newline)
        (t
         (let ((item (cluffer:item-after-cursor cursor)))
           (cluffer:delete-item cursor)
           item))))

(defun smooth-erase-item (cursor)
  (cond ((cluffer:beginning-of-buffer-p cursor)
         (beep)
         nil)
        ((cluffer:beginning-of-line-p cursor)
         (cluffer:join-line (cursor-previous-line cursor))
         #\newline)
        (t
         (let ((item (cluffer:item-before-cursor cursor)))
           (cluffer:erase-item cursor)
           item))))

(defun smooth-set-position (cursor destination &optional position)
  (etypecase destination
    (cluffer:line
     (if (cluffer:cursor-attached-p cursor)
         (unless (eq destination (cluffer:line cursor))
           (cluffer:detach-cursor cursor)
           (cluffer:attach-cursor cursor destination))
         (cluffer:attach-cursor cursor destination)))
    (cluffer:cursor
     (smooth-set-position cursor
                          (cluffer:line destination)
                          (cluffer:cursor-position destination)))
    (integer
     (setf (cursor-linear-position cursor) destination)))
  (when position
    (setf (cluffer:cursor-position cursor) position)))

(defun smooth-clean-buffer (buffer cursor)
  (smooth-beg-of-buffer buffer cursor)
  (handler-case (loop (smooth-delete-line cursor))
    (cluffer:end-of-buffer ()))
  (assert (= 1 (cluffer:line-count buffer)))
  (assert (= 0 (cluffer:item-count (cluffer:line cursor)))))

(defun smooth-beg-of-buffer (buffer cursor)
  (symbol-macrolet ((line0 (cluffer:find-line buffer 0)))
    (unless (zerop (cluffer:line-number cursor))
      (cluffer:detach-cursor cursor)
      (cluffer:attach-cursor cursor line0)))
  (cluffer:beginning-of-line cursor))

(defun smooth-end-of-buffer (buffer cursor)
  (let ((cline (cluffer:line cursor))
        (bline (cluffer:find-line buffer (1- (cluffer:line-count buffer)))))
    (unless (eq cline bline)
      (cluffer:detach-cursor cursor)
      (cluffer:attach-cursor cursor bline)))
  (cluffer:end-of-line cursor))

(defun smooth-move-line (buffer cursor offset)
  (let* ((lnum (+ (cluffer:line-number cursor) offset))
         (cpos (cluffer:cursor-position cursor))
         (lmax (1- (cluffer:line-count buffer)))
         (next (cluffer:find-line buffer (clamp lnum 0 lmax))))
    (cluffer:detach-cursor cursor)
    (handler-case (cluffer:attach-cursor cursor next cpos)
      (cluffer:end-of-line ()
        (cluffer:end-of-line cursor)))))

(defun smooth-warp-line (buffer cursor offset)
  (let* ((lnum (+ (cluffer:line-number cursor) offset))
         (cpos (cluffer:cursor-position cursor))
         (lcnt (cluffer:line-count buffer))
         (next (cluffer:find-line buffer (mod lnum lcnt))))
    (cluffer:detach-cursor cursor)
    (handler-case (cluffer:attach-cursor cursor next cpos)
      (cluffer:end-of-line ()
        (cluffer:end-of-line cursor)))))

(defun smooth-jump-line (buffer cursor lnum)
  (let* ((lmax (1- (cluffer:line-count buffer)))
         (next (cluffer:find-line buffer (clamp lnum 0 lmax))))
    (cluffer:detach-cursor cursor)
    (cluffer:attach-cursor cursor next 0)))

(defun smooth-delete-line (cursor)
  (if (cluffer:end-of-line-p cursor)
      (cluffer:join-line cursor)
      (handler-case (loop (cluffer:delete-item cursor))
        (cluffer:end-of-line ()))))

(defun smooth-kill-line (cursor)
  (prog1 (if (cluffer:end-of-line-p cursor)
             (list #\newline)
             (copy-seq (cluffer:items cursor :start (cluffer:cursor-position cursor))))
    (smooth-delete-line cursor)))

(defun smooth-delete-input (slide)
  (loop with lcursor = (lcursor slide)
        with rcursor = (rcursor slide)
          initially (assert (cursor<= lcursor rcursor))
        while (cursor< lcursor rcursor)
        do (smooth-delete-item lcursor)))

(defun smooth-insert-input (cursor input)
  ;; This "insert" splits the line on a newline character.
  (map nil (curry #'smooth-insert-item cursor) input))

(defun smooth-insert-items (cursor items)
  ;; This "insert" preserves a newline character. Render goes crazy here :-).
  (map nil (curry #'cluffer:insert-item cursor) items))

(defun smooth-insert-line (cursor items)
  ;; This "insert" ignores a newline character.
  (do-sequence (item items)
    (unless (eql item #\newline)
      (cluffer:insert-item cursor item))))

(defun smooth-replace-input (slide items)
  (loop with lcursor = (lcursor slide)
        with rcursor = (rcursor slide)
          initially (assert (cursor<= lcursor rcursor))
        while (cursor< lcursor rcursor)
        do (smooth-delete-item lcursor)
        finally (smooth-insert-input rcursor items)))

(defun smooth-replace-line (slide items)
  (loop with lcursor = (lcursor slide)
        with rcursor = (rcursor slide)
          initially (assert (cursor<= lcursor rcursor))
        while (cursor< lcursor rcursor)
        do (smooth-delete-item lcursor)
        finally (smooth-insert-line rcursor items)))

;;; This DWIM operator compares line and cursor positions. When a cursor is
;;; compared with a line then 0 means "attached to a line". [-1 0 +1]
(defun cursor-compare (c1 c2)
  (let ((l1 (cluffer:line-number c1))
        (l2 (cluffer:line-number c2)))
    (cond ((< l1 l2) -1)
          ((> l1 l2) +1)
          ((or (typep c1 'cluffer:line)
               (typep c2 'cluffer:line))
           0)
          ((let ((p1 (cluffer:cursor-position c1))
                 (p2 (cluffer:cursor-position c2)))
             (cond ((< p1 p2) -1)
                   ((> p1 p2) +1)
                   (t          0)))))))

(macrolet ((defcmp (name cmp val)
             `(defun ,name (c1 c2)
                (,cmp (cursor-compare c1 c2) ,val))))
  (defcmp cursor< = -1)
  (defcmp cursor> = +1)
  (defcmp cursor= =  0)
  (defcmp cursor<= /= +1)
  (defcmp cursor>= /= -1))

;;; Another DWIM operator. A slide may be compared with a cursor, a line or with
;;; an another slide. The result is more nuanced because objects may partially
;;; overlap:
;;;
;;; [-3] - s1 strict before s2
;;; [-2] - s1 starts before s2 and ends inside s2
;;; [-1] - s1 starts before s2 and ends after s2 (contains)
;;; [ 0] - s1 and s2 denote the same region
;;; [+1] - s2 contains s1
;;; [+2] - s2 weakly precedes s1
;;; [+3] - s2 strict precedes s1
;;;
;;; If the beginning of one slide is at the same position as the ending of
;;; another then it is no overlap. This functions seems to be correct but I've
;;; never actually used it. Oh well - here goes my 2h on Saturday. -- jd
(defun slide-compare (s1 s2 &aux free)
  (flet ((get-position (obj)
           (etypecase obj
             (buffer-slide   (values (lcursor obj) (rcursor obj)))
             (cluffer:cursor (values obj obj))
             (cluffer:line
              (let* ((slide (make-buffer-slide obj))
                     (count (cluffer:item-count obj))
                     (lcursor (lcursor slide))
                     (rcursor (rcursor slide)))
                (push slide free)
                (setf (cluffer:cursor-position lcursor) count)
                (values lcursor rcursor)))))
         (strict= (q x y z)
           (if (and (cursor= q y) (cursor= x z))
               0
               nil))
         (inside= (q x y z)
           (cond ((and (<= q y) (<= z x)) -1) ; [q {y z] x}
                 ((and (<= y q) (<= x z)) +1) ; {y [q x] z}
                 (t nil)))
         (strict< (q x y z)
           (cond ((<= x y) -3)          ; {q x} [y z]
                 ((<= z q) +3)          ; [y z] {q x}
                 (nil)))
         (inside< (q x y z)
           (cond ((and (<= q y) (<= x z)) -2) ; {q [y x} z]
                 ((and (<= y q) (<= z x)) +2) ; [y {q z] x}
                 (nil))))
    (multiple-value-bind   (a b) (get-position s1)
      (multiple-value-bind (c d) (get-position s2)
        (unwind-protect (or (strict= a b c d)
                            (inside= a b c d)
                            (strict< a b c d)
                            (inside< a b c d)
                            (error "It is a miracle!"))
          (mapc #'detach-mark free))))))

;;; The continuation is expected to accept START and END arguments.
(defun map-over-lines-with-slides (function buffer slides)
  (let ((slides (remove-if-not #'mark-attached-p slides))
        (active '()))
    ;; Activation happens when the left cursor is on the same line as the
    ;; processed line. Deactivation happens similarily for the right cursor.
    (labels ((reactivate (op slide)
               (ecase op
                 (:add (push slide active))
                 (:del (setf active (delete slide active)
                             slides (delete slide slides)))))
             (butcher-line (line)
               ;; Why does it remind me a polygon triangulation? Ah, because
               ;; we compute monotonous segments. How cool is that?
               (loop for slide in slides
                     for lcursor = (lcursor slide)
                     for rcursor = (rcursor slide)
                     when (cursor= lcursor line)
                       collect (list (cluffer:cursor-position lcursor)
                                     :add slide lcursor)
                     when (cursor= rcursor line)
                       collect (list (cluffer:cursor-position rcursor)
                                     :del slide rcursor)))
             (process-line (line)
               (loop with start = 0
                     with end = (cluffer:item-count line)
                     for (pos op sel cur) in (butcher-line line)
                     do (cond
                          ((= pos start)
                           (reactivate op sel))
                          ((> pos start)
                           (funcall function line start pos active)
                           (setf start pos)
                           (reactivate op sel)))
                     finally
                        ;; - (zerop start) means "function not called yet"
                        ;; - (/= start end) implies the line reminder
                        (when (or (zerop start) (/= start end))
                          (funcall function line start end active)))))
      (map-over-lines #'process-line buffer))))

;;; Operations on cluffer's buffer and line instances.

(defun map-over-lines (function buffer)
  (loop with length = (cluffer:line-count buffer)
        for lineno from 0 below length
        for line = (cluffer:find-line buffer lineno)
        do (funcall function line)))

(defun map-over-slide (function slide)
  (when (mark-attached-p slide)
    (loop with lcursor = (lcursor slide)
          with llineno = (cluffer:line-number lcursor)
          with lcurpos = (cluffer:cursor-position lcursor)
          with rcursor = (rcursor slide)
          with rlineno = (cluffer:line-number rcursor)
          with rcurpos = (cluffer:cursor-position rcursor)
          with buffer = (cluffer:buffer lcursor)
          for lineno from llineno upto rlineno
          for args = `(,@(and (= lineno llineno) `(:start ,lcurpos))
                       ,@(and (= lineno rlineno) `(:end   ,rcurpos)))
          do (apply function (cluffer:find-line buffer lineno) args))))

(defun string-from-items (items &key start end)
  (with-output-to-string (str)
    (loop for index from (or start 0) below (or end (length items))
          for item = (aref items index)
          if (characterp item)
            do (princ item str)
          else
            do (format str "@~a" item))))

(defun line-string (line &rest args &key start end)
  (declare (ignore start end))
  (apply #'string-from-items (cluffer:items line) args))

(defun slide-string (slide)
  (when (mark-attached-p slide)
    (with-output-to-string (stream)
      (flet ((add-line (line &rest args &key start end)
               (declare (ignore start))
               (princ (apply #'line-string line args) stream)
               (unless end
                 (terpri stream))))
        (declare (dynamic-extent (function add-line)))
        (map-over-slide #'add-line slide)))))
