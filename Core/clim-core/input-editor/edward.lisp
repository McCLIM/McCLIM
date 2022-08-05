;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2020 Daniel Kochmański <daniel@turtleware.eu>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Edward - the input editor (based on Cluffer).
;;;

(in-package #:climi)

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

(defclass edward-mixin ()
  ((edward-buffer :reader input-editor-buffer)
   (edit-cursor :reader edit-cursor)
   (scan-cursor :reader scan-cursor)
   ;; (edward-kill-history :reader edward-killring)
   ;; (edward-undo-history :reader edward-undo-history)
   ;; (edward-redo-history :reader edward-redo-history)
   ))

(defmethod shared-initialize :after ((object edward-mixin) slot-names &key)
  (declare (ignore slot-names))
  (let* ((i-cursor (make-instance 'cluffer-standard-line:right-sticky-cursor))
         (s-cursor (make-instance 'cluffer-standard-line:left-sticky-cursor))
         (line (make-instance 'cluffer-standard-line:open-line))
         (buffer (make-instance 'cluffer-standard-buffer:buffer
                                :initial-line line)))
    (cluffer:attach-cursor i-cursor line 0)
    (cluffer:attach-cursor s-cursor line 0)
    (setf (slot-value object 'edward-buffer) buffer
          (slot-value object 'edit-cursor) i-cursor
          (slot-value object 'scan-cursor) s-cursor)))

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

(defun smooth-delete-item (cursor)
  (cond ((cluffer:end-of-buffer-p cursor)
         (beep))
        ((cluffer:end-of-line-p cursor)
         (cluffer:join-line cursor))
        (t
         (cluffer:delete-item cursor))))

(defun smooth-erase-item (cursor)
  (cond ((cluffer:beginning-of-buffer-p cursor)
         (beep))
        ((cluffer:beginning-of-line-p cursor)
         (cluffer:join-line (cursor-previous-line cursor)))
        (t
         (cluffer:erase-item cursor))))

(defun edward-buffer-string (editor)
  (with-output-to-string (str)
    (loop with buffer = (input-editor-buffer editor)
          with length = (cluffer:line-count buffer)
          for lineno from 0 below length
          for line = (cluffer:find-line buffer lineno)
          for text = (coerce (cluffer:items line) 'string)
          do (princ text str)
          unless (= (1+ lineno) length)
            do (terpri str))))

(defun edward-replace-input
    (editor new-input start end buffer-start)
  (let ((buffer (input-editor-buffer editor))
        (cursor (make-instance 'cluffer-standard-line:right-sticky-cursor))
        (open-cursor-position (if (integerp buffer-start)
                                  buffer-start
                                  (cursor-linear-position buffer-start)))
        (scan-cursor-position (cursor-linear-position (scan-cursor editor))))
    (flet ((thunk ()
             (setf (cursor-linear-position cursor) open-cursor-position)
             ;; The scan cursor is "left sticky", so the order of operation
             (loop for i from start below end do
               (cluffer:insert-item cursor (char new-input i)))
             (loop repeat (- scan-cursor-position open-cursor-position) do
               (smooth-delete-item cursor))))
      (cluffer:attach-cursor cursor (cluffer:find-line buffer 0))
      (unwind-protect (thunk)
        (cluffer:detach-cursor cursor)))))

;;; editor function prototype

;; (defgeneric edward-move (cursor unit syntax numeric-argument))
;; ;;; (edward-move cursor :char <text> +1)
;; ;;; (edward-move cursor :word <text> +1)
;; ;;; (edward-move cursor :line <text> -2)

;; (defgeneric edward-search (cursor object syntax numeric-argument))
;; ;;; (edward-move cursor "s/abc/g" <regexp>  1)
;; ;;; (edward-move cursor "s/abc/g" <regexp> -1)
;; ;;; (edward-move cursor "abc"     <search> 1)

;; (defgeneric edward-clear (cursor unit syntax numeric-argument))
;; ;;; (edward-clear cursor :line   <text> 0)
;; ;;; (edward-clear cursor :buffer <text> 0)

;; (defgeneric edward-delete (cursor unit syntax numeric-argument))
;; ;;; (edward-delete cursor :char <text> -1)
;; ;;; (edward-delete cursor :char <text> +1)

;; (defgeneric edward-transpose (cursor unit syntax numeric-argument))
;; ;;; (edward-transpose cursor :char <text> 1)

;; (defgeneric edward-insert (cursor object syntax numeric-argument))
;; ;;; (edward-insert cursor cursor #\newline <text> -1)

(defmethod ie-forward-object ((sheet edward-mixin)
                              (buffer cluffer:buffer)
                              event
                              &optional (numeric-argument 1))
  (declare (ignore event))
  (loop with cursor = (edit-cursor sheet)
        repeat numeric-argument
        do (smooth-forward-item cursor)))

(defmethod ie-forward-word ((sheet edward-mixin)
                            (buffer cluffer:buffer)
                            event
                            &optional (numeric-argument 1))
  (loop with cursor = (edit-cursor sheet)
        repeat numeric-argument
        do (loop do (ie-forward-object sheet buffer event)
                 until (or (cluffer:end-of-line-p cursor)
                           (cluffer:end-of-buffer-p cursor)
                           (char= (cluffer:item-after-cursor cursor) #\space)))))

(defmethod ie-backward-object ((sheet edward-mixin)
                               (buffer cluffer:buffer)
                               event
                               &optional (numeric-argument 1))
  (declare (ignore event))
  (loop with cursor = (edit-cursor sheet)
        repeat numeric-argument
        do (smooth-backward-item cursor)))

(defmethod ie-backward-word ((sheet edward-mixin)
                             (buffer cluffer:buffer)
                             event
                             &optional (numeric-argument 1))
  (loop with cursor = (edit-cursor sheet)
        repeat numeric-argument
        do (loop do (ie-backward-object sheet buffer event)
                 until (or (cluffer:beginning-of-line-p cursor)
                           (cluffer:beginning-of-buffer-p cursor)
                           (char= (cluffer:item-before-cursor cursor) #\space)))))

(defmethod ie-beginning-of-line ((sheet edward-mixin)
                                 (buffer cluffer:buffer)
                                 event
                                 &optional numeric-argument)
  (declare (ignore event numeric-argument))
  (cluffer:beginning-of-line (edit-cursor sheet)))

(defmethod ie-end-of-line ((sheet edward-mixin)
                           (buffer cluffer:buffer)
                           event
                           &optional numeric-argument)
  (declare (ignore event numeric-argument))
  (cluffer:end-of-line (edit-cursor sheet)))


(defmethod ie-next-line ((sheet edward-mixin)
                         (buffer cluffer:buffer)
                         event
                         &optional (numeric-argument 1))
  (declare (ignore event))
  (loop with cursor = (edit-cursor sheet)
        with cpos = (cluffer:cursor-position cursor)
        repeat numeric-argument
        do (handler-case
               (let* ((lnum (1+ (cluffer:line-number cursor)))
                      (next (cluffer:find-line buffer lnum)))
                 (cluffer:detach-cursor cursor)
                 (cluffer:attach-cursor cursor next cpos))
             (cluffer:end-of-buffer ()
               (return))
             (cluffer:end-of-line ()
               (cluffer:end-of-line cursor)))))

(defmethod ie-previous-line ((sheet edward-mixin)
                             (buffer cluffer:buffer)
                             event
                             &optional (numeric-argument 1))
  (declare (ignore event))
  (loop with cursor = (edit-cursor sheet)
        with cpos = (cluffer:cursor-position cursor)
        repeat numeric-argument
        do (handler-case
               (let* ((lnum (1- (cluffer:line-number cursor)))
                      (next (cluffer:find-line buffer lnum)))
                 (cluffer:detach-cursor cursor)
                 (cluffer:attach-cursor cursor next cpos))
             (cluffer:beginning-of-buffer ()
               (return))
             (cluffer:end-of-line ()
               (cluffer:end-of-line cursor)))))

(defmethod ie-beginning-of-buffer ((sheet edward-mixin)
                                   (buffer cluffer:buffer)
                                   event
                                   &optional numeric-argument)
  (declare (ignore event numeric-argument))
  (let* ((cursor (edit-cursor sheet))
         (linum  (cluffer:line-number cursor)))
    (unless (= linum 0)
      (cluffer:detach-cursor cursor)
      (cluffer:attach-cursor cursor (cluffer:find-line buffer 0)))))

(defmethod ie-end-of-buffer ((sheet edward-mixin)
                             (buffer cluffer:buffer)
                             event
                             &optional numeric-argument)
  (declare (ignore event numeric-argument))
  (let* ((cursor (edit-cursor sheet))
         (linum  (cluffer:line-number cursor))
         (linum* (1- (cluffer:line-count buffer))))
    (unless (= linum linum*)
      (cluffer:detach-cursor cursor)
      (cluffer:attach-cursor cursor (cluffer:find-line buffer linum*)))
    (cluffer:end-of-line cursor)))

(defmethod ie-erase-object ((sheet edward-mixin)
                            (buffer cluffer:buffer)
                            event
                            &optional (numeric-argument 1))
  (declare (ignore event))
  (loop with cursor = (edit-cursor sheet)
        repeat numeric-argument
        do (smooth-erase-item cursor)))

(defmethod ie-erase-word ((sheet edward-mixin)
                          (buffer cluffer:buffer)
                          event
                          &optional (numeric-argument 1))
  (loop with cursor = (edit-cursor sheet)
        repeat numeric-argument
        do (loop do (ie-erase-object sheet buffer event)
                 until (or (cluffer:beginning-of-line-p cursor)
                           (cluffer:beginning-of-buffer-p cursor)
                           (char= (cluffer:item-before-cursor cursor) #\space)))))

(defmethod ie-delete-object
    ((sheet edward-mixin)
     (buffer cluffer:buffer)
     event
     &optional (numeric-argument 1))
  (declare (ignore event))
  (loop with cursor = (edit-cursor sheet)
        repeat numeric-argument
        do (smooth-delete-item cursor)))

(defmethod ie-delete-word ((sheet edward-mixin)
                          (buffer cluffer:buffer)
                          event
                          &optional (numeric-argument 1))
  (loop with cursor = (edit-cursor sheet)
        repeat numeric-argument
        do (loop do (ie-delete-object sheet buffer event)
                 until (or (cluffer:end-of-line-p cursor)
                           (cluffer:end-of-buffer-p cursor)
                           (char= (cluffer:item-after-cursor cursor) #\space)))))

(defmethod ie-kill-line
    ((sheet edward-mixin)
     (buffer cluffer:buffer)
     event
     &optional (numeric-argument 1))
  (declare (ignore event))
  (handler-bind ((cluffer:end-of-buffer
                   (lambda (condition)
                     (declare (ignore condition))
                     (return-from ie-kill-line))))
    (loop with cursor = (edit-cursor sheet)
          repeat numeric-argument
          if (cluffer:end-of-line-p cursor)
            do (cluffer:join-line cursor)
          else
            do (loop until (cluffer:end-of-line-p cursor)
                     do (cluffer:delete-item cursor)))))

(defmethod ie-clear-input-buffer
    ((sheet edward-mixin)
     (buffer cluffer:buffer)
     event
     &optional numeric-argument)
  (declare (ignore event numeric-argument))
  (reinitialize-instance sheet))

(defmethod ie-insert-newline
    ((sheet edward-mixin)
     (buffer cluffer:buffer)
     event
     &optional (numeric-argument 1))
  (declare (ignore event))
  (loop with cursor = (edit-cursor sheet)
        repeat numeric-argument
        do (cluffer:split-line cursor)))

(defmethod ie-insert-newline-after-cursor
    ((sheet edward-mixin)
     (buffer cluffer:buffer)
     event
     &optional (numeric-argument 1))
  (declare (ignore event))
  (loop with cursor = (edit-cursor sheet)
        with initial-line = (cluffer:line cursor)
        with initial-position = (cluffer:cursor-position cursor)
        repeat numeric-argument
        do (cluffer:split-line cursor)
        finally (cluffer:detach-cursor cursor)
                (cluffer:attach-cursor cursor initial-line initial-position)))

(defmethod ie-transpose-objects
    ((sheet edward-mixin)
     (buffer cluffer:buffer)
     event
     &optional (numeric-argument 1))
  (let ((cursor (edit-cursor sheet)))
    (when (cluffer:beginning-of-buffer-p cursor)
      (return-from ie-transpose-objects))
    (when (cluffer:end-of-line-p cursor)
      (ie-backward-object sheet buffer event))
    (let ((elt (if (cluffer:beginning-of-line-p cursor)
                   #\newline
                   (cluffer:item-before-cursor cursor))))
      (ie-erase-object sheet buffer event)
      (ie-forward-object sheet buffer event numeric-argument)
      (ie-insert-object sheet buffer elt))))

#+ (or)                                 ; requires more work
(defmethod ie-transpose-words
    ((sheet edward-mixin)
     (buffer cluffer:buffer)
     event
     &optional (numeric-argument 1))
  (let ((cursor (edit-cursor sheet))
        word-start
        word-end
        word-items)
    (ie-backward-word sheet buffer event 2)
    (ie-forward-word sheet buffer event)
    (setf word-start (cluffer:cursor-position cursor))
    (ie-forward-word sheet buffer event)
    (setf word-end (cluffer:cursor-position cursor))
    (setf word-items (cluffer:items cursor :start word-start :end word-end))
    (ie-backward-word sheet buffer event 2)
    (ie-forward-word sheet buffer event)
    (ie-delete-word sheet buffer event)
    (ie-forward-word sheet buffer event numeric-argument)
    (loop for item across word-items
          do (cluffer:insert-item cursor item))))

;; ie-yank-kill-ring
;; ie-yank-history
;; ie-yank-next-item

(defmethod ie-insert-object
    ((sheet edward-mixin)
     (buffer cluffer:buffer)
     event
     &optional (numeric-argument 1))
  (when-let ((ch (typecase event
                   (keyboard-event (event-char event))
                   (character event))))
    (if (char= ch #\newline)
        (ie-insert-newline sheet buffer event numeric-argument)
        (loop with cursor = (edit-cursor sheet)
              repeat numeric-argument
              do (cluffer:insert-item cursor ch)))))

(defmethod ie-select-object
    ((sheet edward-mixin)
     (buffer cluffer:buffer)
     event
     &optional numeric-argument)
  (declare (ignore numeric-argument))
  ;; XXX doesn't handle soft line wrapping
  (with-sheet-medium (medium sheet)
    (let* ((rel-x (- (pointer-event-x event) 4))
           (rel-y (- (pointer-event-y event) 2))
           (lin-h (text-style-height (medium-text-style medium) medium))
           (linum (truncate rel-y lin-h))
           (licnt (cluffer:line-count buffer))
           (cline (cluffer:find-line buffer (clamp linum 0 (1- (cluffer:line-count buffer)))))
           (cursor (edit-cursor sheet)))
      (unless (eq (cluffer:line cursor) cline)
        (cluffer:detach-cursor cursor)
        (cluffer:attach-cursor cursor cline))
      (when (>= linum licnt)
        (cluffer:end-of-line cursor)
        (return-from ie-select-object))
      (when-let ((line-breaks (line-breaks
                               (coerce (cluffer:items cline) 'string)
                               (lambda (string start end)
                                 (text-size sheet string :start start :end end))
                               :break-strategy nil
                               ;; If the initial offset is 0, then the utility
                               ;; line-breaks breaks the line at least after
                               ;; the first character. To avoid that we add 1
                               ;; to both initial-ofset and margin and in
                               ;; consequence "break" may occur at index 0).
                               :initial-offset 1
                               :margin (+ rel-x 1)
                               :count 1)))
        (setf (cluffer:cursor-position (edit-cursor sheet))
              (first line-breaks))))))

(defmethod ie-scroll-forward ((sheet edward-mixin)
                              (buffer cluffer:buffer)
                              event
                              &optional (numeric-arg 4))
  (ie-next-line sheet buffer event numeric-arg))

(defmethod ie-scroll-backward ((sheet edward-mixin)
                               (buffer cluffer:buffer)
                               event
                               &optional (numeric-arg 4))
  (ie-previous-line sheet buffer event numeric-arg))

