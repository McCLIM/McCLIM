(in-package #:clim-internals)

;;;
;;; TEXT-EDITING-GADGET implements all behavior (and initargs) expected from
;;; TEXT-FIELD-PANE and TEXT-EDITOR-PANE. Both classes differ by default
;;; initarg values (i.e TEXT-FIELD-PANE does not allow line breaks).
;;;
;;; TODO ("nice to have" extensions)
;;;
;;;   (end-of-line-action :initarg :end-of-line-action)
;;;   (end-of-page-action :initarg :end-of-page-action)
;;;

(defclass text-editing-gadget (edward-mixin text-editor)
  ((allow-line-breaks
    :reader allow-line-breaks
    :initarg :allow-line-breaks) ; single line?
   (activation-gestures :accessor activation-gestures
                        :initarg :activation-gestures))
  (:default-initargs :foreground +black+
                     :background +white+
                     :allow-line-breaks t
                     :activation-gestures '()))

(defmethod ie-insert-newline :around
    ((sheet text-editing-gadget) buffer event numarg)
  (if (allow-line-breaks sheet)
      (call-next-method)
      (beep sheet)))

(defmethod ie-insert-newline-after-cursor :around
    ((sheet text-editing-gadget) buffer event numarg)
  (if (allow-line-breaks sheet)
      (call-next-method)
      (beep sheet)))

(defmethod ie-yank-kill-ring
    ((sheet edward-mixin) (buffer cluffer:buffer) event numarg)
  (let ((cursor (edit-cursor sheet)))
    (ensure-edward-selection sheet :yank :start cursor :end cursor)
    (if (allow-line-breaks sheet)
        (smooth-insert-input cursor (input-editor-yank-kill sheet))
        (smooth-insert-line  cursor (input-editor-yank-kill sheet)))))

(defmethod ie-yank-next-item
    ((sheet edward-mixin) (buffer cluffer:buffer) event numarg)
  (when-let ((items (input-editor-yank-next sheet)))
    (if (allow-line-breaks sheet)
        (smooth-replace-input (ensure-edward-selection sheet :yank) items)
        (smooth-replace-line  (ensure-edward-selection sheet :yank) items))))

(defun fix-cursors (gadget)
  (with-sheet-medium (medium gadget)
    (let* ((ts (medium-text-style medium))
           (ht (text-style-height ts medium))
           (editable (editable-p gadget)))
      (do-cursors (cursor gadget)
        (when (typep cursor 'standard-text-cursor)
          (with-slots (sheet width height) cursor
            (setf sheet gadget width 2 height ht)
            (setf (cursor-visibility cursor) editable)))))))

(defmethod initialize-instance :after ((gadget text-editing-gadget) &key value)
  (setf (gadget-value gadget :invoke-callback t) value)
  (fix-cursors gadget))

(defmethod reinitialize-instance :after ((gadget text-editing-gadget) &key)
  (fix-cursors gadget))

(defmethod (setf editable-p) :after (new-value (object text-editing-gadget))
  (setf (cursor-visibility (edit-cursor object)) new-value))

(defmethod gadget-value ((sheet text-editing-gadget))
  (edward-buffer-string sheet))

(defmethod (setf gadget-value) (new-value (sheet text-editing-gadget) &rest args)
  (declare (ignore args))
  (ie-clear-input-buffer sheet (input-editor-buffer sheet) nil 1)
  (loop for ch across new-value do
    (handle-editor-event sheet ch))
  (change-space-requirements sheet)
  (dispatch-repaint sheet +everywhere+))

(defmethod handle-event :around ((sheet text-editing-gadget) event)
  (with-activation-gestures ((activation-gestures sheet) :override t)
    (if (activation-gesture-p event)
        (activate-callback sheet (gadget-client sheet) (gadget-id sheet))
        (call-next-method))))

(defmethod handle-event ((sheet text-editing-gadget) (event key-press-event))
  (if (handle-editor-event sheet event)
      (progn
        (change-space-requirements sheet)
        (dispatch-repaint sheet +everywhere+)
        (when (editable-p sheet)
          (value-changed-callback sheet
                                  (gadget-client sheet)
                                  (gadget-id sheet)
                                  (gadget-value sheet))))
      (call-next-method)))

(defmethod handle-event ((sheet text-editing-gadget) (event pointer-event))
  (if (handle-editor-event sheet event)
      (dispatch-repaint sheet +everywhere+)
      (call-next-method)))

(defmethod handle-event ((sheet text-editing-gadget)
                         (event window-manager-focus-event))
  (when (editable-p sheet)
    (stream-set-input-focus sheet)))

(defmethod note-input-focus-changed ((sheet text-editing-gadget) state)
  (dispatch-repaint sheet +everywhere+))

(defmethod handle-repaint ((sheet text-editing-gadget) region)
  (declare (ignore region))
  (with-sheet-medium (medium sheet)
    (let* ((edit-cursor (edit-cursor sheet))
           (text-style (medium-text-style medium))
           (line-height (text-style-height text-style medium))
           (current-y 0))
      (labels ((fix-cursor (line text cursor)
                 ;; Update the cursor.
                 (when (and (eq line (cluffer:line cursor))
                            (cursor-active cursor))
                   (let ((pos (cluffer:cursor-position cursor)))
                     (setf (cursor-position cursor)
                           (values (nth-value 2 (text-size medium text :end pos))
                                   current-y))
                     pos)))
               (draw-line (line)
                 (let ((text (line-string line)))
                   ;; Update cursors.
                   (fix-cursor line text edit-cursor)
                   ;; Draw the line.
                   (draw-text* sheet text 0 current-y :align-x :left :align-y :top)
                   ;; Increment the drawing position.
                   (incf current-y line-height))))
        (declare (dynamic-extent (function draw-line)))
        (map-over-lines #'draw-line (input-editor-buffer sheet))
        (draw-design sheet edit-cursor)
        (scroll-extent* sheet edit-cursor)))))

(defmethod compose-space ((sheet text-editing-gadget) &key width height)
  (declare (ignore width height))
  (multiple-value-bind (buffer-w buffer-h)
      (edward-buffer-extent sheet)
    (let ((text-cursor (edit-cursor sheet)))
      (when (cursor-state text-cursor)
        (incf buffer-w (cursor-width text-cursor))))
    (with-sheet-medium (medium sheet)
      (let* ((text-style (medium-text-style medium))
             (ts-w (text-style-width text-style medium))
             (ts-h (text-style-height text-style medium))
             (min-w (* ts-w (text-editor-ncolumns sheet)))
	     (min-h (* ts-h (text-editor-nlines sheet)))
             (result-w (max min-w buffer-w))
             (result-h (max min-h buffer-h)))
        (make-space-requirement
         :width  result-w :min-width  min-w :max-width  result-w
         :height result-h :min-height min-h :max-height result-h)))))

(defclass text-field-pane (text-editing-gadget)
  ()
  (:default-initargs :nlines 1
                     :ncolumns 20
                     ;; :end-of-line-action :scroll
                     :allow-line-breaks nil
                     :activation-gestures *standard-activation-gestures*))

(defun wrap-text-field-pane (pane)
  (let ((space (compose-space pane)))
    (wrap-clim-pane pane nil :scroll-bars
                    `(nil :width ,(space-requirement-min-width space)
                          :height ,(space-requirement-min-height space)))))

(defmethod make-pane-1 :around
    (realizer frame (type (eql :text-field)) &rest initargs)
  (declare (ignore initargs))
  (let ((pane (call-next-method)))
    (if (typep pane 'text-field-pane)
        (wrap-text-field-pane pane)
        pane)))

(defmethod reinitialize-pane ((pane text-field-pane) &rest initargs)
  (apply #'reinitialize-instance pane initargs)
  (wrap-text-field-pane pane))

(defclass text-editor-pane (text-editing-gadget)
  ()
  (:default-initargs :nlines 6
                     :ncolumns 20
                     ;; :end-of-line-action :wrap*
   ))

(defun wrap-text-editor-pane (pane)
  (let ((space (compose-space pane)))
    (wrap-clim-pane pane nil :scroll-bars
                    `(nil :width ,(space-requirement-min-width space)
                          :height ,(space-requirement-min-height space)))))

(defmethod make-pane-1 :around
    (realizer frame (class (eql :text-editor)) &rest initargs)
  (declare (ignore initargs))
  (let ((pane (call-next-method)))
    (if (typep pane 'text-editor-pane)
        (wrap-text-editor-pane pane)
        pane)))

(defmethod reinitialize-pane ((pane text-editor-pane) &rest initargs)
  (apply #'reinitialize-instance pane initargs)
  (wrap-text-editor-pane pane))
