(in-package #:clim-internals)

;;; TEXT-EDITING-GADGET implements all behavior (and initargs) expected from
;;; TEXT-FIELD-PANE and TEXT-EDITOR-PANE. Both classes differ by default
;;; initarg values (i.e TEXT-FIELD-PANE does not allow line breaks).
(defclass text-editing-gadget (edward-mixin)
  ((activation-gestures :accessor activation-gestures
                        :initarg :activation-gestures)
   (end-of-line-action :initarg :end-of-line-action)
   (end-of-page-action :initarg :end-of-page-action)
   (allow-line-breaks :initarg :allow-line-breaks)
   (nlines :initarg :nlines)
   (ncolumns :initarg :ncolumns)
   (resizable :initarg :resizable))
  (:default-initargs :foreground +black+
                     :background +white+))

(defmethod gadget-value ((sheet text-editing-gadget))
  ;; XXX retain the old string and update it with a fill pointer?
  ;; XXX maintain a "dirty" status to return the "old" value on no change?
  (loop with buffer = (input-editor-buffer sheet)
        with nitems = (cluffer:item-count buffer)
        with nlines = (cluffer:line-count buffer)
        with width = (+ nitems nlines -1)
        with value = (make-string width)
        with index = 0
        for line-no from 0 below nlines
        for line = (cluffer:find-line buffer line-no)
        do (loop for ch across (cluffer:items line)
                 do (setf (aref value index) ch)
                    (incf index))
        unless (= index width)
          do (setf (aref value index) #\newline)
             (incf index)
        finally (return value)))

(defmethod (setf gadget-value) (new-value (sheet text-editing-gadget) &rest args)
  (declare (ignore args))
  (ie-clear-input-buffer sheet (input-editor-buffer sheet) nil 1)
  (loop for ch across new-value do
    (handle-editor-event sheet ch))
  (dispatch-repaint sheet +everywhere+))

(defmethod handle-event :around ((sheet text-editing-gadget) event)
  (with-activation-gestures ((activation-gestures sheet) :override t)
    (if (activation-gesture-p event)
        (activate-callback sheet (gadget-client sheet) (gadget-id sheet))
        (call-next-method))))

(defmethod handle-event ((sheet text-editing-gadget) (event key-press-event))
  (if (handle-editor-event sheet event)
      ;; XXX Update size after adding output? (probably not)
      ;; XXX Incremental redisplay? (would require output recording)
      (dispatch-repaint sheet +everywhere+)
      (call-next-method)))

(defmethod handle-event ((sheet text-editing-gadget) (event pointer-event))
  (if (handle-editor-event sheet event)
      ;; XXX Update size after adding output? (probably not)
      ;; XXX Incremental redisplay? (would require output recording)
      (dispatch-repaint sheet +everywhere+)
      (call-next-method)))

(defmethod handle-event ((sheet text-editing-gadget)
                         (event window-manager-focus-event))
  (stream-set-input-focus sheet))

(defmethod note-input-focus-changed ((sheet text-editing-gadget) state)
  (dispatch-repaint sheet +everywhere+))

(defmethod handle-repaint ((sheet text-editing-gadget) region)
  (with-sheet-medium (medium sheet)
    (loop with buffer = (input-editor-buffer sheet)
          with cursor = (edward-cursor sheet)
          with cursor-line = (cluffer:line cursor)
          with cursor-position = (cluffer:cursor-position cursor)
          with tstyle = (medium-text-style medium)
          with height = (text-style-height tstyle medium)
          with sfocus = (port-keyboard-input-focus (port sheet))
          for lineno from 0 below (cluffer:line-count buffer)
          for line = (cluffer:find-line buffer lineno)
          for text = (coerce (cluffer:items line) 'string)
          for cury from 0 by height
          do (draw-text* sheet text 0 cury :align-x :left :align-y :top)
          when (eq line cursor-line)
            do (let ((cursor-dx (nth-value 2 (text-size medium text :end cursor-position))))
                 (draw-rectangle* sheet cursor-dx cury (+ cursor-dx 2) (+ cury height)
                                  :ink (if (eq sheet sfocus) +black+ +dark-grey+))))))


(defclass text-field-pane (text-editing-gadget text-field)
  ()
  (:default-initargs :nlines 1
                     :ncolumns 20
                     :end-of-line-action :scroll
                     :allow-line-breaks nil
                     :activation-gestures *standard-activation-gestures*))

(defmethod compose-space ((gadget text-field-pane) &key width height)
  (declare (ignore width height))
  (with-sheet-medium (medium gadget)
    (let ((width (text-size medium (gadget-value gadget)))
	  (height (text-style-height (medium-text-style medium) medium)))
      (make-space-requirement :height height
                              :min-height height
                              :max-height height
			      :width width
                              :min-width width))))

(defclass text-editor-pane (text-editing-gadget text-editor)
  ()
  (:default-initargs :nlines 6
                     :ncolumns 20
                     :end-of-line-action :wrap*
                     :allow-line-breaks t
                     :activation-gestures nil))

(defmethod compose-space ((gadget text-editor-pane) &key width height)
  (declare (ignore width height))
  (with-sheet-medium (medium gadget)
    (let ((width (* (slot-value gadget 'ncolumns)
                    (text-style-width (medium-text-style medium) medium)))
	  (height (* (slot-value gadget 'nlines)
                     (text-style-height (medium-text-style medium) medium))))
      (make-space-requirement :height height
                              :min-height height
			      :width width
                              :min-width width))))
