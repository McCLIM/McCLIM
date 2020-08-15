;;; -*- Mode: Lisp; Package: CLIM-SDL; -*-

;;;  (c) copyright 2005 by Christophe Rhodes (c.rhodes@gold.ac.uk)
;;;  (c) copyright 2014 by Robert Strandh (robert.strandh@gmail.com)

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

(in-package :clim-sdl)

(defclass sdl-pointer (standard-pointer)
  ((cursor :accessor pointer-cursor :initform :upper-left)
   (x :initform 0)
   (y :initform 0)))

(defclass sdl-port (basic-port)
  ((id)
   (pointer            :accessor port-pointer :initform (make-instance 'sdl-pointer))
   (window             :initform nil :accessor sdl-port-window)
   (sheet-to-window-id :initform (make-hash-table :test 'eql)
                       :reader sdl-port/sheet-to-window-id)
   (window-id-to-sheet :initform (make-hash-table :test 'eql)
                       :reader sdl-port/window-id-to-sheet)))

(defun parse-sdl-server-path (path)
  path)

;;; FIXME: if :port-type and :server-path-parser aren't CLIM-specified
;;; keywords, they should be altered to be in some mcclim-internal
;;; package instead.
(setf (get :sdl :port-type) 'sdl-port)
(setf (get :sdl :server-path-parser) 'parse-sdl-server-path)

(defmethod initialize-instance :after ((port sdl-port) &rest initargs)
  (declare (ignore initargs))
  (setf (slot-value port 'id) (gensym "SDL-PORT-"))
  ;; FIXME: it seems bizarre for this to be necessary
  (push (make-instance 'sdl-frame-manager :port port)
	(slot-value port 'climi::frame-managers))
  (init-sdl port))

(defmethod print-object ((object sdl-port) stream)
  (print-unreadable-object (object stream :identity t :type t)
    (format stream "~S ~S" :id (slot-value object 'id))))

(defclass sdl-renderer-sheet (mirrored-sheet-mixin)
  ((renderer      :initform nil
                  :accessor sdl-renderer-sheet/renderer)
   (texture       :initform nil
                  :accessor sdl-renderer-sheet/texture)
   (surface       :initform nil
                  :accessor sdl-renderer-sheet/surface)
   (cairo-context :initform nil
                  :accessor sdl-renderer-sheet/cairo-context)))

(defclass sdl-top-level-sheet-pane (sdl-renderer-sheet climi::top-level-sheet-pane)
  ())

(defmethod port-set-mirror-region ((port sdl-port) mirror mirror-region)
  (multiple-value-bind (old-width old-height)
      (sdl2:in-main-thread ()
        (sdl2:get-window-size mirror))
    (with-bounding-rectangle* (x1 y1 x2 y2) mirror-region
      (declare (ignore x1 y1))
      (let ((new-width (round-coordinate x2))
            (new-height (round-coordinate y2)))
        (unless (and (= old-width new-width)
                     (= old-height new-height))
          (sdl2:in-main-thread ()
            (sdl2:set-window-size mirror new-width new-height)))))))
                                   
(defmethod port-set-mirror-transformation ((port sdl-port) mirror mirror-transformation)
  nil)

(defmethod climi::port-lookup-mirror ((port sdl-port) (sheet sdl-renderer-sheet))
  (alexandria:if-let ((window-id (gethash sheet (sdl-port/sheet-to-window-id port))))
    (sdl2-ffi.functions:sdl-get-window-from-id window-id)
    nil))

(defmethod climi::port-lookup-sheet ((port sdl-port) mirror)
  (gethash (sdl2:get-window-id mirror) (sdl-port/window-id-to-sheet port)))

(defmethod climi::port-register-mirror ((port sdl-port) (sheet sdl-renderer-sheet) mirror)
  (let ((window-id (sdl2:get-window-id mirror)))
    (setf (gethash window-id (sdl-port/window-id-to-sheet port)) sheet)
    (setf (gethash sheet (sdl-port/sheet-to-window-id port)) window-id)))

(defmethod climi::port-unregister-mirror ((port sdl-port) (sheet sdl-renderer-sheet) mirror)
  (remhash (sdl2:get-window-id mirror) (sdl-port/window-id-to-sheet port))
  (remhash sheet (sdl-port/sheet-to-window-id port)))

(defmethod realize-mirror ((port sdl-port) (sheet mirrored-sheet-mixin))
  (let* ((q (compose-space sheet))
         (mirror-region (climi::%sheet-mirror-region sheet))
         (win (sdl2:in-main-thread ()
                (log:info "Creating window (~s,~s) mr:~s"
                          (climi::space-requirement-width q) (climi::space-requirement-height q)
                          mirror-region)
                (sdl2:create-window :title (clime:sheet-pretty-name sheet)
                                    :w (round-coordinate (if mirror-region
                                                             (bounding-rectangle-width mirror-region)
                                                             (climi::space-requirement-width q)))
                                    :h (round-coordinate (if mirror-region
                                                             (bounding-rectangle-height mirror-region)
                                                             (climi::space-requirement-height q)))
                                    :flags (append
                                            '(:resizable)
                                            (if (sheet-enabled-p sheet) '((:shown)) nil))))))
    (log:info "Registered win: ~s" win)
    (climi::port-register-mirror port sheet win)))

(defmethod destroy-mirror ((port sdl-port) (sheet mirrored-sheet-mixin))
  (sdl2:in-main-thread ()
    (when (sdl-renderer-sheet/renderer sheet)
      (sdl2:destroy-texture (sdl-renderer-sheet/texture sheet))
      (sdl2:destroy-renderer (sdl-renderer-sheet/renderer sheet)))
    (let ((win (sheet-mirror sheet)))
      (sdl2:destroy-window win))))

(defmethod mirror-transformation ((port sdl-port) mirror)
  nil)

(defmethod port-enable-sheet ((port sdl-port) (mirror mirrored-sheet-mixin))
  nil)

(defmethod port-disable-sheet ((port sdl-port) (mirror mirrored-sheet-mixin))
  nil)

(defmethod destroy-port :before ((port sdl-port))
  (quit-sdl port))

(defvar *event-ts* 0)

(defun process-sdl-event (port event)
  (case (sdl2:get-event-type event)
    (:windowevent
     (let* ((window-id (plus-c:c-ref event sdl2-ffi:sdl-event :window :window-id))
            (win (sdl2-ffi.functions:sdl-get-window-from-id window-id)))
       (unless win
         (error "Event from unknown window: ~s" window-id))
       (let ((sheet (climi::port-lookup-sheet port win)))
         (case (plus-c:c-ref event sdl2-ffi:sdl-event :window :event)
           (#.sdl2-ffi:+sdl-windowevent-exposed+
            (log:trace "Expose")
            (list (make-instance 'window-repaint-event
                                 :timestamp (incf *event-ts*)
                                 :sheet sheet
                                 :region clim:+everywhere+)))
           (#.sdl2-ffi:+sdl-windowevent-resized+
            (invalidate-context sheet)
            (log:trace "Resized: new size: ~s×~s"
                       (plus-c:c-ref event sdl2-ffi:sdl-event :window :data1)
                       (plus-c:c-ref event sdl2-ffi:sdl-event :window :data2))
            (list (make-instance 'window-configuration-event
                                 :timestamp (incf *event-ts*)
                                 :sheet sheet
                                 :x 0
                                 :y 0
                                 :width (plus-c:c-ref event sdl2-ffi:sdl-event :window :data1)
                                 :height (plus-c:c-ref event sdl2-ffi:sdl-event :window :data1))
                  (make-instance 'window-repaint-event
                                 :timestamp (incf *event-ts*)
                                 :sheet sheet
                                 :region clim:+everywhere+)))))))
    (:mousemotion
     (let* ((x (plus-c:c-ref event sdl2-ffi:sdl-event :motion :x))
            (y (plus-c:c-ref event sdl2-ffi:sdl-event :motion :y))
            (window-id (plus-c:c-ref event sdl2-ffi:sdl-event :motion :window-id))
            (win (sdl2-ffi.functions:sdl-get-window-from-id window-id))
            (sheet (climi::port-lookup-sheet port win)))
       (multiple-value-bind (window-x window-y)
           (sdl2:get-window-position win)
         (let ((root-x (+ window-x x))
               (root-y (+ window-y y)))
           (log:trace "Mouse motion: (~s,~s) abs:(~s,~s) sheet:~s" x y root-x root-y sheet)
           (list (make-instance 'pointer-motion-event
                                :timestamp (incf *event-ts*)
                                :sheet sheet
                                :pointer 0
                                :button 0
                                :x x
                                :y y
                                :graft-x root-x
                                :graft-y root-y
                                :modifier-state 0))))))))

(defmethod process-next-event ((port sdl-port) &key wait-function (timeout nil))
  #+nil
  (cond ((maybe-funcall wait-function)
         (values nil :wait-function))
        ((not (null timeout))
         (sleep timeout)
         (if (maybe-funcall wait-function)
             (values nil :wait-function)
             (values nil :timeout)))
        ((not (null wait-function))
         (loop do (sleep 0.1)
               until (funcall wait-function)
               finally (return (values nil :wait-function))))
        (t
         (error "Game over. Listening for an event on Sdl backend.")))

  #+nil
  (sdl2:with-event-loop (:method :wait :timeout (if timeout (truncate (* timeout 1000)) nil))
    (:windowevent
     (:event window-event-type)
     (case window-event-type
       (#.sdl2-ffi:+sdl-windowevent-resized+ (log:info "resized"))
       (#.sdl2-ffi:+sdl-windowevent-exposed+ (log:info "expose")))))

  (sdl2:in-main-thread ()
    (sdl2:with-sdl-event (event)
      (let ((result (sdl2:next-event event :wait (if timeout (truncate (* timeout 1000)) nil))))
        (labels ((call-wait-function ()
                   (if (maybe-funcall wait-function)
                       (values nil :wait-funtion)
                       (values nil :timeout))))
          (if (zerop result)
              (call-wait-function)
              (let ((processed (process-sdl-event port event)))
                (if processed
                    (prog1 t
                      (mapc (lambda (event) (distribute-event port event)) processed))
                    (call-wait-function)))))))))

(defmethod make-graft ((port sdl-port) &key (orientation :default) (units :device))
  (make-instance 'sdl-graft
                 :port port :mirror (gensym)
                 :orientation orientation :units units))

(defmethod make-medium ((port sdl-port) sheet)
  (log:trace "Making medium for sheet=~s" sheet)
  (make-instance 'sdl-medium :sheet sheet))

(defmethod text-style-mapping ((port sdl-port) (text-style text-style) &optional character-set)
  (declare (ignore port text-style character-set))
  nil)

(defmethod (setf text-style-mapping) (font-name
                                      (port sdl-port)
                                      (text-style text-style)
                                      &optional character-set)
  (declare (ignore font-name text-style character-set))
  nil)

(defmethod graft ((port sdl-port))
  (first (climi::port-grafts port)))

(defmethod port-allocate-pixmap ((port sdl-port) sheet width height)
  (declare (ignore sheet width height))
  ;; FIXME: this isn't actually good enough; it leads to errors in
  ;; WITH-OUTPUT-TO-PIXMAP
  nil)

(defmethod port-deallocate-pixmap ((port sdl-port) pixmap)
  #+nil
  (when (port-lookup-mirror port pixmap)
    (destroy-mirror port pixmap)))

(defmethod pointer-position ((pointer sdl-pointer))
  (values (slot-value pointer 'x) (slot-value pointer 'y)))

(defmethod pointer-button-state ((pointer sdl-pointer))
  nil)

(defmethod port-modifier-state ((port sdl-port))
  nil)

(defmethod synthesize-pointer-motion-event ((pointer sdl-pointer))
  nil)

(defmethod port-frame-keyboard-input-focus ((port sdl-port) frame)
  (frame-properties frame 'focus))

(defmethod (setf port-frame-keyboard-input-focus) 
    (focus (port sdl-port) frame)
  (setf (frame-properties frame 'focus) focus))

(defmethod (setf port-keyboard-input-focus) (focus (port sdl-port))
  focus)

(defmethod port-keyboard-input-focus ((port sdl-port))
  nil)

(defmethod port-force-output ((port sdl-port))
  nil)

#+nil
(defmethod distribute-event :around ((port sdl-port) event)
  (declare (ignore event))
  nil)

(defmethod set-sheet-pointer-cursor ((port sdl-port) sheet cursor)
  (declare (ignore sheet cursor))
  nil)        
