;;; -*- Mode: Lisp; Package: CLIM-GRAPHIC-FORMS; -*-

;;; (c) 2006 Jack D. Unrue (jdunrue (at) gmail (dot) com)
;;; based on the null backend by:
;;;  (c) 2005 Christophe Rhodes (c.rhodes@gold.ac.uk)

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

(in-package :clim-graphic-forms)

(defclass gf-mirror-mixin ()
  ((sheet
    :accessor sheet
    :initarg :sheet
    :initform nil)))

(defclass gfw-top-level (gfw:top-level gf-mirror-mixin) ())
(defclass gfw-panel (gfw:panel gf-mirror-mixin) ())
(defclass gfw-menu (gfw:menu gf-mirror-mixin) ())
(defclass gfw-menu-item (gfw:menu-item gf-mirror-mixin) ())
(defclass gfw-button (gfw:button gf-mirror-mixin) ())
(defclass gfw-scroll-bar (gfw:scrollbar gf-mirror-mixin) ())

(defclass gfw-widget-pane-mixin () ())

(defclass gfw-menu-pane-mixin (gfw-widget-pane-mixin)
  ((label
    :accessor label
    :initarg :label
    :initform "Mu!")
   (contents
    :accessor contents
    :initarg :contents
    :initform nil)
   (command-table
    :accessor command-table
    :initarg :command-table
    :initform nil)))

(defclass gfw-menu-bar-pane (basic-pane sheet-multiple-child-mixin gfw-menu-pane-mixin) ())

(defclass gfw-menu-pane (basic-pane sheet-parent-mixin sheet-multiple-child-mixin gfw-menu-pane-mixin) ())

(defclass gfw-menu-item-pane (climi::menu-button-pane sheet-parent-mixin gfw-widget-pane-mixin)
  ((item
    :accessor item
    :initarg :item
    :initform nil)
   (callback :initarg :value-changed-callback :accessor callback)
   (command
    :accessor command
    :initarg :command
    :initform nil)))

(defmethod print-object ((object gfw-menu-item-pane) stream)
  (print-unreadable-object (object stream :identity t :type t)
    (format stream
            "~S ~S ~S ~S"
            :item (item object)
            :command (command object))))

(defclass sheet-event-dispatcher (gfw:event-dispatcher)
  ((port
    :accessor port
    :initform nil)))

(defclass pane-event-dispatcher (gfw:event-dispatcher)
  ((port
    :accessor port
    :initform nil)))

(defclass menu-clicked-event (window-event)
  ((item
    :accessor event-item
    :initarg :item
    :initform nil)))

(defmethod print-object ((object menu-clicked-event) stream)
  (print-unreadable-object (object stream :identity t :type t)
    (format stream
            "~S ~S ~S ~S ~S ~S"
            :timestamp (climi::event-timestamp object)
            :sheet (event-sheet object)
            :item (event-item object))))

(defclass gfw-pointer (standard-pointer)
  ((cursor :accessor pointer-cursor :initform :upper-left)
   (x :initform 0)
   (y :initform 0)))

(defclass graphic-forms-port (basic-port)
  ((id)
   (events
    :accessor events
    :initform nil)
   (pointer
    :accessor port-pointer
    :initform (make-instance 'gfw-pointer))))

(defun enqueue (port event)
  (setf (slot-value event 'climi::timestamp) (gfw:obtain-event-time))
  (push event (events port)))

(defvar *sheet-dispatcher* (make-instance 'sheet-event-dispatcher))

(defvar *pane-dispatcher* (make-instance 'pane-event-dispatcher))

(defun parse-graphic-forms-server-path (path)
  path)

;;; FIXME: if :port-type and :server-path-parser aren't CLIM-specified
;;; keywords, they should be altered to be in some mcclim-internal
;;; package instead.
(setf (get :graphic-forms :port-type) 'graphic-forms-port)
(setf (get :graphic-forms :server-path-parser) 'parse-graphic-forms-server-path)

(defun resolve-abstract-pane-name (type)
  (when (get type 'climi::concrete-pane-class-name)
    (setf type (get type 'climi::concrete-pane-class-name)))
  (class-name
   (or (find-class
	(intern (concatenate 'string (symbol-name type) "-PANE") :climi)
	nil)
       (if (keywordp type)
	   (find-class (intern (symbol-name type) :climi))
	   (find-class type)))))

(defgeneric make-pane-2 (type &rest initargs)
  (:documentation "Implement this to instantiate specific pane types.")
  (:method (type &rest initargs)
    (apply #'make-instance (resolve-abstract-pane-name type) initargs)))

;;;
;;; helper functions
;;;


;;;
;;; port methods
;;;

(defmethod initialize-instance :after ((port graphic-forms-port) &rest initargs)
  (declare (ignore initargs))
  (setf (slot-value port 'id) (gensym "GRAPHIC-FORMS-PORT-")
        (port *sheet-dispatcher*) port
        (port *pane-dispatcher*) port)
  ;; FIXME: gtkairo backend comments that it seems bizarre for this to be necessary
  (push (make-instance 'graphic-forms-frame-manager :port port)
        (slot-value port 'climi::frame-managers)))

(defmethod print-object ((object graphic-forms-port) stream)
  (print-unreadable-object (object stream :identity t :type t)
    (format stream "~S ~S" :id (slot-value object 'id))))

;;;
;;; mirror methods
;;;

(defmethod port-set-mirror-region ((port graphic-forms-port) (mirror gfw-top-level) region)
  (let ((size (gfs:make-size :width (floor (bounding-rectangle-width region))
                             :height (floor (bounding-rectangle-height region)))))
    (setf (gfw:size mirror) (gfw::compute-outer-size mirror size))))

(defmethod port-set-mirror-region ((port graphic-forms-port) (mirror gf-mirror-mixin) region)
  (setf (gfw:size mirror)
        (gfs:make-size :width (floor (bounding-rectangle-width region))
                       :height (floor (bounding-rectangle-height region)))))

(defmethod port-set-mirror-region ((port graphic-forms-port) (mirror gfw-menu) region)
  (declare (ignore port mirror region)))

(defmethod port-set-mirror-region ((port graphic-forms-port) (mirror gfw-menu-item) region)
  (declare (ignore port mirror region)))

(defmethod port-set-mirror-transformation ((port graphic-forms-port) (mirror gfw-top-level) transformation)
  ;; FIXME: does McCLIM really need to set position of top-level window's?
  ())

(defmethod port-set-mirror-transformation ((port graphic-forms-port) (mirror gf-mirror-mixin) transformation)
  (multiple-value-bind (x y)
      (transform-position transformation 0 0)
    (setf (gfw:location mirror)
          (gfs:make-point :x (floor x)
                          :y (floor y)))))

(defmethod port-set-mirror-transformation ((port graphic-forms-port) (mirror gfw-menu) transformation)
  (declare (ignore port mirror transformation)))

(defmethod port-set-mirror-transformation ((port graphic-forms-port) (mirror gfw-menu-item) transformation)
  (declare (ignore port mirror transformation)))

;;;
;;; sheet methods
;;;

(defmethod realize-mirror ((port graphic-forms-port) (sheet climi::top-level-sheet-pane))
  #+nil (gfs::debug-format "realizing ~a~%" (class-of sheet))
  (let* ((mirror (make-instance 'gfw-top-level
                                :sheet sheet
                                :dispatcher *sheet-dispatcher*
                                :style '(:workspace)
                                :text (frame-pretty-name (pane-frame sheet)))))
    (let ((menu-bar (make-instance 'gfw-menu :handle (gfs::create-menu))))
      (gfw::put-widget (gfw::thread-context) menu-bar)
      (setf (gfw:menu-bar mirror) menu-bar))
    (climi::port-register-mirror (port sheet) sheet mirror)
    mirror))

(defmethod destroy-mirror ((port graphic-forms-port) (sheet climi::top-level-sheet-pane))
  (let ((mirror (climi::port-lookup-mirror port sheet)))
    (climi::port-unregister-mirror port sheet mirror)
    (gfs:dispose mirror)))

(defmethod realize-mirror ((port graphic-forms-port) (sheet mirrored-sheet-mixin))
  (let* ((parent (sheet-mirror (sheet-parent sheet)))
         (mirror (make-instance 'gfw-panel
                                :sheet sheet
                                :dispatcher *sheet-dispatcher*
                                :style '() ;was: '(:border)
                                :parent parent)))
    (climi::port-register-mirror (port sheet) sheet mirror)
    mirror))

(defmethod destroy-mirror ((port graphic-forms-port) (sheet mirrored-sheet-mixin))
  (let ((mirror (climi::port-lookup-mirror port sheet)))
#|
    (let ((medium (climi::sheet-medium sheet)))
      (destroy-medium medium)
      (setf (climi::%sheet-medium sheet) nil))
|#
    (climi::port-unregister-mirror port sheet mirror)
    (gfs:dispose mirror)))

(defmethod port-enable-sheet ((port graphic-forms-port) (sheet mirrored-sheet-mixin))
  (gfw:show (climi::port-lookup-mirror port sheet) t))

(defmethod port-disable-sheet ((port graphic-forms-port) (sheet mirrored-sheet-mixin))
  (gfw:show (climi::port-lookup-mirror port sheet) nil))

(defmethod destroy-port :before ((port graphic-forms-port))
  ())

(defmethod port-motion-hints ((port graphic-forms-port) (sheet mirrored-sheet-mixin))
  ())

(defmethod (setf port-motion-hints)
    (value (port graphic-forms-port) (sheet mirrored-sheet-mixin))
  value)

(defmethod get-next-event ((port graphic-forms-port) &key wait-function (timeout nil))
  (declare (ignore wait-function timeout)) ; FIXME
  (or (pop (events port))
      (cffi:with-foreign-object (msg-ptr 'gfs::msg)
        (let ((gm (gfs::get-message msg-ptr (cffi:null-pointer) 0 0)))
          (gfw::default-message-filter gm msg-ptr))
        (setf (events port) (nreverse (events port)))
        (pop (events port)))))

(defmethod process-next-event :after ((port graphic-forms-port) &key wait-function (timeout nil))
  (declare (ignore wait-function timeout))
  (render-pending-mediums))

(defmethod make-graft ((port graphic-forms-port) &key (orientation :default) (units :device))
  (make-instance 'graphic-forms-graft
                 :port port :mirror (gensym)
                 :orientation orientation :units units))

(defmethod make-medium ((port graphic-forms-port) sheet)
  #+nil (gfs::debug-format "creating medium for ~a~%" (class-of sheet))
  (make-instance 'graphic-forms-medium :port port :sheet sheet))

(defmethod text-style-mapping
    ((port graphic-forms-port) text-style &optional character-set)
  ())

(defmethod (setf text-style-mapping)
    (font-name (port graphic-forms-port)
     (text-style text-style) &optional character-set)
  ())

(defmethod port-character-width ((port graphic-forms-port) text-style char)
  #+nil (gfs::debug-format "port-character-width called: ~a ~c~%" text-style char))

(defmethod port-string-width ((port graphic-forms-port) text-style string &key (start 0) end)
  #+nil (gfs::debug-format "port-string-width called: ~a ~c~%" text-style string))

(defmethod port-mirror-width ((port graphic-forms-port) (sheet mirrored-sheet-mixin))
  #+nil (gfs::debug-format "port-mirror-width called for ~a~%" sheet)
  (let ((mirror (climi::port-lookup-mirror port sheet)))
    (gfs:size-width (gfw:size mirror))))

(defmethod port-mirror-height ((port graphic-forms-port) (sheet mirrored-sheet-mixin))
  #+nil (gfs::debug-format "port-mirror-height called for ~a~%" sheet)
  (let ((mirror (climi::port-lookup-mirror port sheet)))
    (gfs:size-height (gfw:size mirror))))

(defmethod port-mirror-width ((port graphic-forms-port) (sheet graphic-forms-graft))
  (graft-width sheet))

(defmethod port-mirror-height ((port graphic-forms-port) (sheet graphic-forms-graft))
  (graft-height sheet))

(defmethod graft ((port graphic-forms-port))
  (first (climi::port-grafts port)))

(defmethod port-allocate-pixmap ((port graphic-forms-port) sheet width height)
  ())

(defmethod port-deallocate-pixmap ((port graphic-forms-port) pixmap)
  #+nil
  (when (climi::port-lookup-mirror port pixmap)
    (destroy-mirror port pixmap)))

(defmethod pointer-position ((pointer gfw-pointer))
  (values (slot-value pointer 'x) (slot-value pointer 'y)))

(defmethod pointer-button-state ((pointer gfw-pointer))
  ())

(defmethod port-modifier-state ((port graphic-forms-port))
  ())

(defmethod synthesize-pointer-motion-event ((pointer gfw-pointer))
  ())

;;; Set the keyboard input focus for the port.

(defmethod port-frame-keyboard-input-focus
    ((port graphic-forms-port) frame)
  ;; fixme
  (frame-properties frame 'focus))

(defmethod (setf port-frame-keyboard-input-focus)
    (focus (port graphic-forms-port) frame)
  (gfw:give-focus (sheet-mirror focus))
  (setf (frame-properties frame 'focus) focus))

(defmethod %set-port-keyboard-focus (focus (port graphic-forms-port) &key timestamp)
  (declare (ignore timestamp))
  ())

(defmethod port-force-output ((port graphic-forms-port))
  ())

;; FIXME: What happens when CLIM code calls tracking-pointer recursively?
(defmethod port-grab-pointer ((port graphic-forms-port) pointer sheet)
  ())

(defmethod port-ungrab-pointer ((port graphic-forms-port) pointer sheet)
  ())

(defmethod set-sheet-pointer-cursor ((port graphic-forms-port) sheet cursor)
  ())        

(defmethod bind-selection ((port graphic-forms-port) window &optional time)
  ())

(defmethod release-selection ((port graphic-forms-port) &optional time)
  ())

(defmethod request-selection ((port graphic-forms-port) requestor time)
  ())

(defmethod get-selection-from-event ((port graphic-forms-port) event)
  ())

(defmethod send-selection ((port graphic-forms-port) event string)
  nil)

(defmethod compose-space ((pane gfw-menu-bar-pane) &key width height)
  (declare (ignore width height))
  (make-space-requirement :width 0 :height 0))

;;;
;;; dispatchers and callbacks
;;;

(defun debug-menu-callback (disp menu)
  (declare (ignore disp))
  (gfs::debug-format "menu: ~a activated~%" menu))

(defun debug-menu-item-callback (disp item)
  (declare (ignore disp))
  (gfs::debug-format "menu item: ~a invoked~%" item))

(defmethod gfw:event-close ((self sheet-event-dispatcher) mirror)
  (enqueue (port self)
	   (make-instance 'window-manager-delete-event :sheet (sheet mirror))))

;; copy&paste from port.lisp|CLX:
(defun sheet-desired-ink (sheet)
  (typecase sheet
    (sheet-with-medium-mixin
      (medium-background sheet))
    (basic-pane
      ;; CHECKME [is this sensible?] seems to be
      (let ((background (pane-background sheet)))
	(if (typep background 'color)
	    background
	    +white+)))
    (t
      +white+)))

(defmethod gfw:event-paint ((self sheet-event-dispatcher) mirror gc rect)
  (let ((sheet (sheet mirror)))
    (when (and (typep sheet 'sheet-with-medium-mixin)
               (not (image-of (sheet-medium sheet))))
      (let ((c (ink-to-color (sheet-medium sheet)
                             (sheet-desired-ink sheet))))
        (setf (gfg:background-color gc) c
              (gfg:foreground-color gc) c))
      (gfg:draw-filled-rectangle gc rect))
    (enqueue (port self)
             (make-instance 'window-repaint-event
                            :sheet sheet
                            :region (translate-rectangle rect)))))

(defun generate-configuration-event (mirror pnt size)
  (make-instance 'window-configuration-event
                 :sheet (sheet mirror)
                 :x (gfs:point-x pnt)
                 :y (gfs:point-y pnt)
                 :width (gfs:size-width size)
                 :height (gfs:size-height size)))

(defmethod gfw:event-resize ((self sheet-event-dispatcher) mirror size type)
  (declare (ignore type))
  (setf size (gfw:client-size mirror))
  (let ((sheet (sheet mirror)))
    (if (and sheet (subtypep (class-of sheet) 'sheet-with-medium-mixin))
        (let ((medium (climi::sheet-medium sheet)))
          (when (and medium (image-of medium))
            (resize-medium-buffer medium size)))))
  (enqueue (port self)
           (generate-configuration-event mirror (gfw:location mirror) size)))

(defmethod gfw:event-move ((self sheet-event-dispatcher) mirror pnt)
  (enqueue (port self)
           (generate-configuration-event mirror pnt (gfw:client-size mirror))))

(defclass gadget-event (window-event) ())
(defclass button-pressed-event (gadget-event) ())

(defmethod gfw:event-select ((self pane-event-dispatcher) mirror)
  (enqueue (port self)
	   (typecase mirror
	     (gfw-button
	      (make-instance 'button-pressed-event :sheet (sheet mirror)))
	     (t
	      (make-instance 'menu-clicked-event
			     :sheet (sheet (gfw:owner mirror))
			     :item (sheet mirror))))))

(defmethod handle-event ((pane push-button) (event button-pressed-event))
  (activate-callback pane (gadget-client pane) (gadget-id pane)))

(defun translate-button-name (name)
  (case name
    (:left-button +pointer-left-button+)
    (:right-button +pointer-right-button+)
    (:middle-button +pointer-middle-button+)
    (t
     (warn "unknown button name: ~A" name)
     nil)))

(defmethod gfw:event-mouse-move
    ((self sheet-event-dispatcher) mirror point button)
  (enqueue (port self)
	   (make-instance 'pointer-motion-event
			  :pointer 0
			  :sheet (sheet mirror)
			  :x (gfs:point-x point)
			  :y (gfs:point-y point)
			  :button (translate-button-name button)
			  ;; FIXME:
;;; 		       :graft-x
;;; 		       :graft-y
			  :modifier-state 0
			  )))

(defmethod gfw:event-mouse-down ((self sheet-event-dispatcher) mirror point button)
  (enqueue (port self)
	   (make-instance 'pointer-button-press-event
			  :pointer 0
			  :sheet (sheet mirror)
			  :x (gfs:point-x point)
			  :y (gfs:point-y point)
			  :button (translate-button-name button)
			  ;; FIXME:
;;; 		       :graft-x
;;; 		       :graft-y
			  :modifier-state 0
			  )))

(defmethod gfw:event-mouse-up ((self sheet-event-dispatcher) mirror point button)
  (enqueue (port self)
	   (make-instance 'pointer-button-release-event
			  :pointer 0
			  :sheet (sheet mirror)
			  :x (gfs:point-x point)
			  :y (gfs:point-y point)
			  :button (translate-button-name button)
			  ;; FIXME:
;;; 		       :graft-x
;;; 		       :graft-y
			  :modifier-state 0
			  )))

(defun char-to-sym (char)
  (case char
    (#\  :| |) (#\! :!) (#\" :|"|) (#\# :|#|) (#\$ :$) (#\% :%) (#\& :&)
    (#\' :|'|) (#\( :|(|) (#\) :|)|) (#\* :*) (#\+ :+) (#\, :|,|) (#\- :-)
    (#\. :|.|) (#\/ :/) (#\0 :|0|) (#\1 :|1|) (#\2 :|2|) (#\3 :|3|) (#\4 :|4|)
    (#\5 :|5|) (#\6 :|6|) (#\7 :|7|) (#\8 :|8|) (#\9 :|9|) (#\: :|:|) (#\; :|;|)
    (#\< :<) (#\= :=) (#\> :>) (#\? :?) (#\@ :@) (#\A :A) (#\B :B) (#\C :C)
    (#\D :D) (#\E :E) (#\F :F) (#\G :G) (#\H :H) (#\I :I) (#\J :J) (#\K :K)
    (#\L :L) (#\M :M) (#\N :N) (#\O :O) (#\P :P) (#\Q :Q) (#\R :R) (#\S :S)
    (#\T :T) (#\U :U) (#\V :V) (#\W :W) (#\X :X) (#\Y :Y) (#\Z :Z) (#\[ :[)
    (#\\ :|\\|) (#\] :]) (#\_ :_) (#\` :|`|) (#\a :|a|) (#\b :|b|) (#\c :|c|)
    (#\d :|d|) (#\e :|e|) (#\f :|f|) (#\g :|g|) (#\h :|h|) (#\i :|i|) (#\j :|j|)
    (#\k :|k|) (#\l :|l|) (#\m :|m|) (#\n :|n|) (#\o :|o|) (#\p :|p|) (#\q :|q|)
    (#\r :|r|) (#\s :|s|) (#\t :|t|) (#\u :|u|) (#\v :|v|) (#\w :|w|) (#\x :|x|)
    (#\y :|y|) (#\z :|z|) (#\{ :{) (#\| :|\||) (#\} :}) (#\Backspace :BACKSPACE)
    (#\Tab :TAB) (#\Return :RETURN) (#\Rubout :DELETE)))

(defmethod gfw:event-key-down ((self sheet-event-dispatcher) mirror code char)
  (enqueue (port self)
	   (make-instance 'key-press-event
			  :key-name (char-to-sym char)
			  :key-character char
			  :sheet (sheet mirror)
			  ;; FIXME:
			  :x 0
			  :y 0
			  :modifier-state 0
;;; 			 :graft-x root-x
;;; 			 :graft-y root-y
			  )))

(defmethod gfw:event-key-up ((self sheet-event-dispatcher) mirror code char)
  (enqueue (port self)
	   (make-instance 'key-release-event
			  :key-name (char-to-sym char)
			  :key-character char
			  :sheet (sheet mirror)
			  ;; FIXME:
			  :x 0
			  :y 0
			  :modifier-state 0
;;; 			 :graft-x root-x
;;; 			 :graft-y root-y
			  )))


;;;
;;; McCLIM handle-event methods
;;;

(defun handle-menu-clicked-event (event)
  (let ((pane (event-item event)))
    (if pane
      (let ((menu-item (item pane)))
        (if menu-item
	    (if (eql (command-menu-item-type menu-item) :command)
		(climi::throw-object-ptype menu-item 'menu-item))
	    (funcall (callback pane) pane nil))))))

(defmethod handle-event ((pane gfw-menu-pane) (event menu-clicked-event))
  (handle-menu-clicked-event event))

(defmethod handle-event ((pane gfw-menu-bar-pane) (event menu-clicked-event))
  (handle-menu-clicked-event event))
