;;; -*- Mode: Lisp; Package: CLIM-CLX; -*-

;;;  (c) copyright 1998,1999,2000 by Michael McDonald (mikemac@mikemac.com)
;;;  (c) copyright 2000,2001 by 
;;;           Iban Hatchondo (hatchond@emi.u-bordeaux.fr)
;;;           Julien Boninfante (boninfan@emi.u-bordeaux.fr)
;;;           Robert Strandh (strandh@labri.u-bordeaux.fr)

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

(in-package :clim-clx)

;;; CLX-PORT class

(defclass clx-pointer (pointer)
  ((cursor :accessor pointer-cursor :initform :upper-left)))

(defclass standard-pointer (clx-pointer)
  ())

#|
 Perhaps this belongs elsewhere

 We have a couple of problems, one is character-sets.

 Unfortunately no-one seems to define what a character-set is, really.
 So, I define a character-set as being the same as a language, since
 a language is far more useful.

 This is important, since a given language may include many characters
 from what might be traditionally considered disparate character-sets.
 Also it is important, since we cannot simply map a character to a glyph
 in a language independent fashion, since the style of the character may
 have some language component.

 In our rendering/translation mechanism we switch fonts when a font fails
 to supply the glyph that we want. So, to facilitate this we need a given
 fontset with a set of disjoint ranges mapped to particular font/encoding
 pairs.

 For the time being, an alist should do here.

 We assume that we're given disjoint ranges for now, which is optimistic.
 Currently building a fontset will be a tricksy business, think about how
 to generalise this for the future.
|#

; this is to inform the text renderer which fontset it should be using.
; it is a complement to the graphics-context stuff, effectively.
; the #'translate uses/needs this to switch fonts

(defclass fontset () (
  ; of the form ((start . stop) font translator)
  (name
    :type     simple-string
    :initform "fontset"
    :initarg  :name
    :reader   fontset-name)
  (default-font
    :initform nil
    :reader fontset-default-font)
  (ranges
    :type     list
    :initform nil
    :initarg  :ranges)
  (ascent
    :type     integer
    :initform 0
    :initarg  :ascent
    :reader   fontset-ascent)
  (descent
    :type     integer
    :initform 0
    :initarg  :ascent
    :reader   fontset-descent)
  (height
    :type     integer
    :initform 0
    :initarg  :ascent
    :reader   fontset-height)
  (width
    :type     integer
    :initform 0
    :initarg  :ascent
    :reader   fontset-width)))

(defmethod print-object ((object fontset) stream)
  (format stream "#<fontset ~A>" (fontset-name object)))

(defmacro make-fontset (name &body entries)
  (let ((fontset (gensym)))
    `(let ((,fontset (make-instance 'fontset :name ,name)))
       ,@(mapcar (lambda (entry)
                   (destructuring-bind (start finish font translator) entry
                     `(set-fontset-range ,fontset ,font ,translator ,start ,finish)))
                 entries)
       ,fontset)))

(defmethod set-fontset-range ((fontset fontset) font translator start finish)
  ; should check ordering invariants, disjointity, etc
  (with-slots (ranges ascent descent height default-font) fontset
    (unless default-font
      (setf default-font font))
    (push (list (cons start finish) font translator) ranges)
    (setf ascent  (max (xlib:font-ascent font) ascent))
    (setf descent (max (xlib:font-descent font) descent))
    (setf height  (+ ascent descent))))

(defun fontset-point-width (point &optional (fontset *fontset*))
  (let ((entry (fontset-point point fontset)))
    (if entry
        (destructuring-bind ((range-start . range-stop) font translator) entry
          (xlib:char-width font (funcall translator point)))
        0)))

(defun fontset-point (point &optional (fontset *fontset*))
  (%fontset-point fontset point))

(defmethod %fontset-point ((fontset fontset) point)

  (with-slots (ranges) fontset
    (assoc point ranges :test (lambda (point range)
                                (<= (car range) point (cdr range))))))

(defclass clx-port (basic-port)
  ((display :initform nil
	    :accessor clx-port-display)
   (screen :initform nil
	   :accessor clx-port-screen)
   (window :initform nil
	   :accessor clx-port-window)
   (color-table :initform (make-hash-table :test #'eq))
   (modifier-cache :initform nil
		   :accessor clx-port-modifier-cache)
   (design-cache :initform (make-hash-table :test #'eq))
   (pointer :reader port-pointer)))

(defun parse-clx-server-path (path)
  (pop path)
  (let* ((s (get-environment-variable "DISPLAY"))
	 (colon (position #\: s))
	 (dot (position #\. s :start colon))
	 (host-name (subseq s 0 colon))
	 (display-number (parse-integer s :start (1+ colon) :end dot))
	 (screen-number (if dot (parse-integer s :start (1+ dot)) 0)))
    (list :clx
	  :host (getf path :host host-name)
	  :display-id (getf path :display-id display-number)
	  :screen-id (getf path :screen-id screen-number))))

(setf (get :x11 :port-type) 'clx-port)
(setf (get :x11 :server-path-parser) 'parse-clx-server-path)
(setf (get :clx :port-type) 'clx-port)
(setf (get :clx :server-path-parser) 'parse-clx-server-path)

(defmethod initialize-instance :after ((port clx-port) &rest args)
  (declare (ignore args))
  (push (make-instance 'clx-frame-manager :port port) (slot-value port 'frame-managers))
  (setf (slot-value port 'pointer)
	(make-instance 'standard-pointer :port port))
  (initialize-clx port))

(defmethod print-object ((object clx-port) stream)
  (print-unreadable-object (object stream :identity t :type t)
    (when (slot-boundp object 'display)
      (format stream "~S ~S ~S ~S"
              :host (xlib:display-host (slot-value object 'display))
              :display-id (xlib:display-display (slot-value object 'display))))))

(defun clx-error-handler (display error-name &key &allow-other-keys)
  (declare (ignore display))
  (format *error-output* "clx-error: ~a~%" error-name))

(defmethod initialize-clx ((port clx-port))
  (let ((options (cdr (port-server-path port))))
    (setf (clx-port-display port)
	  (xlib:open-display (getf options :host "") :display (getf options :display-id 0)))
    (progn
      #+nil
      (setf (xlib:display-error-handler (clx-port-display port))
        #'clx-error-handler)
    
      #-nil
      (setf (xlib:display-after-function (clx-port-display port)) #'xlib:display-force-output))
    
    (setf (clx-port-screen port) (nth (getf options :screen-id 0)
				      (xlib:display-roots (clx-port-display port))))
    (setf (clx-port-window port) (xlib:screen-root (clx-port-screen port)))

    (make-graft port)
    (when clim-sys:*multiprocessing-p*
      (setf (port-event-process port)
        (clim-sys:make-process
         (lambda ()
           (loop
             (with-simple-restart
                 (restart-event-loop
                  "Restart CLIM's event loop.")
               (loop
                 (process-next-event port)))))
         :name (format nil "~S's event process." port)))) ))

#+nil
(defmethod (setf sheet-mirror-transformation) :after (new-value (sheet mirrored-sheet-mixin))
  )

(defmethod port-set-mirror-region ((port clx-port) mirror mirror-region)
  (setf (xlib:drawable-width mirror) (floor (bounding-rectangle-max-x mirror-region))
        (xlib:drawable-height mirror) (floor (bounding-rectangle-max-y mirror-region))))
                                   
(defmethod port-set-mirror-transformation ((port clx-port) mirror mirror-transformation)
  (setf (xlib:drawable-x mirror) (floor (nth-value 0 (transform-position mirror-transformation 0 0)))
        (xlib:drawable-y mirror) (floor (nth-value 1 (transform-position mirror-transformation 0 0)))))



(defun invent-sheet-mirror-transformation-and-region (sheet)
  ;; -> tr region
  (let* ((r (sheet-region sheet))
         (r* (transform-region
              (sheet-native-transformation (sheet-parent sheet))
              (transform-region (sheet-transformation sheet) r)))
         #+nil
         (r*
          (bounding-rectangle
           (region-intersection
                  r*
                  (make-rectangle* 0 0
                                   (port-mirror-width (port sheet) (sheet-parent sheet))
                                   (port-mirror-height (port sheet) (sheet-parent sheet))))))
         (mirror-transformation
          (if (region-equal r* +nowhere+)
              (make-translation-transformation 0 0)
            (make-translation-transformation 
             (bounding-rectangle-min-x r*)
             (bounding-rectangle-min-y r*))))
         (mirror-region
          (untransform-region mirror-transformation r*)))
    (values
     mirror-transformation
     mirror-region)))

(defun realize-mirror-aux (port sheet
				&key (width 100) (height 100) (x 0) (y 0)
				(border-width 0) (border 0)
				(override-redirect :off)
				(map t)
				(backing-store :not-useful)
				(event-mask `(:exposure 
					      :key-press :key-release
					      :button-press :button-release
					      :enter-window :leave-window
					      :structure-notify
					     ;:pointer-motion
                                              :button-motion)))
  (when (null (port-lookup-mirror port sheet))
    (update-mirror-geometry sheet)
    (let* ((desired-color (typecase sheet
                            (sheet-with-medium-mixin
                              (medium-background sheet))
                            (basic-pane ; CHECKME [is this sensible?] seems to be
                              (let ((background (pane-background sheet)))
                                (if (typep background 'color)
                                    background
                                    +white+)))
                            (t
                              +white+)))
           (color (multiple-value-bind (r g b)
                      (color-rgb desired-color)
                    (xlib:make-color :red r :green g :blue b)))
           (pixel (xlib:alloc-color (xlib:screen-default-colormap (clx-port-screen port)) color))
           (window (xlib:create-window
                    :parent (sheet-mirror (sheet-parent sheet))
                    :width (if (%sheet-mirror-region sheet)
                               (round-coordinate (bounding-rectangle-width (%sheet-mirror-region sheet)))
                               width)
                    :height (if (%sheet-mirror-region sheet)
                               (round-coordinate (bounding-rectangle-height (%sheet-mirror-region sheet)))
                               height)
                    :x (if (%sheet-mirror-transformation sheet)
                               (round-coordinate (nth-value 0 (transform-position
                                                               (%sheet-mirror-transformation sheet)
                                                               0 0)))
                               x)
                    :y (if (%sheet-mirror-transformation sheet)
                           (round-coordinate (nth-value 1 (transform-position
                                                           (%sheet-mirror-transformation sheet)
                                                           0 0)))
                           y)
                    :border-width 0 ;;border-width
                    :border border
                    :override-redirect override-redirect
                    :backing-store backing-store
                    :gravity :north-west
                    ;; Evil Hack -- but helps enormously (Has anybody
                    ;; a good idea how to sneak the concept of
                    ;; bit-gravity into CLIM)? --GB
                    :bit-gravity (if (typep sheet 'climi::extended-output-stream)
                                     :north-west
                                     :forget)
                    :background pixel
                    :event-mask (apply #'xlib:make-event-mask
                                       event-mask))))
      (port-register-mirror (port sheet) sheet window)
      (when map
        (xlib:map-window window)
        )))
  (port-lookup-mirror port sheet))

(defmethod realize-mirror ((port clx-port) (sheet mirrored-sheet-mixin))
  (realize-mirror-aux port sheet
                      :border-width 0
                      :map (sheet-enabled-p sheet)))

(defmethod realize-mirror ((port clx-port) (sheet border-pane))
  ;;(rotatef (medium-background (sheet-medium sheet)) (medium-foreground (sheet-medium sheet)))
  (realize-mirror-aux port sheet
		      :border-width 0 ; (border-pane-width sheet)
		      :event-mask '(:exposure
				    :structure-notify)
                      :map (sheet-enabled-p sheet)))

(defmethod realize-mirror ((port clx-port) (sheet top-level-sheet-pane))
  (let ((q (compose-space sheet)))
    #+nil ; SHEET and its descendants are grafted, but unmirrored :-\ -- APD
    (allocate-space sheet
                    (space-requirement-width q)
                    (space-requirement-height q))
    (let ((frame (pane-frame sheet))
          (window (realize-mirror-aux port sheet
                                      :map nil
                                      :width (round-coordinate (space-requirement-width q))
                                      :height (round-coordinate (space-requirement-height q))
                                      :event-mask nil)))
      (setf (xlib:wm-name window) (frame-pretty-name frame))
      (setf (xlib:wm-icon-name window) (frame-pretty-name frame))
      (setf (xlib:wm-protocols window) `(:wm_delete_window)))))

(defmethod realize-mirror ((port clx-port) (sheet unmanaged-top-level-sheet-pane))
  (realize-mirror-aux port sheet
		      :override-redirect :on
		      :map nil
		      :event-mask '(:structure-notify)))

(defmethod realize-mirror ((port clx-port) (sheet menu-button-pane))
  (realize-mirror-aux port sheet
		      :event-mask '(:exposure
				    :key-press :key-release
				    :button-press :button-release
				    :enter-window :leave-window
				    :structure-notify
				   ;:pointer-motion
				    :button-motion
				    :owner-grab-button)
                      :map (sheet-enabled-p sheet)))

(defmethod realize-mirror ((port clx-port) (sheet clim-stream-pane))
  (realize-mirror-aux port sheet
		      :event-mask '(:exposure
				    :key-press :key-release
				    :button-press :button-release
				    :enter-window :leave-window
				    :structure-notify
				    :pointer-motion :pointer-motion-hint
				    :button-motion
				    :owner-grab-button)
                      :map (sheet-enabled-p sheet)))


(defmethod destroy-mirror ((port clx-port) (sheet mirrored-sheet-mixin))
  (when (port-lookup-mirror port sheet)
    (xlib:destroy-window (port-lookup-mirror port sheet))
    (port-unregister-mirror port sheet (sheet-mirror sheet))))

(defmethod mirror-transformation ((port clx-port) mirror)
  (make-translation-transformation (xlib:drawable-x mirror)
                                   (xlib:drawable-y mirror)))



(defmethod port-set-sheet-region ((port clx-port) (graft graft) region)
  (declare (ignore region))
  nil)

(defmethod port-set-sheet-transformation ((port clx-port) (graft graft) transformation)
  (declare (ignore transformation))
  nil)

#+nil
(defmethod port-set-sheet-transformation ((port clx-port) (pane application-pane) transformation)
  (declare (ignore transformation))
  nil)

#+nil
(defmethod port-set-sheet-transformation ((port clx-port) (pane interactor-pane) transformation)
  (declare (ignore transformation))
  nil)

(defmethod port-set-sheet-transformation ((port clx-port) (sheet mirrored-sheet-mixin) transformation)
  (declare (ignore transformation)) ;; why?
  (break)                               ;obsolete now
  (let ((mirror (sheet-direct-mirror sheet)))
    (multiple-value-bind (tr rg) (invent-sheet-mirror-transformation-and-region sheet)
      (multiple-value-bind (x y) (transform-position tr 0 0)
        (multiple-value-bind (x1 y1 x2 y2) (if (eql rg +nowhere+)
                                               (values 0 0 0 0)
                                               (bounding-rectangle* rg))
          (declare (ignore x1 y1))      ;XXX assumed to be 0
          (setf (xlib:drawable-x mirror) (round x)
                (xlib:drawable-y mirror) (round y))
          (setf (xlib:drawable-width mirror)  (clamp 1 (round x2) #xFFFF)
                (xlib:drawable-height mirror) (clamp 1 (round y2) #xFFFF))
          ;;(xlib:clear-area mirror :exposures-p t)
          (invalidate-cached-transformations sheet)
          )))))

(defmethod port-set-sheet-region ((port clx-port) (sheet mirrored-sheet-mixin) region)
  (declare (ignore region)) ;; why?
  (let ((mirror (sheet-direct-mirror sheet)))
    (multiple-value-bind (tr rg) (invent-sheet-mirror-transformation-and-region sheet)
      (declare (ignore tr))
      (multiple-value-bind (x1 y1 x2 y2) (if (eql rg +nowhere+)
                                             (values 0 0 0 0)
                                             (bounding-rectangle* rg))
        (declare (ignore x1 y1))      ;XXX assumed to be 0
        (setf x2 (round x2))
        (setf y2 (round y2))
        (cond ((or (<= x2 0) (<= y2 0))
               ;; XXX
               ;; now X does not allow for a zero width/height window,
               ;; we should unmap instead ...
               ;; Nevertheless we simply clamp
               ))
        (setf (xlib:drawable-width mirror)  (clamp x2 1 #xFFFF)
              (xlib:drawable-height mirror) (clamp y2 1 #xFFFF))))))

(defmethod port-enable-sheet ((port clx-port) (mirror mirrored-sheet-mixin))
  (xlib:map-window (sheet-direct-mirror mirror)) )

(defmethod port-disable-sheet ((port clx-port) (mirror mirrored-sheet-mixin))
  (xlib:unmap-window (sheet-direct-mirror mirror)) )

(defmethod destroy-port :before ((port clx-port))
  (xlib:close-display (clx-port-display port)))

(defmethod port-motion-hints ((port clx-port) (sheet mirrored-sheet-mixin))
  (let ((event-mask (xlib:window-event-mask (sheet-direct-mirror sheet))))
    (if (zerop (logand event-mask
		       #.(xlib:make-event-mask :pointer-motion-hint)))
	nil
	t)))

(defmethod (setf port-motion-hints)
    (val (port clx-port) (sheet mirrored-sheet-mixin))
  (let* ((mirror (sheet-direct-mirror sheet))
	 (event-mask (xlib:window-event-mask mirror)))
    (setf (xlib:window-event-mask mirror)
	  (if val
	      (logior event-mask #.(xlib:make-event-mask :pointer-motion-hint))
	      (logandc2 event-mask
			#.(xlib:make-event-mask :pointer-motion-hint)))))
  val)

; think about rewriting this macro to be nicer
(defmacro peek-event ((display &rest keys) &body body)
  (let ((escape (gensym)))
    `(block ,escape
       (xlib:process-event ,display :timeout 0 :peek-p t :handler
         #'(lambda (&key ,@keys &allow-other-keys)
             (return-from ,escape
               (progn
                 ,@body)))))))

(defun decode-x-button-code (code)
  ;; FIXME: CLIM doesn't provide for more then 3 buttons, how should
  ;; we handle this better?
  (aref #.(vector +pointer-left-button+
                  +pointer-middle-button+
                  +pointer-right-button+
		  nil
		  nil)
        (1- code)))

;; From "Inter-Client Communication Conventions Manual", Version 2.0.xf86.1,
;; section 4.1.5:
;; 
;; |   Advice to Implementors
;; |
;; |   Clients cannot distinguish between the case where a top-level
;; |   window is resized and moved from the case where the window is
;; |   resized but not moved, since a real ConfigureNotify event will be
;; |   received in both cases. Clients that are concerned with keeping
;; |   track of the absolute position of a top-level window should keep
;; |   a piece of state indicating whether they are certain of its
;; |   position. Upon receipt of a real ConfigureNotify event on the
;; |   top-level window, the client should note that the position is
;; |   unknown. Upon receipt of a synthetic ConfigureNotify event, the
;; |   client should note the position as known, using the position in
;; |   this event. If the client receives a KeyPress, KeyRelease,
;; |   ButtonPress, ButtonRelease, MotionNotify, EnterNotify, or
;; |   LeaveNotify event on the window (or on any descendant), the
;; |   client can deduce the top-level window's position from the
;; |   difference between the (event-x, event-y) and (root-x, root-y)
;; |   coordinates in these events. Only when the position is unknown
;; |   does the client need to use the TranslateCoordinates request to
;; |   find the position of a top-level window.
;; |

;; The moral is that we need to distinguish between synthetic and
;; genuine configure-notify events. We expect that synthetic configure
;; notify events come from the window manager and state the correct
;; size and position, while genuine configure events only state the
;; correct size.

(defun event-handler (&rest event-slots
                      &key display window event-key code state mode time
		      width height x y root-x root-y
		      data override-redirect-p send-event-p
		      hint-p
                      &allow-other-keys)
  ;; NOTE: Although it might be tempting to compress (consolidate)
  ;; events here, this is the wrong place. In our current architecture
  ;; the process calling this function (the port's event handler
  ;; process) just reads the events from the X server, and does it
  ;; with almost no lack behind the reality. While the application
  ;; frame's event top level loop does the actual processing of events
  ;; and thus may produce lack. So the events have to be compressed in
  ;; the frame's event queue.
  ;;
  ;; So event compression is implemented in EVENT-QUEUE-APPEND.
  ;;
  ;; This changes for possible _real_ immediate repainting sheets,
  ;; here a possible solution for the port's event handler loop can be
  ;; to read all available events off into a temponary queue (and
  ;; event compression for immediate events is done there) and then
  ;; dispatch all events from there as usual.
  ;;
  ;;--GB
  
  ;; XXX :button code -> :button (decode-x-button-code code)
  (declare (ignorable event-slots))
  (declare (special *clx-port*))
  (let ((sheet (and window
		    (port-lookup-sheet *clx-port* window))))
    (when sheet
      (case event-key
	((:key-press :key-release)
         (multiple-value-bind (keyname modifier-state)
	     (x-event-to-key-name-and-modifiers *clx-port* 
						event-key code state)
           (make-instance (if (eq event-key :key-press)
			      'key-press-event
			      'key-release-event)
             :key-name keyname
	     :key-character (and (characterp keyname) keyname)
	     :x x :y y
	     :graft-x root-x
	     :graft-y root-y
             :sheet sheet :modifier-state modifier-state :timestamp time)))
	((:button-press :button-release)
	 (let ((modifier-state (x-event-state-modifiers *clx-port* state)))
	   (make-instance (if (eq event-key :button-press)
			      'pointer-button-press-event
			      'pointer-button-release-event)
			  :pointer 0
			  :button (decode-x-button-code code) :x x :y y
			  :graft-x root-x
			  :graft-y root-y
			  :sheet sheet :modifier-state modifier-state
			  :timestamp time)))
	(:enter-notify
	 (make-instance 'pointer-enter-event :pointer 0 :button code :x x :y y
                        :graft-x root-x
                        :graft-y root-y
			:sheet sheet
			:modifier-state (x-event-state-modifiers *clx-port*
								 state)
			:timestamp time))
	(:leave-notify
	 (make-instance (if (eq mode :ungrab)
			    'pointer-ungrab-event
			    'pointer-exit-event)
			:pointer 0 :button code
			:x x :y y
			:graft-x root-x
			:graft-y root-y
			:sheet sheet
			:modifier-state (x-event-state-modifiers *clx-port*
								 state)
			:timestamp time))
        ;;
	(:configure-notify
         ;; it would be nice to consolidate these for resizes, but because of the
         ;; interleaving exposures it becomes a bit tricky to do at this point. - BTS
         (cond ((and (eq (sheet-parent sheet) (graft sheet))
                     (not override-redirect-p)
                     (not send-event-p))
                ;; this is genuine event for a top-level sheet (with
                ;; override-redirect off)
                ;;
                ;; Since the root window is not our real parent, but
                ;; there the window managers decoration in between,
                ;; only the size is correct, so we need to deduce the
                ;; position from our idea of it.
                (multiple-value-bind (x y) (transform-position
                                            (compose-transformations
                                             (sheet-transformation sheet)
                                             (sheet-native-transformation (graft sheet)))
                                            0 0)
                  (make-instance 'window-configuration-event
                                 :sheet sheet
                                 :x x
                                 :y y
                                 :width width :height height)))
               (t
                ;; nothing special here
                (make-instance 'window-configuration-event
                               :sheet sheet
                               :x x :y y :width width :height height))))
	(:destroy-notify
	 (make-instance 'window-destroy-event :sheet sheet))
	(:motion-notify
	 (let ((modifier-state (x-event-state-modifiers *clx-port*
							state)))
	   (if hint-p
	       (multiple-value-bind (x y same-screen-p child mask
				     root-x root-y)
		   (xlib:query-pointer window)
		 (declare (ignore mask))
		 ;; If not same-screen-p or the child is different
		 ;; from the original event, assume we're way out of date
		 ;; and don't return an event.
		 (when (and same-screen-p (not child))
		   (make-instance 'pointer-motion-hint-event
				  :pointer 0 :button code
				  :x x :y y
				  :graft-x root-x :graft-y root-y
				  :sheet sheet
				  :modifier-state modifier-state
				  :timestamp time)))
	       (progn
		 (make-instance 'pointer-motion-event
				:pointer 0 :button code
				:x x :y y
				:graft-x root-x
				:graft-y root-y
				:sheet sheet
				:modifier-state modifier-state
				:timestamp time)))))
        ;;
	((:exposure :display)
         ;; Notes:
         ;; . Do not compare count with 0 here, last rectangle in an
         ;;   :exposure event sequence does not cover the whole region. 
         ;;
         ;; . Do not transform the event region here, since
         ;;   WINDOW-EVENT-REGION does it already. And rightfully so. 
         ;;   (think about changing a sheet's native transformation).
         ;;--GB
         ;;
         ;; Mike says:
         ;;   One of the lisps is bogusly sending a :display event instead of an
         ;; :exposure event. I don't remember if it's CMUCL or SBCL. So the
         ;; :display event should be left in.
         ;;
         (make-instance 'window-repaint-event
           :timestamp time
           :sheet sheet
           :region (make-rectangle* x y (+ x width) (+ y height))))
        ;;
	(:client-message
	 (when (eq (xlib:atom-name display (aref data 0)) :wm_delete_window)
	   (make-instance 'window-manager-delete-event
	     :sheet sheet
	     :timestamp time)))
	(t
	 nil)))))

(defmethod get-next-event ((port clx-port) &key wait-function (timeout nil))
  (declare (ignore wait-function))
  (let* ((*clx-port* port)
         (display    (clx-port-display port)))
    (declare (special *clx-port*))
    (unless (xlib:event-listen display)
      (xlib:display-finish-output (clx-port-display port)))
    ; temporary solution
    (or (xlib:process-event (clx-port-display port) :timeout timeout :handler #'event-handler :discard-p t)
	:timeout)))
;; [Mike] Timeout and wait-functions are both implementation 
;;        specific and hence best done in the backends.


(defmethod make-graft ((port clx-port) &key (orientation :default) (units :device))
  (let ((graft (make-instance 'clx-graft
		 :port port :mirror (clx-port-window port)
		 :orientation orientation :units units)))
    (setf (sheet-region graft) (make-bounding-rectangle 0 0 (xlib:screen-width (clx-port-screen port)) (xlib:screen-height (clx-port-screen port))))
    (push graft (port-grafts port))
    graft))

(defmethod make-medium ((port clx-port) sheet)
  (make-instance 'clx-medium 
		 ;; :port port 
		 ;; :graft (find-graft :port port) 
		 :sheet sheet))

(defconstant *clx-text-families* '(:fix         "adobe-courier"
				   :serif       "adobe-times"
				   :sans-serif  "adobe-helvetica"))

(defconstant *clx-text-faces* '(:roman          "medium-r"
				:bold           "bold-r"
				:italic         "medium-i"
				:bold-italic    "bold-i"
				:italic-bold    "bold-i"))

(defparameter *clx-text-sizes* '(:normal         14
				:tiny            8
				:very-small     10
				:small          12
				:large          18
				:very-large     20
				:huge           24))

(defparameter *clx-text-family+face-map*
  '(:fix
    #-nil
    ("adobe-courier"
     (:roman               "medium-r"
      :bold                "bold-r"
      :italic              "medium-o"
      :bold-italic         "bold-o"
      :italic-bold         "bold-o"))
    #+nil
    ("*-lucidatypewriter"
     (:roman               "medium-r"
      :bold                "bold-r"
      :italic              "medium-r"
      :bold-italic         "bold-r"
      :italic-bold         "bold-r"))
    :sans-serif
    ("adobe-helvetica"
     (:roman               "medium-r"
      :bold                "bold-r"
      :italic              "medium-o"
      :bold-italic         "bold-o"
      :italic-bold         "bold-o"))
    :serif
    ("adobe-times"
     (:roman               "medium-r"
      :bold                "bold-r"
      :italic              "medium-i"
      :bold-italic         "bold-i"
      :italic-bold         "bold-i")) ))

(defun open-font (display font-name)


  (let ((fonts (xlib:list-font-names display font-name :max-fonts 1)))
    (if fonts
	(xlib:open-font display (first fonts))
        (xlib:open-font display "fixed"))))

(defvar *fontset* nil)

#-unicode
(defmethod text-style-mapping ((port clx-port) text-style
                               &optional character-set)
  (declare (ignore character-set))

  (let ((table (port-text-style-mappings port)))
    (or (car (gethash text-style table))
        (multiple-value-bind (family face size)
            (text-style-components text-style)
          (destructuring-bind (family-name face-table)
              (if (stringp family)
                  (list family *clx-text-faces*)
                  (or (getf *clx-text-family+face-map* family)
                      (getf *clx-text-family+face-map* :fix)))
            (let* ((face-name (if (stringp face)
                                  face
                                  (or (getf face-table
                                            (if (listp face)
                                                (intern (format nil "~A-~A"
                                                                (symbol-name (first face))
                                                                (symbol-name (second face)))
                                                        :keyword)
                                                face))
                                      (getf *clx-text-faces* :roman))))
                   (size-number (if (numberp size)
                                    (round size)
                                    (or (getf *clx-text-sizes* size)
                                        (getf *clx-text-sizes* :normal))))
                   (font-name (format nil "-~A-~A-*-*-~D-*-*-*-*-*-*-*"
                                      family-name face-name size-number)))
              (setf (gethash text-style table)
                    (cons font-name
                          (open-font (clx-port-display port) font-name)))
              font-name))))))

#+unicode
(defun build-english-font-name (text-style)
  (multiple-value-bind (family face size language)
      (text-style-components text-style)
    (destructuring-bind (family-name face-table)
         (if (stringp family)
             (list family *clx-text-faces*)
             (or (getf *clx-text-family+face-map* family)
                 (getf *clx-text-family+face-map* :fix)))
       (let* ((face-name (if (stringp face)
                             face
                             (or (getf face-table
                                       (if (listp face)
                                           (intern (format nil "~A-~A"
                                                           (symbol-name (first face))
                                                           (symbol-name (second face)))
                                                   :keyword)
                                           face))
                                 (getf *clx-text-faces* :roman))))
              (size-number (if (numberp size)
                               (round size)
                               (or (getf *clx-text-sizes* size)
                                   (getf *clx-text-sizes* :normal))))
              (font-name (format nil "-~A-~A-*-*-~D-*-*-*-*-*-*-*"
                                 family-name face-name size-number)))
          font-name))))

#+unicode
(defun build-korean-font-name (text-style)
  (multiple-value-bind (family face size language)
      (text-style-components text-style)
    (let* ((face (if (equal face '(:bold :italic)) :bold-italic face))
           (font (case family
                   ((:fix nil)
                    (case face
                      ((:roman nil)          "baekmuk-dotum-medium-r")
                      ((:bold)               "baekmuk-dotum-bold-r")
                      ((:italic)             "baekmuk-dotum-medium-r")
                      ((:bold-italic)        "baekmuk-dotum-bold-r")))
                   ((:serif)
                    (case face
                      ((:roman nil)          "baekmuk-batang-medium-r")
                      ((:bold)               "baekmuk-batang-bold-r")
                      ((:italic)             "baekmuk-batang-medium-r")
                      ((:bold-italic)        "baekmuk-batang-bold-r")))
                   ((:sans-serif)
                    (case face
                      ((:roman nil)          "baekmuk-gulim-medium-r")
                      ((:bold)               "baekmuk-gulim-bold-r")
                      ((:italic)             "baekmuk-gulim-medium-r")
                      ((:bold-italic)        "baekmuk-gulim-bold-r")))))
            (size-number (if (numberp size)
                             (round size)
                             (or (getf *clx-text-sizes* size)
                                 (getf *clx-text-sizes* :normal)))))
      (format nil "-~A-*-*-~D-*-*-*-*-*-ksx1001.1997-*" font size-number))))

; this needs much refactoring... FIXME
#+unicode
(defmethod text-style-mapping ((port clx-port) text-style
                               &optional character-set)
  (declare (ignore character-set))

  (let ((table (port-text-style-mappings port)))
    (or (car (gethash text-style table))
        (multiple-value-bind (family face size language)
            (text-style-components text-style)
          (let* ((display (clx-port-display port))
                 (fontset (case language
                            ((nil :english)
                             (let* ((font-name (build-english-font-name text-style))
                                    (font      (xlib:open-font display  font-name)))
                               (make-fontset font-name
                                 (0 255 font #'external-format::ascii-code-to-font-index))))
                            ((:korean)
                             (let* ((english-font-name (build-english-font-name text-style))
                                    (english-font      (xlib:open-font display  english-font-name))
                                    (korean-font-name  (build-korean-font-name  text-style))
                                    (korean-font       (xlib:open-font display  korean-font-name)))
                               (make-fontset korean-font-name
                                 (0      255    english-font
                                                #'external-format::ascii-code-to-font-index)
                                 (#xAC00 #xD7A3 korean-font
                                                #'external-format::ksc5601-code-to-font-index)
                                 (#x4E00 #x9FA5 korean-font
                                                #'external-format::ksc5601-code-to-font-index)))))))
            (setf (gethash text-style table)
                  (cons (fontset-name fontset) fontset))
            (fontset-name fontset))))))

(defmethod (setf text-style-mapping) (font-name (port clx-port)
                                      (text-style text-style)
                                      &optional character-set)
  (declare (ignore character-set))
  (setf (gethash text-style (port-text-style-mappings port))
        (cons font-name (open-font (clx-port-display port) font-name)))
  font-name)

#-unicode
(defun text-style-to-X-font (port text-style)
  (let ((text-style (parse-text-style text-style)))
    (text-style-mapping port text-style)
    (cdr (gethash text-style (port-text-style-mappings port)))))

#+unicode
(defun text-style-to-X-fontset (port text-style)
  (let ((text-style (parse-text-style text-style)))
    (text-style-mapping port text-style)
    (cdr (gethash text-style (port-text-style-mappings port)))))

#-unicode
(defmethod port-character-width ((port clx-port) text-style char)
  (let* ((font (text-style-to-X-font port text-style))
	 (width (xlib:char-width font (char-code char))))
    width))

#+unicode
(defmethod port-character-width ((port clx-port) text-style char)
  (fontset-point-width (char-code char) (text-style-to-X-fontset port text-style)))

#-unicode
(defmethod port-string-width ((port clx-port) text-style string &key (start 0) end)
  (xlib:text-width (text-style-to-X-font port text-style)
		   string :start start :end end))

#+unicode ; this requires a translator and so on.
(defmethod port-string-width ((port clx-port) text-style string &key (start 0) end)
  (let ((*fontset* (text-style-to-X-fontset port text-style)))
    (xlib:text-width nil string :start start :end end :translator #'translate)))

(defmethod X-pixel ((port clx-port) color)
  (let ((table (slot-value port 'color-table)))
    (or (gethash color table)
	(setf (gethash color table)
	      (multiple-value-bind (r g b) (color-rgb color)
		(xlib:alloc-color (xlib:screen-default-colormap
				   (first (xlib:display-roots (clx-port-display port))))
				  (xlib:make-color :red r :green g :blue b)))))))

(defmethod port-mirror-width ((port clx-port) sheet)
  (let ((mirror (port-lookup-mirror port sheet)))
    (xlib:drawable-width mirror)))

(defmethod port-mirror-height ((port clx-port) sheet)
  (let ((mirror (port-lookup-mirror port sheet)))
    (xlib:drawable-height mirror)))

(defmethod graft ((port clx-port))
  (first (port-grafts port)))

;;; Pixmap

(defmethod realize-mirror ((port clx-port) (pixmap pixmap))
  (when (null (port-lookup-mirror port pixmap))
    (let* ((window (sheet-direct-mirror (pixmap-sheet pixmap)))
	   (pix (xlib:create-pixmap 
		    :width (round (pixmap-width pixmap))
		    :height (round (pixmap-height pixmap))
		    :depth (xlib:drawable-depth window)
		    :drawable window)))
      (port-register-mirror port pixmap pix))
    (values)))

(defmethod destroy-mirror ((port clx-port) (pixmap pixmap))
  (when (port-lookup-mirror port pixmap)
    (xlib:free-pixmap (port-lookup-mirror port pixmap))
    (port-unregister-mirror port pixmap (port-lookup-mirror port pixmap))))

(defmethod port-allocate-pixmap ((port clx-port) sheet width height)
  (let ((pixmap (make-instance 'mirrored-pixmap
			       :sheet sheet
			       :width width
			       :height height
			       :port port)))
    (when (sheet-grafted-p sheet)
      (realize-mirror port pixmap))
    pixmap))

(defmethod port-deallocate-pixmap ((port clx-port) pixmap)
  (when (port-lookup-mirror port pixmap)
    (destroy-mirror port pixmap)))

;; Top-level-sheet

;; this is evil.
(defmethod allocate-space :after ((pane top-level-sheet-pane) width height)
  (when (sheet-direct-mirror pane)
    (with-slots (space-requirement) pane
      '(setf (xlib:wm-normal-hints (sheet-direct-mirror pane))
            (xlib:make-wm-size-hints 
             :width (round width)
             :height (round height)
             :max-width (min 65535 (round (space-requirement-max-width space-requirement)))
             :max-height (min 65535 (round (space-requirement-max-height space-requirement)))
             :min-width (round (space-requirement-min-width space-requirement))
             :min-height (round (space-requirement-min-height space-requirement)))))))


(defmethod pointer-position ((pointer clx-pointer))
  (let* ((port (port pointer))
	 (sheet (port-pointer-sheet port)))
    (when sheet
      (multiple-value-bind (x y same-screen-p)
	  (xlib:query-pointer (sheet-direct-mirror sheet))
	(when same-screen-p
	  (untransform-position (sheet-native-transformation sheet) x y))))))

;;; pointer button bits in the state mask

(defconstant +right-button-mask+ #x100)
(defconstant +middle-button-mask+ #x200)
(defconstant +left-button-mask+ #x400)

(defmethod pointer-button-state ((pointer clx-pointer))
  (multiple-value-bind (x y same-screen-p child mask)
      (xlib:query-pointer (clx-port-window (port pointer)))
    (declare (ignore x y same-screen-p child))
    (cond ((logtest +right-button-mask+ mask)
	   +pointer-right-button+)
	  ((logtest +middle-button-mask+ mask)
	   +pointer-middle-button+)
	  ((logtest +left-button-mask+ mask)
	   +pointer-left-button+)
	  (t 0))))

(defmethod pointer-modifier-state ((pointer clx-pointer))
  (multiple-value-bind (x y same-screen-p child mask)
      (xlib:query-pointer (clx-port-window (port pointer)))
    (declare (ignore x y same-screen-p child))
    (x-event-state-modifiers (port pointer) mask)))
