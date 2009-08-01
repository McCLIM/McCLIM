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

(declaim (inline round-coordinate))
(defun round-coordinate (x)
  "Function used for rounding coordinates."
  ;; We use "mercantile rounding", instead of the CL round to nearest
  ;; even number, when in doubt.
  ;;
  ;; Reason: As the CLIM drawing model is specified, you quite often
  ;; want to operate with coordinates, which are multiples of 1/2. 
  ;; Using CL:ROUND gives you "random" results. Using "mercantile
  ;; rounding" gives you consistent results.
  ;;
  ;; Note that CLIM defines pixel coordinates to be at the corners,
  ;; while in X11 they are at the centers. We don't do much about the
  ;; discrepancy, but rounding up at half pixel boundaries seems to
  ;; work well.
  (floor (+ x .5)))

;;; CLX-PORT class

(defclass clx-pointer (standard-pointer)
  ((cursor :accessor pointer-cursor :initform :upper-left)))

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

(defvar *fontset* nil)

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
	  (declare (ignore range-start range-stop))
          (xlib:char-width font (funcall translator point)))
        0)))

(defun fontset-point (point &optional (fontset *fontset*))
  (%fontset-point fontset point))

(defmethod %fontset-point ((fontset fontset) point)

  (with-slots (ranges) fontset
    (assoc point ranges :test (lambda (point range)
                                (<= (car range) point (cdr range))))))

(defclass clx-port (clim-xcommon:keysym-port-mixin basic-port)
  ((display :initform nil
	    :accessor clx-port-display)
   (screen :initform nil
	   :accessor clx-port-screen)
   (window :initform nil
	   :accessor clx-port-window)
   (color-table :initform (make-hash-table :test #'eq))
   (cursor-table :initform (make-hash-table :test #'eq)
                 :accessor clx-port-cursor-table)
   (design-cache :initform (make-hash-table :test #'eq))
   (pointer :reader port-pointer)
   (pointer-grab-sheet :accessor pointer-grab-sheet :initform nil)
   (selection-owner :initform nil :accessor selection-owner)
   (selection-timestamp :initform nil :accessor selection-timestamp)
   (font-families :accessor font-families)))

(defun automagic-clx-server-path ()  
  (let ((name (get-environment-variable "DISPLAY")))
    (assert name (name)
            "Environment variable DISPLAY is not set")
    (let* (; this code courtesy telent-clx.
           (slash-i (or (position #\/ name) -1))
           (colon-i (position #\: name :start (1+ slash-i)))
           (decnet-colon-p (eql (elt name (1+ colon-i)) #\:))
           (host (subseq name (1+ slash-i) colon-i))
           (dot-i (and colon-i (position #\. name :start colon-i)))
           (display (and colon-i
                      (parse-integer name
                                     :start (if decnet-colon-p
                                                (+ colon-i 2)
                                                (1+ colon-i))
                                     :end dot-i)))
           (screen (and dot-i
                     (parse-integer name :start (1+ dot-i))))
           (protocol
            (cond ((or (string= host "") (string-equal host "unix")) :local)
                  (decnet-colon-p :decnet)
                  ((> slash-i -1) (intern
                                   (string-upcase (subseq name 0 slash-i))
                                   :keyword))
                  (t :internet))))
      (list :clx
	    :host host
	    :display-id (or display 0)
	    :screen-id (or screen 0)
	    :protocol protocol))))

(defun helpfully-automagic-clx-server-path ()
  (restart-case (automagic-clx-server-path)
    (use-localhost ()
      :report "Use local unix display"
      (parse-clx-server-path '(:clx :host "" :protocol :unix)))))

(defun parse-clx-server-path (path)
  (pop path)
  (if path
      (list :clx
	    :host       (getf path :host "localhost")
	    :display-id (getf path :display-id 0)
	    :screen-id  (getf path :screen-id 0)
	    :protocol   (getf path :protocol :internet))
      (helpfully-automagic-clx-server-path)))

(setf (get :x11 :port-type) 'clx-port)
(setf (get :x11 :server-path-parser) 'parse-clx-server-path)
(setf (get :clx :port-type) 'clx-port)
(setf (get :clx :server-path-parser) 'parse-clx-server-path)

(defmethod initialize-instance :after ((port clx-port) &rest args)
  (declare (ignore args))
  (push (make-instance 'clx-frame-manager :port port) (slot-value port 'frame-managers))
  (setf (slot-value port 'pointer)
	(make-instance 'clx-pointer :port port))
  (initialize-clx port))

(defmethod print-object ((object clx-port) stream)
  (print-unreadable-object (object stream :identity t :type t)
    (when (slot-boundp object 'display)
      (let ((display (slot-value object 'display)))
	(when display
	  (format stream "~S ~S ~S ~S"
		  :host (xlib:display-host display)
		  :display-id (xlib:display-display display)))))))

(defun clx-error-handler (display error-name &rest args &key major &allow-other-keys)
  (unless (and (eql major 42)  ; 42 is SetInputFocus, we ignore match-errors from that
               (eq error-name 'xlib:match-error))
    (format *error-output* "Received CLX ~A in process ~W~%"
            error-name (clim-sys:process-name (clim-sys:current-process)))
    (apply #'xlib:default-error-handler display error-name args)))

(defvar *clx-cursor-mapping*  
  '(;; These are taken from the Franz CLIM User's Guide
    (:busy 150)
    (:button 60)
    (:default 68)
    (:horizontal-scroll 108)
    (:horizontal-thumb 108)
    (:lower-left 12)
    (:lower-right 14)
    (:move 52)
    (:position 130)
    (:prompt 152)
    (:scroll-down 106)
    (:scroll-left 110)
    (:scroll-right 112)
    (:scroll-up 114)
    (:upper-left 134)
    (:upper-right 136)
    (:vertical-scroll 116)
    (:vertical-thumb 116)
    ;; The following are not in the Franz docs, but might be useful.
    (:i-beam 152)
    (:vertical-pointer 22)
    (:pencil 86)
    (:rotate 50)    
    (:choose 60)))

(defun make-cursor-table (port)
  (declare (optimize (safety 3) (debug 3) (speed 0) (space 0)))
  (let ((font (xlib:open-font (clx-port-display port) "cursor")))
    
    (loop for (symbol code) in *clx-cursor-mapping*
          do (setf (gethash symbol (clx-port-cursor-table port))
                   (xlib:create-glyph-cursor :foreground (xlib:make-color :red 0.0 :green 0.0 :blue 0.0)
                                             :background (xlib:make-color :red 1.0 :green 1.0 :blue 1.0)
                                             :source-font font
                                             :source-char code
                                             :mask-font font
                                             :mask-char (1+ code))))
    (xlib:close-font font)))

(defmethod initialize-clx ((port clx-port))
  (let ((options (cdr (port-server-path port))))
    (setf (clx-port-display port)
	  (xlib:open-display (getf options :host) 
			     :display (getf options :display-id) 
			     :protocol (getf options :protocol)))
    (progn
      (setf (xlib:display-error-handler (clx-port-display port))
        #'clx-error-handler)

      #+nil ;; Uncomment this when debugging CLX backend if asynchronous errors become troublesome..
      (setf (xlib:display-after-function (clx-port-display port)) #'xlib:display-finish-output))
      
    
    (setf (clx-port-screen port) (nth (getf options :screen-id)
				      (xlib:display-roots (clx-port-display port))))
    (setf (clx-port-window port) (xlib:screen-root (clx-port-screen port)))
    (make-cursor-table port)    
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
                   (process-next-event port)) )))
         :name (format nil "~S's event process." port)))
      #+nil(format *trace-output* "~&Started CLX event loop process ~A~%" (port-event-process port))) ))

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
                                (save-under :off)
				(event-mask `(:exposure 
					      :key-press :key-release
					      :button-press :button-release
					      :enter-window :leave-window
					      :structure-notify
					      :pointer-motion
					      :button-motion)))
  ;; I am declaring BORDER-WIDTH ignore to get a cleaner build, but I
  ;; don't really understand why the use of it is commented out in favor
  ;; of the constant 0.  -- RS 2007-07-22
  (declare (ignore border-width))
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
                    :save-under save-under
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
                                      :event-mask '(:key-press :key-release))))
      (setf (xlib:wm-hints window) (xlib:make-wm-hints :input :on))
      (setf (xlib:wm-name window) (frame-pretty-name frame))
      (setf (xlib:wm-icon-name window) (frame-pretty-name frame))
      (xlib:set-wm-class
       window
       (string-downcase (frame-name frame))
       (string-capitalize (string-downcase (frame-name frame))))
      (setf (xlib:wm-protocols window) `(:wm_delete_window))
      (xlib:change-property window
                            :WM_CLIENT_LEADER (list (xlib:window-id window))
                            :WINDOW 32))))

(defmethod realize-mirror ((port clx-port) (sheet unmanaged-top-level-sheet-pane))
  (realize-mirror-aux port sheet
		      :override-redirect :on
                      :save-under :on
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

(defmethod raise-mirror ((port clx-port) (sheet basic-sheet))
  (let ((mirror (sheet-mirror sheet)))
    (when (and mirror
	       (typep mirror 'xlib:window))
      (xlib:circulate-window-up mirror))))

(defmethod bury-mirror ((port clx-port) (sheet basic-sheet))
  (let ((mirror (sheet-mirror sheet)))
    (when (and mirror
	       (typep mirror 'xlib:window))
      (xlib:circulate-window-down mirror))))

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
  (handler-case
      (xlib:close-display (clx-port-display port))
    (stream-error ()
      (xlib:close-display (clx-port-display port) :abort t))))

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
  (let ((button-mapping #.(vector +pointer-left-button+
                                  +pointer-middle-button+
                                  +pointer-right-button+
                                  +pointer-wheel-up+
                                  +pointer-wheel-down+
                                  +pointer-wheel-left+
                                  +pointer-wheel-right+))
        (code (1- code)))
    (when (and (>= code 0)
               (< code (length button-mapping)))
      (aref button-mapping code))))

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
;;
;; Only button and keypress events get a :code keyword argument! For mouse
;; button events, one should use decode-x-button-code; otherwise one needs to
;; look at the state argument to get the current button state. The CLIM spec
;; says that pointer motion events are a subclass of pointer-event, which is
;; reasonable, but unfortunately they use the same button slot, whose value
;; should only be a single button. Yet pointer-button-state can return the
;; logical or of the button values... aaargh. For now I'll canonicalize the
;; value going into the button slot and think about adding a
;; pointer-event-buttons slot to pointer events. -- moore
;; 

(defvar *clx-port*)

(defun event-handler (&key display window event-key code state mode time
		      type width height x y root-x root-y
		      data override-redirect-p send-event-p hint-p
                      target property requestor selection
                      request first-keycode count
                      &allow-other-keys)
  (declare (ignore display request first-keycode count))
  (let ((sheet (and window (port-lookup-sheet *clx-port* window))))
    (when sheet
      (case event-key
	((:key-press :key-release)
         (multiple-value-bind (keyname modifier-state keysym-keyword)
	     (x-event-to-key-name-and-modifiers *clx-port* 
						event-key code state)
           (make-instance (if (eq event-key :key-press)
			      'key-press-event
			      'key-release-event)
             :key-name keysym-keyword
	     :key-character (and (characterp keyname) keyname)
	     :x x :y y
	     :graft-x root-x
	     :graft-y root-y
             :sheet (or (frame-properties (pane-frame sheet) 'focus) sheet)
             :modifier-state modifier-state :timestamp time)))
	((:button-press :button-release)
	 (let ((modifier-state (clim-xcommon:x-event-state-modifiers *clx-port*
								     state)))
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
			:modifier-state (clim-xcommon:x-event-state-modifiers
					 *clx-port* state)
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
			:modifier-state (clim-xcommon:x-event-state-modifiers
					 *clx-port* state)
			:timestamp time))
        ;;
	(:configure-notify
         (cond ((and (eq (sheet-parent sheet) (graft sheet))
                     (graft sheet)
                     (not override-redirect-p)
                     (not send-event-p))
                ;; this is genuine event for a top-level sheet (with
                ;; override-redirect off)
                ;;
                ;; Since the root window is not our real parent, but
                ;; there the window managers decoration in between,
                ;; only the size is correct, so we need to deduce the
                ;; position from our idea of it.

                ;; I believe the code below is totally wrong, because
                ;; sheet-native-transformation will not be up to date.
                ;; Instead, query the new coordinates from the X server,
                ;; and later the event handler will set the correct
                ;; native-transformation using those. --Hefner
;                (multiple-value-bind (x y) (transform-position
;                                            (compose-transformations
;                                             (sheet-transformation sheet)
;                                             (sheet-native-transformation (graft sheet)))
;                                            0 0)

                ;; Easier to let X compute the position relative to the root window for us.
                (multiple-value-bind (x y)
                    (xlib:translate-coordinates window 0 0 (clx-port-window *clx-port*))
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
	 (let ((modifier-state (clim-xcommon:x-event-state-modifiers *clx-port*
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
	((:exposure :display :graphics-exposure)
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
        (:selection-notify
         (make-instance 'clx-selection-notify-event
          :sheet sheet
          :selection selection
          :target target
          :property property))
        (:selection-clear
         (make-instance 'selection-clear-event
          :sheet sheet
          :selection selection))
        (:selection-request
         (make-instance 'clx-selection-request-event
          :sheet sheet
          :selection selection
          :requestor requestor
          :target target
          :property property
          :timestamp time))
	(:client-message
         (port-client-message sheet time type data))
	(t         
	 (unless (xlib:event-listen (clx-port-display *clx-port*))
	   (xlib:display-force-output (clx-port-display *clx-port*)))
	 nil)))))


;; Handling of X client messages

(defmethod port-client-message (sheet time (type (eql :wm_protocols)) data)
  (port-wm-protocols-message sheet time
                             (xlib:atom-name (slot-value *clx-port* 'display) (aref data 0))
                             data))

(defmethod port-client-message (sheet time (type t) data)
  (warn "Unprocessed client message: ~:_type = ~S;~:_ data = ~S;~_ sheet = ~S."
        type data sheet))

;;; this client message is only necessary if we advertise that we
;;; participate in the :WM_TAKE_FOCUS protocol; otherwise, the window
;;; manager is responsible for all setting of input focus for us.  If
;;; we want to do something more complicated with server input focus,
;;; then this method should be adjusted appropriately and the
;;; top-level-sheet REALIZE-MIRROR method should be adjusted to add
;;; :WM_TAKE_FOCUS to XLIB:WM-PROTOCOLS.  CSR, 2009-02-18
(defmethod port-wm-protocols-message (sheet time (message (eql :wm_take_focus)) data)
  (let ((timestamp (elt data 1))
        (mirror (sheet-mirror sheet)))
    (when mirror
      (xlib:set-input-focus (clx-port-display *clx-port*)
                            mirror :parent timestamp))
    nil))

(defmethod port-wm-protocols-message (sheet time (message (eql :wm_delete_window)) data)
  (declare (ignore data))
  (make-instance 'window-manager-delete-event :sheet sheet :timestamp time))

(defmethod port-wm-protocols-message (sheet time (message t) data)
  (warn "Unprocessed WM Protocols message: ~:_message = ~S;~:_ data = ~S;~_ sheet = ~S."
        message data sheet))



(defmethod get-next-event ((port clx-port) &key wait-function (timeout nil))
  (declare (ignore wait-function))
  (let* ((*clx-port* port)
         (display    (clx-port-display port)))
    (unless (xlib:event-listen display)
      (xlib:display-force-output (clx-port-display port)))
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
                                        (getf *clx-text-sizes* :normal)))))
              (flet ((try (encoding)
                       (let* ((fn (format nil "-~A-~A-*-*-~D-*-*-*-*-*-~A"
                                          family-name face-name size-number
                                          encoding))
                              (font (open-font (clx-port-display port) fn)))
                         (and font (cons fn font)))))
                (let ((fn-font
                       (or
                        (and (> char-code-limit #x100) (try "iso10646-1"))
                        (try "iso8859-1")
                        (try "*-*"))))
                  (setf (gethash text-style table) fn-font)
                  (car fn-font)))))))))

(defmethod (setf text-style-mapping) (font-name (port clx-port)
                                      (text-style text-style)
                                      &optional character-set)
  (declare (ignore character-set))
  (setf (gethash text-style (port-text-style-mappings port))
        (cons font-name (open-font (clx-port-display port) font-name)))
  font-name)

(defun text-style-to-X-font (port text-style)
  (let ((text-style (parse-text-style text-style)))
    (text-style-mapping port text-style)
    (cdr (gethash text-style (port-text-style-mappings port)))))

(defmethod port-character-width ((port clx-port) text-style char)
  (let* ((font (text-style-to-X-font port text-style))
	 (width (xlib:char-width font (char-code char))))
    width))

(defmethod port-string-width ((port clx-port) text-style string &key (start 0) end)
  (xlib:text-width (text-style-to-X-font port text-style)
		   string :start start :end end))

(defmethod X-pixel ((port clx-port) color)
  (let ((table (slot-value port 'color-table)))
    (or (gethash color table)
	(setf (gethash color table)
	      (multiple-value-bind (r g b) (color-rgb color)
		(xlib:alloc-color (xlib:screen-default-colormap
                                   (clx-port-screen port))
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

;;; Happily, The McCLIM pointer constants correspond directly to the X
;;; constants.

(defconstant +right-button-mask+ #x100)
(defconstant +middle-button-mask+ #x200)
(defconstant +left-button-mask+ #x400)
(defconstant +wheel-up-mask+ #x800)
(defconstant +wheel-down-mask+ #x1000)

(defmethod pointer-button-state ((pointer clx-pointer))
  (multiple-value-bind (x y same-screen-p child mask)
      (xlib:query-pointer (clx-port-window (port pointer)))
    (declare (ignore x y same-screen-p child))
    (ldb (byte 5 8) mask)))

;;; In button events we don't want to see more than one button, according to
;;; the spec, so pick a canonical ordering. :P The mask is that state mask
;;; from an X event.

(defun button-from-state (mask)
  (cond ((logtest +right-button-mask+ mask)
	   +pointer-right-button+)
	  ((logtest +middle-button-mask+ mask)
	   +pointer-middle-button+)
	  ((logtest +left-button-mask+ mask)
	   +pointer-left-button+)
	  ((logtest +wheel-up-mask+ mask)
	   +pointer-wheel-up+)
	  ((logtest +wheel-down-mask+ mask)
	   +pointer-wheel-down+)
	  (t 0)))

#+nil
(defmethod pointer-modifier-state ((pointer clx-pointer))
  (multiple-value-bind (x y same-screen-p child mask)
      (xlib:query-pointer (clx-port-window (port pointer)))
    (declare (ignore x y same-screen-p child))
    (clim-xcommon:x-event-state-modifiers (port pointer) mask)))

(defmethod port-modifier-state ((port clx-port))
  (multiple-value-bind (x y same-screen-p child mask)
      (xlib:query-pointer (clx-port-window port))
    (declare (ignore x y same-screen-p child))
    (clim-xcommon:x-event-state-modifiers port mask)))

;;; XXX Should we rely on port-pointer-sheet being correct? -- moore
(defmethod synthesize-pointer-motion-event ((pointer clx-pointer))
  (let* ((port (port pointer))
	 (sheet (port-pointer-sheet port)))
    (when sheet
      (let ((mirror (sheet-direct-mirror sheet)))
	(when mirror
	  (multiple-value-bind (x y same-screen-p child mask root-x root-y)
	      (xlib:query-pointer mirror)
	    (declare (ignore child))
	    (when same-screen-p
	      (make-instance
	       'pointer-motion-event
	       :pointer 0 :button (button-from-state mask)
	       :x x :y y
	       :graft-x root-x
	       :graft-y root-y
	       :sheet sheet
	       :modifier-state (clim-xcommon:x-event-state-modifiers port mask)
	       ;; The event initialization code will give us a
	       ;; reasonable timestamp.
	       :timestamp 0))))))))
  
(defmethod port-frame-keyboard-input-focus ((port clx-port) frame)
  (frame-properties frame 'focus))
(defmethod (setf port-frame-keyboard-input-focus) (focus (port clx-port) frame)
  (setf (frame-properties frame 'focus) focus))

(defmethod port-force-output ((port clx-port))
  (xlib:display-force-output (clx-port-display port)))

;; FIXME: What happens when CLIM code calls tracking-pointer recursively?
;; I expect the xlib:grab-pointer call will fail, and so the call to
;; xlib:ungrab-pointer will ungrab prematurely.

;;; XXX Locks around pointer-grab-sheet!!!

(defmethod port-grab-pointer ((port clx-port) pointer sheet)
  ;; FIXME: Use timestamps?
  (let ((grab-result (xlib:grab-pointer
		      (sheet-mirror sheet)
		      '(:button-press :button-release
			:leave-window :enter-window
			:pointer-motion :pointer-motion-hint)
		      ;; Probably we want to set :cursor here..
		      :owner-p t)))
    (if (eq grab-result :success)
	(setf (pointer-grab-sheet port) sheet)
	nil)))

(defmethod port-ungrab-pointer ((port clx-port) pointer sheet)
  (declare (ignore pointer))
  (when (eq (pointer-grab-sheet port) sheet)
    (xlib:ungrab-pointer (clx-port-display port))
    (setf (pointer-grab-sheet port) nil)))

(defmethod distribute-event :around ((port clx-port) event)
  (let ((grab-sheet (pointer-grab-sheet port)))
    (if grab-sheet
	(queue-event grab-sheet event)
	(call-next-method))))

(defmethod set-sheet-pointer-cursor ((port clx-port) sheet cursor)
  (let ((cursor (gethash cursor (clx-port-cursor-table port))))
    (when cursor
      (setf (xlib:window-cursor (sheet-mirror sheet)) cursor))))
        

;;; Modifier cache support

(defmethod clim-xcommon:modifier-mapping ((port clx-port))
  (let* ((display (clx-port-display port))
	 (x-modifiers (multiple-value-list (xlib:modifier-mapping display)))
	 (modifier-map (make-array (length x-modifiers) :initial-element nil)))
    (loop
       for keycodes in x-modifiers
       for i from 0
       do (setf (aref modifier-map i)
		(mapcan (lambda (keycode)
			  (modifier-keycode->keysyms display keycode))
			keycodes)))
    modifier-map))


;;;; Backend component of text selection support

;;; Event classes

(defclass clx-selection-notify-event (selection-notify-event)
  ((target   :initarg :target
             :reader selection-event-target)
   (property :initarg :property
             :reader selection-event-property)))

(defclass clx-selection-request-event (selection-request-event)
  ((target    :initarg :target
              :reader selection-event-target)
   (property  :initarg :property
              :reader selection-event-property)))

;;; Conversions

;; we at least want to support:

;;; :TEXT, :STRING
;;;
;;; :UTF8_STRING
;;;    As seen from xterm [make that the preferred encoding]
;;;
;;; :COMPOUND_TEXT
;;;    Perhaps relatively easy to produce, hard to grok.
;;;
;;; :TARGETS
;;;    Clients want legitimately to find out what we support.
;;;
;;; :TIMESTAMP
;;;    Clients want to know when we took ownership of the selection.

;;; Utilities

(defun utf8-string-encode (code-points)
  (let ((res (make-array (length code-points)
                         :adjustable t
                         :fill-pointer 0)))
    (map 'nil
         (lambda (code-point)
           (cond ((< code-point (expt 2 7))
                  (vector-push-extend code-point res))
                 ((< code-point (expt 2 11))
                  (vector-push-extend (logior #b11000000 (ldb (byte 5 6) code-point)) res)
                  (vector-push-extend (logior #b10000000 (ldb (byte 6 0) code-point)) res))
                 ((< code-point (expt 2 16))
                  (vector-push-extend (logior #b11100000 (ldb (byte 4 12) code-point)) res)
                  (vector-push-extend (logior #b10000000 (ldb (byte 6 6) code-point)) res)
                  (vector-push-extend (logior #b10000000 (ldb (byte 6 0) code-point)) res))
                 ((< code-point (1- (expt 2 21)))
                  (vector-push-extend (logior #b11110000 (ldb (byte 3 18) code-point)) res)
                  (vector-push-extend (logior #b10000000 (ldb (byte 6 12) code-point)) res)
                  (vector-push-extend (logior #b10000000 (ldb (byte 6 6) code-point)) res)
                  (vector-push-extend (logior #b10000000 (ldb (byte 6 0) code-point)) res))
                 ((< code-point (1- (expt 2 26)))
                  (vector-push-extend (logior #b11110000 (ldb (byte 2 24) code-point)) res)
                  (vector-push-extend (logior #b10000000 (ldb (byte 6 18) code-point)) res)
                  (vector-push-extend (logior #b10000000 (ldb (byte 6 12) code-point)) res)
                  (vector-push-extend (logior #b10000000 (ldb (byte 6 6) code-point)) res)
                  (vector-push-extend (logior #b10000000 (ldb (byte 6 0) code-point)) res))
                 ((< code-point (1- (expt 2 31)))
                  (vector-push-extend (logior #b11110000 (ldb (byte 1 30) code-point)) res)
                  (vector-push-extend (logior #b10000000 (ldb (byte 6 24) code-point)) res)
                  (vector-push-extend (logior #b10000000 (ldb (byte 6 18) code-point)) res)
                  (vector-push-extend (logior #b10000000 (ldb (byte 6 12) code-point)) res)
                  (vector-push-extend (logior #b10000000 (ldb (byte 6 6) code-point)) res)
                  (vector-push-extend (logior #b10000000 (ldb (byte 6 0) code-point)) res))
                 (t
                  (error "Bad code point: ~D." code-point))))
         code-points)
    res))

;;; Protocol functions

(defmethod bind-selection ((port clx-port) window &optional time)
  (xlib:set-selection-owner
   (xlib:window-display (sheet-direct-mirror window))
   :primary (sheet-direct-mirror window) time)
  (eq (xlib:selection-owner
       (xlib:window-display (sheet-direct-mirror window))
       :primary)
      (sheet-direct-mirror window)))

(defmethod release-selection ((port clx-port) &optional time)
  (xlib:set-selection-owner
   (clx-port-display port)
   :primary nil time)
  (setf (selection-owner port) nil)
  (setf (selection-timestamp port) nil))

(defmethod request-selection ((port clx-port) requestor time)
  (xlib:convert-selection :primary :STRING requestor :bounce time))

(defmethod get-selection-from-event ((port clx-port) (event clx-selection-notify-event))
  ; (describe event *trace-output*)  
  (if (null (selection-event-property event))
      (progn
        (format *trace-output* "~&;; Oops, selection-notify property is null. Trying the cut buffer instead..~%")
        (xlib:cut-buffer (clx-port-display port)))                
      (map 'string #'code-char
           (xlib:get-property (sheet-direct-mirror (event-sheet event))
                              (selection-event-property event)
                              ;; :type :text
                              :delete-p t
                              :result-type 'vector))))

;; Incredibly crappy broken unportable Latin 1 encoder which should be
;; replaced by various implementation-specific versions.
(flet ((latin1-code-p (x)
	 (not (or (< x 9) (< 10 x 32) (< #x7f x #xa0) (> x 255)))))
  (defun string-encode (string)
    (delete-if-not #'latin1-code-p (map 'vector #'char-code string)))
  (defun exactly-encodable-as-string-p (string)
    (every #'latin1-code-p (map 'vector #'char-code string))))

;;; TODO: INCR property?
;;;
;;; FIXME: per ICCCM we MUST support :MULTIPLE
(defmethod send-selection ((port clx-port) (event clx-selection-request-event) string)
  (let ((requestor (selection-event-requestor event))
        (property  (selection-event-property event))
        (target    (selection-event-target event))
        (time      (event-timestamp event)))
    (when (null property)
      (format *trace-output* "~&* Requestor property is null! *~%"))
    #+nil ; debugging output
    (progn
      (describe event *trace-output*)
      (force-output *trace-output*))
    (flet ((send-event (&key target (property property))
	     ;; debugging output, but the KDE Klipper client turns out
	     ;; to poll other clients for selection, which means it
	     ;; would be bad to print at every request.
             #+nil
             (format *trace-output*
                     "~&;; clim-clx::send-selection - Requested target ~A, sent ~A to property ~A. time ~S~%"
                     (selection-event-target event)
                     target
                     property time)
             (xlib:send-event requestor
			      :selection-notify nil
			      :window requestor
			      :event-window requestor
			      :selection (climi::selection-event-selection event)
			      :target target
			      :property property
			      :time time)))
      (case target
	((:UTF8_STRING)
	 (xlib:change-property requestor property
			       (utf8-string-encode
				(map 'vector #'char-code string))
			       :UTF8_STRING 8)
	 (send-event :target :UTF8_STRING))
	((:STRING :COMPOUND_TEXT)
	 (xlib:change-property requestor property
			       (string-encode string)
			       target 8)            
	 (send-event :target target))
	((:TEXT)
	 (cond
	   ((exactly-encodable-as-string-p string)
	    (xlib:change-property requestor property
				  (string-encode string)
				  :STRING 8)
	    (send-event :target :STRING))
	   (t 
	    (xlib:change-property requestor property
				  (utf8-string-encode
				   (map 'vector #'char-code string))
				  :UTF8_STRING 8)
	    (send-event :target :UTF8_STRING))))
	((:TARGETS)
	 (let* ((display (clx-port-display port))
		(targets (mapcar (lambda (x) (xlib:intern-atom display x))
				 '(:TARGETS :STRING :TEXT :UTF8_STRING
				   :COMPOUND_TEXT :TIMESTAMP))))
	   (xlib:change-property requestor property targets target 32))
	 (send-event :target :TARGETS))
	((:TIMESTAMP)
	 (when (null (selection-timestamp port))
	   (format *trace-output* "~&;; selection-timestamp is null!~%"))
	 (xlib:change-property requestor property
			       (list (selection-timestamp port))
			       target 32)
	 (send-event :target :TIMESTAMP))
	(t
	 (format *trace-output*
		 "~&;; Warning, unhandled type \"~A\". ~
                  Sending property NIL to target.~%" target)
	 (send-event :target target :property nil))))
    (xlib:display-force-output (xlib:window-display requestor))))

;;; XXX CLX in ACL doesn't use local sockets, so here's a fix. This is gross
;;; and should obviously be included in Franz' clx and portable clx, but I
;;; believe that enough users will find that their X servers don't listen for
;;; TCP connections that it is worthwhile to include this code here
;;; temporarily.

#+allegro
(defun xlib::open-x-stream (host display protocol)
  (declare (ignore protocol)) ;; Derive from host
  (let ((stream (if (or (string= host "") (string= host "unix"))
		    (socket:make-socket
		     :address-family :file
		     :remote-filename (format nil "/tmp/.X11-unix/X~D" display)
		     :format :binary)
		    (socket:make-socket :remote-host (string host)
					:remote-port (+ xlib::*x-tcp-port*
                                                        display)
					:format :binary))))
    (if (streamp stream)
	stream
      (error "Cannot connect to server: ~A:~D" host display))))


;;;; Font listing implementation:

(defclass clx-font-family (clim-extensions:font-family)
    ((all-faces :initform nil
		:accessor all-faces
		:reader clim-extensions:font-family-all-faces)))

(defclass clx-font-face (clim-extensions:font-face)
    ((all-sizes :initform nil
		:accessor all-sizes
		:reader clim-extensions:font-face-all-sizes)))

(defun split-font-name (name)
  (loop
      repeat 12
      for next = (position #\- name :start 0)
		 :then (position #\- name :start (1+ next))
      and prev = nil then next
      while next
      when prev
      collect (subseq name (1+ prev) next)))

(defun reload-font-table (port)
  (let ((table (make-hash-table :test 'equal)))
    (dolist (font (xlib:list-font-names (clx-port-display port) "*"))
      (destructuring-bind
	  (&optional foundry family weight slant setwidth style pixelsize 
	   &rest ignore		     	;pointsize xresolution yresolution
					;spacing averagewidth registry encoding
	   )
	  (split-font-name font)
	(declare (ignore setwidth style ignore))
	(when family
	  (let* ((family-name (format nil "~A ~A" foundry family))
		 (family-instance
		  (or (gethash family-name table)
		      (setf (gethash family-name table)
			    (make-instance 'clx-font-family
			      :port port
			      :name family-name))))
		 (face-name (format nil "~A ~A" weight slant))
		 (face-instance
		  (find face-name (all-faces family-instance)
			:key #'clim-extensions:font-face-name
			:test #'equal)))
	    (unless face-instance
	      (setf face-instance
		    (make-instance 'clx-font-face
		      :family family-instance
		      :name face-name))
	      (push face-instance (all-faces family-instance)))
	    (pushnew (parse-integer
		      ;; FIXME: Python thinks pixelsize is NIL, resulting
		      ;; in a full WARNING.  Let's COERCE to make it work.
		      (coerce pixelsize 'string))
		     (all-sizes face-instance))))))
    (setf (font-families port)
	  (sort (loop
		    for family being each hash-value in table
		    do
		      (setf (all-faces family)
			    (sort (all-faces family)
				  #'string<
				  :key #'clim-extensions:font-face-name))
		      (dolist (face (all-faces family))
			(setf (all-sizes face) (sort (all-sizes face) #'<)))
		    collect family)
		#'string<
		:key #'clim-extensions:font-family-name))))

(defmethod clim-extensions:port-all-font-families
    ((port clx-port) &key invalidate-cache)
  (when (or (not (slot-boundp port 'font-families)) invalidate-cache)
    (reload-font-table port))
  (font-families port))

(defmethod clim-extensions:font-face-scalable-p ((face clx-font-face))
  nil)

(defun make-unfriendly-name (str)
  (substitute #\- #\space str))

(defmethod clim-extensions:font-face-text-style
    ((face clx-font-face) &optional size)
  (make-text-style (make-unfriendly-name
		    (clim-extensions:font-family-name
		     (clim-extensions:font-face-family face)))
		   (make-unfriendly-name
		    (clim-extensions:font-face-name face))
		   size))
