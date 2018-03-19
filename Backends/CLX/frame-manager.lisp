;;; -*- Mode: Lisp; Package: CLIM-CLX -*-

;;;  (c) copyright 1998,1999,2000,2001 by Michael McDonald (mikemac@mikemac.com)
;;;  (c) copyright 2004 by Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
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

(in-package :clim-clx)

;;; CLX-FRAME-MANAGER class

(defclass clx-frame-manager (frame-manager)
  ())

;; Abstract pane lookup logic

(defun find-first-defined-class (types)
  (first
   (remove-if #'null 
              (mapcar (lambda (class-name)
                        (find-class class-name nil))
                      types))))

(defun find-symbol-from-spec (package-spec name-components)
  (flet ((coerce-name-element (name-elt)
           (typecase name-elt
             (symbol (symbol-name name-elt))
             (sequence (coerce name-elt 'string))
             (t (princ-to-string name-elt)))))    
  (find-symbol
   (apply #'concatenate 'string (mapcar #'coerce-name-element name-components))
   package-spec)))

(defun find-symbols (name-specs)
  (remove-if #'null (mapcar #'(lambda (x) (find-symbol-from-spec (first x) (rest x))) name-specs)))

(defun generate-standard-pane-specs (type)
  (let ((mapping (get type 'climi::concrete-pane-class-name)))
    `((,(symbol-package mapping) ,mapping)
      (:climi ,mapping)
      (:climi ,type #:-pane)
      (:climi ,type))))

(defun generate-clx-pane-specs (type)
  (append 
   `((:clim-clx #:clx- ,type #:-pane)
     (:clim-clx #:clx- ,type)
     (:climi #:clx- ,type #:-pane)
     (:climi #:clx- ,type))
   (generate-standard-pane-specs type)))

(defun find-concrete-pane-class (type)
  (if (or (eql (symbol-package type)
               (find-package '#:clim))
          (eql (symbol-package type)
               (find-package '#:climi))
          (eql (symbol-package type)
               (find-package '#:keyword))
	  (get type 'climi::concrete-pane-class-name))
      (find-first-defined-class (find-symbols (generate-clx-pane-specs type)))
      type))
  
;;; This is an example of how make-pane-1 might create specialized
;;; instances of the generic pane types based upon the type of the
;;; frame-manager. However, in the CLX case, we don't expect there to
;;; be any CLX specific panes. CLX uses the default generic panes
;;; instead.

;;; if the pane is a subclass of basic-pane and it is not mirrored we create a new class.
(defun maybe-mirroring (concrete-pane-class)
  (when (and (not (subtypep concrete-pane-class 'mirrored-sheet-mixin))
	     (subtypep concrete-pane-class 'basic-pane))
    (let* ((concrete-pane-class-symbol (if (typep concrete-pane-class 'class)
                                          (class-name concrete-pane-class)
                                          concrete-pane-class))
	   (concrete-mirrored-pane-class (concatenate 'string
						      "CLX-"
						      (symbol-name concrete-pane-class-symbol)
						      "-DUMMY"))
	   (concrete-mirrored-pane-class-symbol (find-symbol concrete-mirrored-pane-class
							     :clim-clx)))
      #+(or) (format *debug-io* "use dummy mirrored class ~A~%" concrete-mirrored-pane-class)
      (unless concrete-mirrored-pane-class-symbol
	(setf concrete-mirrored-pane-class-symbol
	      (intern concrete-mirrored-pane-class :clim-clx))
	(eval
	 `(defclass ,concrete-mirrored-pane-class-symbol
	      (standard-full-mirrored-sheet-mixin
               ,concrete-pane-class-symbol)
	    ()
	    (:metaclass ,(type-of (find-class concrete-pane-class-symbol))))))
      #+(or) (format *debug-io* "create class ~A~%" concrete-mirrored-pane-class-symbol)
      (setf concrete-pane-class (find-class concrete-mirrored-pane-class-symbol))))
  concrete-pane-class)

(defmethod make-pane-1 ((fm clx-frame-manager) (frame application-frame) type &rest args)
  (apply #'make-instance
	 (maybe-mirroring (find-concrete-pane-class type))
	 :frame frame
	 :manager fm
	 :port (port frame)
	 args))


(defmethod adopt-frame :before ((fm clx-frame-manager) (frame menu-frame))
  ;; Temporary kludge.
  (when (eq (slot-value frame 'climi::top) nil)
    (multiple-value-bind (x y)
        (xlib:query-pointer (clx-port-window (port fm)))
      (incf x 10)
      (setf (slot-value frame 'climi::left) x
            (slot-value frame 'climi::top) y))))

(defmethod adopt-frame :after ((fm clx-frame-manager) (frame menu-frame))
  (when (sheet-enabled-p (slot-value frame 'top-level-sheet))
    (xlib:map-window (sheet-direct-xmirror (slot-value frame 'top-level-sheet)))))

(defgeneric tell-window-manager-about-space-requirements (pane))

(defmethod adopt-frame :after ((fm clx-frame-manager) (frame application-frame))
  (let ((sheet (slot-value frame 'top-level-sheet)))
    (let* ((top-level-sheet (frame-top-level-sheet frame))
           (mirror (sheet-direct-xmirror top-level-sheet)))
      (case (clim-extensions:find-frame-type frame)
        (:override-redirect (setf (xlib:window-override-redirect mirror) :on))
        (:dialog (xlib:change-property mirror
                                       :_NET_WM_WINDOW_TYPE
                                       (list (xlib:intern-atom (xlib:window-display mirror) :_NET_WM_WINDOW_TYPE_DIALOG))
                                       :atom 32)))
      (multiple-value-bind (w h x y) (climi::frame-geometry* frame)
        (declare (ignore w h))
        (when (and x y)
          (setf (xlib:drawable-x mirror) x
                (xlib:drawable-y mirror) y))
        (tell-window-manager-about-space-requirements top-level-sheet))
      ;; :structure-notify events were not yet turned on, turn them
      ;; on now, so that we get informed about the windows position
      ;; (and possibly size), when the window gets maped.
      (setf (xlib:window-event-mask mirror)
            (logior (xlib:window-event-mask mirror)
                    (xlib:make-event-mask :structure-notify)))
      ;; Care for calling-frame, be careful not to trip on missing bits
      (let* ((calling-frame (frame-calling-frame frame))
             (tls (and calling-frame (frame-top-level-sheet calling-frame)))
             (calling-mirror (and tls (sheet-xmirror tls))))
        (when calling-mirror
          (setf (xlib:transient-for mirror)
                calling-mirror)))
      ;;
      (when (sheet-enabled-p sheet)
        (xlib:map-window mirror)))))

(defmethod tell-window-manager-about-space-requirements ((pane top-level-sheet-pane))
  (multiple-value-bind (w h x y) (climi::frame-geometry* (pane-frame pane))
    (declare (ignore w h))
    (let ((q (compose-space pane)))
      (let ((mirror (sheet-direct-xmirror pane)))
        (setf (xlib:wm-normal-hints mirror)
              (xlib:make-wm-size-hints
               :user-specified-position-p (and x y)
               :x x :y y
               :width  (round (space-requirement-width q))
               :height (round (space-requirement-height q))
               :max-width (min 65535 (round (space-requirement-max-width q)))
               :max-height (min 65535 (round (space-requirement-max-height q)))
               :min-width (round (space-requirement-min-width q))
               :min-height (round (space-requirement-min-height q))))))))

(defmethod tell-window-manager-about-space-requirements ((pane t))
  ;; hmm
  nil)

(defmethod note-space-requirements-changed :after ((graft clx-graft) pane)
  (tell-window-manager-about-space-requirements pane))
