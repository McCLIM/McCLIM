
;;; -*- Mode: Lisp; Package: BEAGLE; -*-

;;;  (c) copyright 1998,1999,2000 by Michael McDonald (mikemac@mikemac.com)
;;;  (c) copyright 2000,2001 by 
;;;           Iban Hatchondo (hatchond@emi.u-bordeaux.fr)
;;;           Julien Boninfante (boninfan@emi.u-bordeaux.fr)
;;;           Robert Strandh (strandh@labri.u-bordeaux.fr)
;;;  (c) copyright 2003, 2004 by
;;;           Duncan Rose (duncan@robotcat.demon.co.uk)

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

(in-package :beagle)

;;; The beagle backend of McCLIM runs inside a Cocoa application. There is one
;;; port, associated with that application. There is no separate event process;
;;; the main thread of the Cocoa app is notified of events and puts them on the
;;; proper event queues using the mapping in the port object between NSViews
;;; and sheets.

(defparameter *beagle-port* nil)

(defparameter *default-beagle-frame-manager* 'beagle:beagle-aqua-frame-manager
  "Specifies the frame manager that should be used by default when the port creates its
frame manager. Permissable values are 'beagle::beagle-standard-frame-manager and
'beagle::beagle-aqua-frame-manager (the default).")

;;; ::FIXME:: this holds the current cursor (mouse pointer) being used...
;;; It's not implemented properly I don't think; McCLIM core should know
;;; at least a little about pointers, and it appears to all be handled in
;;; the back end at the moment.

(defclass beagle-pointer (standard-pointer)
  ((cursor :accessor pointer-cursor :initform :upper-left)))

(defclass beagle-cursor ()
  ((image   :accessor cursor-image   :initform nil)
   (hotspot :accessor cursor-hotspot :initform nil)))


;;; Similar to clx-port. We aren't caching modifiers.
;;; Additionally we shove all the views we create into view-table, which is brain-
;;; damaged. This allows us to look up the appropriate mirror / medium when we're handling
;;; events, but the whole medium / mirror / sheet / NSView relationship thing needs rethinking
;;; in the long-term. (NB. this duplicates behaviour in McCLIM "PORT" type but that uses
;;; 'eq tests in the hashtables, and MACPTRs (which is what a mirror boils down to being in
;;; the end) are only ever 'eql.)
;;;
;;; I'm not sure what Duncan was getting at with that comment;
;;; mirror/sheet/NSView seems like a fine correspondence to me, unless you want
;;; to only use one mirror per window. -- moore

(defclass beagle-port (basic-port)
  ((screen          :initform nil :accessor beagle-port-screen)
   (color-table     :initform (make-hash-table :test #'eq))
   (view-table      :initform (make-hash-table :test #'eql))
   (pointer         :reader   port-pointer)
   (design-cache    :initform (make-hash-table :test #'eq))
   ;; holds sheet that should receive key events. ::FIXME:: need to tell McCLIM which sheet
   ;; is taking keyboard events; look into how all that bit hangs together, it's changed
   ;; since this was written.
   (key-focus-sheet :initform nil :accessor beagle-port-key-focus)))

(defmethod destroy-port :before ((port beagle-port))
  ;; clear out color-table, view-table, cached images etc. ::TODO:: check this logic is correct...

  ;; The color table is populated as necessary in %beagle-pixel, which 'RETAINs the NSColor
  ;; objects it creates. We release them when the port is killed.
  (maphash #'(lambda (nscolor) (send nscolor 'release)) (slot-value port 'color-table))
  ;; No need to clear out the hash-table since the port is being destroyed...
;;;  (setf (slot-value port 'color-table) (make-hash-table :test #'eq))

  ;; Look in detail at where / how mirrors are populated in the caches, and where they're
  ;; destroyed.
  
;;  (maphash #'(lambda (nsview) (send nsview 'release)) (slot-value port 'view-table))
;;  (maphash #'(lambda (design) (send design 'release)) (slot-value port 'design-cache))
  (format *debug-io* "DESTROY-PORT ::FINISH ME::~%"))


;;; mirrors parse-clx-server-path, but the cocoa server only needs to know the screen its running
;;; on (it's more like the :genera port than an X port in that respect). I'm not sure this is
;;; doing the right thing. How will we permit the user to run the McCLIM app on a secondary screen?
;;; ::TODO:: not sure this is right; not doing any parsing!
(defun parse-beagle-server-path (path)
  (pop path)
  (let ((screen (send (@class "NSScreen") 'MAIN-SCREEN)))
    (list :beagle :screen (getf path :screen screen))))


(setf (get :beagle :port-type) 'beagle-port)
(setf (get :beagle :server-path-parser) 'parse-beagle-server-path)


;;; As for CLX/port.lisp. At the moment, the default frame manager provides the "standard" McCLIM
;;; look and feel. When a Cocoa look and feel realizer is implemented, we might want to change
;;; this (though the user should still be able to select whichever they want via
;;; "with-look-and-feel-realization (frame-manager frame)".
(defmethod initialize-instance :after ((port beagle-port) &rest args)
  "Initialises an instance of a BEAGLE-PORT. This makes an instance of the default
FRAME-MANAGER and standard-pointer for this port type."
  (declare (ignore args)
	   (special *beagle-port* *default-frame-manager* *default-beagle-frame-manager*))
  (if (null *default-frame-manager*)
      (push (make-instance *default-beagle-frame-manager* :port port)
	    (slot-value port 'frame-managers))
    (push (make-instance *default-frame-manager* :port port) (slot-value port 'frame-managers)))
  (setf (slot-value port 'pointer)
	(make-instance 'beagle-pointer :port port))
  (setf *beagle-port* port)
  (initialize-beagle port))


;;; Another work-alike to the CLX/port.lisp code.
(defmethod print-object ((object beagle-port) stream)
  "Print an unreadable version of the port to the screen; useful for debugging
purposes. Note that it doesn't matter that the :screen in the beagle port is
not initialised, we'll just get \":screen NIL\" in that case."
;;;  (print-unreadable-object (object stream :identity t :type t)
  (print-unreadable-object (object stream :identity nil :type t)
    (when (slot-boundp object 'screen)
;;;      (format stream ":screen ~S" (beagle-port-screen object)))))
      (format stream ":screen MAIN-SCREEN"))))


;;; ::TODO:: Screen selection could be more dynamic (using
;;; the port-server-path? Not sure...) to permit the user to make use of screens other
;;; than the main screen.
(defmethod initialize-beagle ((port beagle-port))
  ;; If we were not already running inside a Cocoa application (OpenMCL's
  ;; COCOA), we would obviously have to invoke the magic here to start up a
  ;; Cocoa main thread.
  (ccl::create-autorelease-pool)	;XXX Is this necessary? -- moore
  
  ;; CLX port gets some options here and uses those to set stuff up. We should probably do
  ;; this too, in the future ::FIXME::
  (setf (beagle-port-screen port) (send (@class "NSScreen") 'MAIN-SCREEN))

;;;  ;; Get the application recognised as multithreaded by Cocoa - this is supposed to cause
;;;  ;; Cocoa to do more sanity checking, though I'm not clear exactly what practical difference
;;;  ;; it makes (if any)
;;;  (unless (send (@class ns-thread) 'is-multi-threaded)
;;;    (send (@class ns-thread)
;;;		  :DETACH-NEW-THREAD-SELECTOR (get-selector-for "setHiddenUntilMouseMoves:")
;;;		  :TO-TARGET (@class "NSCursor")
;;;		  :WITH-OBJECT nil))
  
  (make-cursor-table port)
  (make-graft port))


;;; From CLX/port.lisp
(defun %beagle-pixel (port color &key (alpha #.(cg-floatify 1.0)))
  (let* ((table (slot-value port 'color-table))
	 (nscol (gethash color table)))
    (when (null nscol)
      (setf (gethash color table)
	    (multiple-value-bind (r g b) (color-rgb color)
	      (let ((nsc (send (@class ns-color) :color-with-calibrated-red (cg-floatify r)
			       :green (cg-floatify g)
			       :blue (cg-floatify b)
			       :alpha (cg-floatify alpha))))
		(send nsc 'retain)))))
    (gethash color table)))

;; Hrm... don't think this is ever invoked.
(defmethod graft ((port beagle-port))
  (format *debug-io* "IS METHOD EVER INVOKED? (graft port)~%")
  (first (port-grafts port)))


;;; This method's been added for the Beagle back end.
(defun %beagle-port-lookup-sheet-for-view (port view)
  (let ((table (slot-value port 'view-table)))
    (gethash view table)))


;;; The following are direct copies of the ones in CLX/port.lisp.
;;; They should be suitable for our purposes.
;;; Invoked from 'pixmap.lisp'
(defmethod port-allocate-pixmap ((port beagle-port) sheet width height)
  (let ((pixmap (make-instance 'mirrored-pixmap
			       :sheet sheet
			       :width width
			       :height height
			       :port port)))
    (when (sheet-grafted-p sheet)
      (realize-mirror port pixmap))
    pixmap))

;;; Invoked from 'pixmap.lisp'
(defmethod port-deallocate-pixmap ((port beagle-port) pixmap)
  (when (port-lookup-mirror port pixmap)
    (destroy-mirror port pixmap)))

;;; Currently we just use the default "cursor" (pointer pixmap), unless we do something like
;;; press the "help" key in which case Cocoa gives us a '?' pointer. Varied pointers are not
;;; yet supported in this back end.

;;; Needs implementing ::FIXME::
(defun make-cursor-table (port)
  (declare (ignore port))
  nil)

(defmethod port-frame-keyboard-input-focus ((port beagle-port) frame)
  (declare (ignore frame))
  (beagle-port-key-focus port))

(defmethod (setf port-frame-keyboard-input-focus)
    (focus (port beagle-port) frame)
  (declare (ignore frame))
  (%set-port-keyboard-focus port focus))
