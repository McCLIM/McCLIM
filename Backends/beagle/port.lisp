
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

;;; This port can't do pointer grabbing; set appropriate setting to NIL.
(setf climi::*port-can-grab* NIL)

#||

Class hierarchy - note, I'm not sure how much of the "member" data defined in basic port is defined
by the spec, or that is needed... but we have it (suspect much of it can be removed - maybe we need
both a BASIC-PORT, which really *is* basic, and a MCCLIM-PORT which is not so basic. CLX port could
inherit from the MCCLIM-PORT, and BEAGLE-PORT could inherit from BASIC-PORT. Need to think about this
some more before we start pulling McCLIM to bits!

+---------------+
|     PORT      |
+---------------+
|               |
+---------------+
^
|
 +-------------------+
 |    BASIC-PORT     |
 +-------------------+
 |server-path        |
 |properties         |
 |grafts             |
 |frame-managers     |
 |sheet->mirror      |
 |mirror->sheet      |
 |pixmap->mirror     |
 |mirror->pixmap     |
 |event-process      |
 |lock               |
 |text-style-mappings|
 |pointer-sheet      |
 +-------------------+
          ^
          |
+---------------+
|  BEAGLE-PORT   |
+---------------+
|screen         |
|color-table    |
|view-table     |
|pointer        |
|key-focus-sheet|
+---------------+

Much of the content of this file is a direct copy of the CLX backend code.

SEE ALSO McCLIM/Backends/Cocoa/notes/port.txt

||#

(defparameter *beagle-port* nil)
(defparameter *default-beagle-frame-manager* 'beagle::beagle-aqua-frame-manager
  "Specifies the frame manager that should be used by default when the port creates its
frame manager. Permissable values are 'beagle::beagle-standard-frame-manager and
'beagle::beagle-aqua-frame-manager (the default).")

;;; ::FIXME:: this holds the current cursor (mouse pointer) being used...
(defclass beagle-pointer (pointer)
  ((cursor :accessor pointer-cursor :initform :upper-left)))

(defclass standard-pointer (beagle-pointer)
  ())

(defclass beagle-cursor ()
  ((image   :accessor cursor-image   :initform nil)
   (hotspot :accessor cursor-hotspot :initform nil)))

;;; Similar to clx-port. We aren't caching designs or modifiers, and we don't have a table of
;;; cursors. Additionally we shove all the views we create into view-table, which is brain-
;;; damaged. This allows us to look up the appropriate mirror / medium when we're handling
;;; events, but the whole medium / mirror / sheet / NSView relationship thing needs rethinking
;;; in the long-term.
(defclass beagle-port (basic-port)
  ((screen          :initform nil :accessor beagle-port-screen)
   (color-table     :initform (make-hash-table :test #'eq))
   (view-table      :initform (make-hash-table :test #'equal)) ; Not looking too efficient...
   (pointer         :reader   port-pointer)
   ;; holds sheet that should receive key events.
   (key-focus-sheet :initform nil :accessor beagle-port-key-focus)
   (event-semaphore :initform nil :accessor beagle-port-event-semaphore)))

;;; mirrors parse-clx-server-path, but the cocoa server only needs to know the screen its running
;;; on (it's more like the :genera port than an X port in that respect). I'm not sure this is
;;; doing the right thing. How will we permit the user to run the McCLIM app on a secondary screen?
(defun parse-beagle-server-path (path)
  (pop path)
  (let ((screen (send (@class "NSScreen") 'MAIN-SCREEN)))
    (list :beagle :screen (getf path :screen screen))))

;;; I think it may be better to be an "openmcl" port, rather than a cocoa port. Otherwise, what
;;; will users of other Lisps under Cocoa call their ports?
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
	   (special *beagle-port* *default-beagle-frame-manager*))
  (push (make-instance *default-beagle-frame-manager* :port port) (slot-value port 'frame-managers))
  (setf (slot-value port 'pointer)
		(make-instance 'standard-pointer :port port))
  (setf *beagle-port* port)
  (initialize-beagle port))

;;; Another work-alike to the CLX/port.lisp code.
(defmethod print-object ((object beagle-port) stream)
  "Print an unreadable version of the port to the screen; useful for debugging
purposes. Note that it doesn't matter the the :screen in the beagle port is
not initialised, we'll just get \":screen NIL\" in that case."
  (print-unreadable-object (object stream :identity t :type t)
    (when (slot-boundp object 'screen)
      (format stream ":screen ~S" (beagle-port-screen object)))))


;;; = initialize-clx from CLX/port.lisp. Screen selection could be more dynamic (using
;;; the port-server-path? Not sure...) to permit the user to make use of screens other
;;; than the main screen.
(defmethod initialize-beagle ((port beagle-port))
  
  ;; CLX port gets some options here and uses those to set stuff up. We should probably do
  ;; this too, in the future ::FIXME::
  (setf (beagle-port-screen port) (send (@class "NSScreen") 'MAIN-SCREEN))

  ;; Get the application recognised as multithreaded by Cocoa - this is supposed to cause
  ;; Cocoa to do more sanity checking, though I'm not clear exactly what practical difference
  ;; it makes (if any)
  (unless (send (@class ns-thread) 'is-multi-threaded)
    (send (@class ns-thread)
		  :DETACH-NEW-THREAD-SELECTOR (get-selector-for "setHiddenUntilMouseMoves:")
		  :TO-TARGET (@class "NSCursor")
		  :WITH-OBJECT nil))
  
  (make-cursor-table port)
  (make-graft port)
  
  (setf (beagle-port-event-semaphore port) (ccl:make-semaphore))
  
;;;  (when clim-sys:*multiprocessing-p*  ; BEAGLE back end is *always* multiprocessing...
  (setf (port-event-process port)
	(clim-sys:make-process
	 (lambda ()
	   (loop
	    (with-simple-restart
	     (restart-event-loop "Restart CLIM's event loop.")
	     (loop
	      ;; process-next-event is defined in ports.lisp. It invokes
	      ;; get-next-event in the backend to actually get the next
	      ;; event in the queue
	      (process-next-event port)))))
	 :name (format nil "~S's event process." port))))


;;;(defmethod %beagle-pixel ((port beagle-port) (clim-internals::transformed-design color) &key (alpha 1.0))
;;;  (send (send (@class ns-color) :color-with-calibrated-red 0 :green 0 :blue 0 :alpha alpha) 'retain))

;;; From CLX/port.lisp
(defmethod %beagle-pixel ((port beagle-port) color &key (alpha 1.0))
  (let ((table (slot-value port 'color-table)))
    (or (gethash color table)
        (setf (gethash color table)
          (multiple-value-bind (r g b) (color-rgb color)
	    (let ((nsc (send (@class ns-color) :color-with-calibrated-red r
			     :green g
			     :blue b
			     :alpha alpha)))
	      (send nsc 'retain)
	      nsc))))))

;; Do we want the width in the PARENT coordinate system (use 'frame) or in the coordinate
;; system of the VIEW (use 'bounds)? Not sure, so just pick one for now. ::FIXME::
(defmethod port-mirror-width ((port beagle-port) sheet)
  (let ((mirror (port-lookup-mirror port sheet)))
    (slet ((frame (send mirror 'frame)))
		  (pref frame :<NSR>ect.size.width))))

(defmethod port-mirror-height ((port beagle-port) sheet)
  (let ((mirror (port-lookup-mirror port sheet)))
    (slet ((frame (send mirror 'frame)))
		  (pref frame :<NSR>ect.size.width))))

;; Hrm... check in spec. if this is correct... ::FIXME::
(defmethod graft ((port beagle-port))
  (first (port-grafts port)))

;;; This method's been added for the Beagle back end.
(defmethod port-lookup-sheet-for-view ((port beagle-port) view)
  (let ((table (slot-value port 'view-table)))
    (gethash view table)))

;;; The following are direct copies of the ones in CLX/port.lisp.
;;; They should be suitable for our purposes.
(defmethod port-allocate-pixmap ((port beagle-port) sheet width height)
  (let ((pixmap (make-instance 'mirrored-pixmap
			       :sheet sheet
			       :width width
			       :height height
			       :port port)))
    (when (sheet-grafted-p sheet)
      (realize-mirror port pixmap))
    pixmap))

(defmethod port-deallocate-pixmap ((port beagle-port) pixmap)
  (when (port-lookup-mirror port pixmap)
    (destroy-mirror port pixmap)))

;;; Currently we just use the default "cursor" (pointer pixmap), unless we do something like
;;; press the "help" key in which case cocoa gives us a '?' pointer. Varied pointers are not
;;; yet supported in this back end.

;;; Needs implementing ::FIXME::
(defun make-cursor-table (port)
  (declare (ignore port))
  nil)

