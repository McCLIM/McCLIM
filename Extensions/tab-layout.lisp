;;; -*- Mode: Lisp; show-trailing-whitespace: t; indent-tabs: nil; -*-

;;; Based on the tab-layout by:
;;; ---------------------------------------------------------------------------
;;;     Title: A Tab Layout Pane
;;;   Created: 2005/09/16-19
;;;    Author: Max-Gerd Retzlaff <m.retzlaff@gmx.net>, http://bl0rg.net/~mgr
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 2005 by Max-Gerd Retzlaff
;;;
;;; Available from:
;;;   http://bl0rg.net/~mgr/flux/tab-layout_2005-09-19_02-52+0200.tar.bz2
;;;
;;; License given on IRC:
;;;   http://tunes.org/~nef/logs/lisp/07.01.15
;;; 04:04:49 <mgr> _8work: the license will not be a problem. not with me, not
;;;  with Gilbert. BSD or LGPL, or both.  but I'm on the move.. see you later
;;; 04:05:22 <mgr> _8work: in fact, I wanted to commit it to mcclim long time
;;;  ago, but I have not yet because there seemed to be a lack of interest.

;;; Based on the stack layout by:
;;; ---------------------------------------------------------------------------
;;;     Title: Embryo Stack Layout Pane Class
;;;   Created: 2003-06-01
;;;    Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;   License: As public domain as it can get.
;;; ---------------------------------------------------------------------------
;;; Available from:
;;;   http://bauhh.dyndns.org:8000/mcclim/cookbook/

;;; ---------------------------------------------------------------------------
;;; Adapted for inclusion into McCLIM:
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 2006 David Lichteblau

(in-package :clim-tab-layout)


;;; abstract TAB-LAYOUT superclass

(climi::define-abstract-pane-mapping 'tab-layout 'tab-layout-pane)

(defclass tab-layout (climi::composite-pane)
    ((pages :initform nil :reader tab-layout-pages :initarg :pages)
     (enabled-page :initform nil :accessor tab-layout-enabled-page))
  (:documentation "The abstract tab layout pane is a composite pane arranging
its children so that exactly one child is visible at any time, with a row of
buttons allowing the user to choose between them.  Use WITH-TAB-LAYOUT to
define a tab layout and its children, or use the :PAGES argument
to specify its contents when creating it dynamically using MAKE-PANE."))

(defmethod initialize-instance :after ((instance tab-layout) &key pages)
  (when (eq (class-of instance) (find-class 'tab-layout))
    (error "tab-layout is an abstract class, you cannot instantiate it!"))
  (dolist (page pages)
    (setf (tab-page-tab-layout page) instance)
    (sheet-adopt-child instance (tab-page-pane page)))
  (setf (tab-layout-enabled-page instance) (car pages)))

(defclass tab-page ()
  ((tab-layout :initform nil :accessor tab-page-tab-layout)
   (title :initform nil :accessor tab-page-title :initarg :title)
   (pane :initform nil :accessor tab-page-pane :initarg :pane)
   (presentation-type :initform 'tab-page
		      :accessor tab-page-presentation-type
		      :initarg :presentation-type)
   (enabled-callback :initform nil
		     :accessor tab-page-enabled-callback
		     :initarg :enabled-callback)
   ;; fixme: drawing-options in this generality are a feature of the old
   ;; concrete tab pane.  Gtkairo will only look for the :INK in this list.
   (drawing-options :initform nil
		    :accessor tab-page-drawing-options
		    :initarg :drawing-options))
  (:documentation "Instances of TAB-PAGE represent the pages in a TAB-LAYOUT.
For each child pane, there is a TAB-PAGE providing the page's title and
additional information about the child.  Valid initialization arguments
are :TITLE, :PANE (required) and :PRESENTATION-TYPE,:DRAWING-OPTIONS
(optional)."))

(defmethod print-object ((object tab-page) stream)
  (print-unreadable-object (object stream :identity t :type t)
    (princ (tab-page-title object) stream)))

(defgeneric tab-layout-pages (tab-layout)
  (:documentation "Return all TAB-PAGEs in this tab layout, in order
from left to right.  Do not modify the resulting list destructively.
Use the SETF function of the same name to assign a new list of pages.
The SETF function will automatically add tabs for new page objects, remove
old pages, and reorder the pages to conform to the new list."))

(defgeneric tab-layout-enabled-page (tab-layout)
  (:documentation
   "The currently visible tab page of this tab-layout, or NIL if the tab
layout does not have any pages currently. Use the SETF function of the name
to change focus to another tab page."))

(defgeneric tab-page-tab-layout (tab-page)
  (:documentation "Return the TAB-LAYOUT this page belongs to."))

(defgeneric tab-page-pane (tab-page)
  (:documentation "Return the CLIM pane this page displays.  See also
SHEET-TO-PAGE, the reverse operation."))

(defgeneric tab-page-title (tab-page)
  (:documentation "Return the title displayed in the tab for this PAGE.
Use the SETF function of the same name to set the title dynamically."))

(defgeneric tab-page-presentation-type (tab-page)
  (:documentation "Return the type of the presentation used when this
page's header gets clicked.  Use the SETF function of the same name to
set the presentation type dynamically.  The default is TAB-PAGE."))

(defgeneric tab-page-drawing-options (tab-page)
  (:documentation "Return the drawing options of this page's header.  Use
the SETF function of the same name to set the drawing options dynamically.
Note: Not all implementations of the tab layout will understand all drawing
options.  In particular, the Gtkairo backends understands only the :INK
option at this time."))

(defgeneric (setf tab-layout-enabled-page) (newval tab-layout))

(defgeneric note-tab-page-changed (layout page)
  (:documentation "This internal function is called by the SETF methods
for TAB-PAGE-TITLE and -DRAWING-OPTIONS to inform the page's tab-layout
about the changes, allowing it to update its display.  Only called by
the TAB-LAYOUT implementation and specialized by its subclasses."))

(defmethod (setf tab-layout-enabled-page) :around (page (parent tab-layout))
  ;; As a rule, we always want exactly one enabled page -- unless we
  ;; don't have any pages at all.
  (assert (or page (null (tab-layout-pages parent))))
  ;; This must be an around method, so that we can see the old value, yet
  ;; do the call only after the change has been done:
  (let ((old-page (tab-layout-enabled-page parent)))
    (prog1
	(call-next-method)
      (when (and page (not (equal page old-page)))
	(note-tab-page-enabled page)))))

(defmethod (setf tab-layout-pages) (newval (parent tab-layout))
  (unless (equal newval (remove-duplicates newval))
    (error "page list must not contain duplicates: ~A" newval))
  (let* ((oldval (tab-layout-pages parent))
	 (add (set-difference newval oldval))
	 (remove (set-difference oldval newval)))
    ;; check for errors
    (dolist (page add)
      (unless (null (tab-page-tab-layout page))
	(error "~A has already been added to a different tab layout" page)))
    ;; remove old pages first, because sheet-disown-child still needs access
    ;; to the original page list:
    (dolist (page remove)
      (sheet-disown-child parent (tab-page-pane page)))
    ;; install the pages before adding their sheets (matters for gtkairo)
    (setf (slot-value parent 'pages) newval)
    ;; add new pages:
    (dolist (page add)
      (setf (tab-page-tab-layout page) parent)
      (sheet-adopt-child parent (tab-page-pane page)))))

(defmethod sheet-disown-child :before ((parent tab-layout) child &key errorp)
  (declare (ignore errorp))
  (unless (internal-child-p child parent)
    (let* ((page (sheet-to-page child))
	   (current-page (tab-layout-enabled-page parent))
	   (currentp (equal child (tab-page-pane current-page)))
	   (successor
	    (when currentp
	      (page-successor current-page))))
      (setf (slot-value parent 'pages) (remove page (tab-layout-pages parent)))
      (when currentp
	(setf (tab-layout-enabled-page parent) successor))
      (setf (tab-page-tab-layout page) nil))))

(defun sheet-to-page (sheet)
  "For a SHEET that is a child of a tab layout, return the page corresponding
to this sheet.  See also TAB-PAGE-PANE, the reverse operation."
  (find sheet (tab-layout-pages (sheet-parent sheet)) :key #'tab-page-pane))

(defun find-tab-page-named (name tab-layout)
  "Find the tab page with the specified TITLE in TAB-LAYOUT.
Note that uniqueness of titles is not enforced; the first page found will
be returned."
  (find name
	(tab-layout-pages tab-layout)
        :key #'tab-page-title
	;; fixme: don't we want the case-sensitive STRING= here?
	:test #'string-equal))

(defmethod (setf tab-page-title) :after (newval (page tab-page))
  (declare (ignore newval))
  (let ((layout (tab-page-tab-layout page)))
    (when layout
      (note-tab-page-changed layout page))))

(defmethod (setf tab-page-drawing-options) :after (newval (page tab-page))
  (declare (ignore newval))
  (let ((layout (tab-page-tab-layout page)))
    (when layout
      (note-tab-page-changed layout page))))

(defmethod note-tab-page-changed ((layout tab-layout) page)
  nil)

;;; GTK+ distinguishes between children user code creates and wants to
;;; see, and "internal" children the container creates and mostly hides
;;; from the user.  Let's steal that concept to ignore the header pane.
(defgeneric internal-child-p (child parent))

(defmethod internal-child-p (child (parent tab-layout))
  nil)

(defun page-successor (page)
  "The page we should enable when PAGE is currently enabled but gets removed."
  (loop for (a b c) on (tab-layout-pages (tab-page-tab-layout page)) do
	(cond
	  ((eq a page) (return b))
	  ((eq b page) (return (or c a))))))

(defun note-tab-page-enabled (page)
  (let ((callback (tab-page-enabled-callback page)))
    (when callback
      (funcall callback page))))


;;; convenience functions:

(defun add-page (page tab-layout &optional enable)
  "Add PAGE at the left side of TAB-LAYOUT.  When ENABLE is true, move focus
to the new page.  This function is a convenience wrapper; you can also
push page objects directly into TAB-LAYOUT-PAGES and enable them using
(SETF TAB-LAYOUT-ENABLED-PAGE)."
  (push page (tab-layout-pages tab-layout))
  (when enable
    (setf (tab-layout-enabled-page tab-layout) page)))

(defun switch-to-page (page)
  "Move the focus in page's tab layout to this page.  This function
is a one-argument convenience version of (SETF TAB-LAYOUT-ENABLED-PAGE), which
can also be called directly."
  (setf (tab-layout-enabled-page (tab-page-tab-layout page)) page))

(defun remove-page (page)
  "Remove PAGE from its tab layout.  This is a convenience wrapper around
SHEET-DISOWN-CHILD, which can also be used directly to remove the page's
pane with the same effect."
  (sheet-disown-child (tab-page-tab-layout page)
		      (tab-page-pane page)))

(defun remove-page-named (title tab-layout)
  "Remove the tab page with the specified TITLE from TAB-LAYOUT.
Note that uniqueness of titles is not enforced; the first page found will
be removed.  This is a convenience wrapper, you can also use
FIND-TAB-PAGE-NAMED to find and the remove a page yourself."
  (remove-page (find-tab-page-named title tab-layout)))


;;; creation macro

(defmacro with-tab-layout ((default-presentation-type &rest initargs
			     &key name &allow-other-keys)
			   &body body)
  "Return a TAB-LAYOUT.  Any keyword arguments, including its name, will be
passed to MAKE-PANE.  Child pages of the TAB-LAYOUT can be specified using
BODY, using lists of the form (TITLE PANE &KEY PRESENTATION-TYPE
DRAWING-OPTIONS ENABLED-CALLBACK).  DEFAULT-PRESENTATION-TYPE will be passed
as :PRESENTATION-TYPE to pane creation forms that specify no type themselves."
  (let ((ptypevar (gensym)))
    `(let ((,ptypevar ,default-presentation-type))
       (make-pane 'tab-layout
		  :name ,(or name `',(gensym "tab-layout-"))
		  :pages (list ,@(mapcar (lambda (spec)
					   `(make-tab-page ,@spec
							   :presentation-type
					     ,ptypevar))
					 body))
		  ,@initargs))))

(defun make-tab-page
    (title pane &key presentation-type drawing-options enabled-callback)
  (make-instance 'tab-page
    :title title
    :pane pane
    :presentation-type presentation-type
    :drawing-options drawing-options
    :enabled-callback enabled-callback))


;;; presentation/command system integration

(define-command (com-switch-to-tab-page
		 :command-table clim:global-command-table)
    ((page 'tab-page :prompt "Tab page"))
  (switch-to-page page))

(define-presentation-to-command-translator switch-via-tab-button
    (tab-page com-switch-to-tab-page clim:global-command-table
	      :gesture :select
	      :documentation "Switch to this page"
	      :pointer-documentation "Switch to this page")
    (object)
  (list object))

(define-command (com-remove-tab-page :command-table clim:global-command-table)
    ((page 'tab-page :prompt "Tab page"))
  (remove-page page))


;;; generic TAB-LAYOUT-PANE implementation

(defclass tab-bar-view (gadget-view)
  ())

(defparameter +tab-bar-view+ (make-instance 'tab-bar-view))

(define-presentation-method present
    (tab-page (type tab-page) stream (view tab-bar-view) &key)
  (stream-increment-cursor-position stream 5 0)
  (multiple-value-bind (x y) (stream-cursor-position stream)
    (let* ((length-top-line
	    (+ x 6 (text-size stream (tab-page-title tab-page)) 3))
           (tab-button-polygon
	    (list x (+ y 14)   (+ x 6) y
		  (+ x 6) y   length-top-line y
		  length-top-line y   (+ length-top-line 6) (+ y 14))))

      ;; grey-filled polygone for the disabled panes
      (unless (sheet-enabled-p (tab-page-pane tab-page))
        (draw-polygon* stream tab-button-polygon :ink +grey+))

      ;; black non-filled polygon
      (draw-polygon* stream tab-button-polygon :ink +black+ :filled nil)

      ;; "breach" the underline for the enabled pane
      (when (sheet-enabled-p (tab-page-pane tab-page))
        (draw-line stream
		   (apply #'make-point (subseq tab-button-polygon 0 2))
		   (apply #'make-point
			  (subseq tab-button-polygon
				  (- (length tab-button-polygon) 2)))
		   :ink +background-ink+))))

  (stream-increment-cursor-position stream 8 0)
  (apply #'invoke-with-drawing-options stream
         (lambda (rest)
           (declare (ignore rest))
           (write-string (tab-page-title tab-page) stream))
         (tab-page-drawing-options tab-page))
  (stream-increment-cursor-position stream 10 0))

(defclass tab-layout-pane (tab-layout)
    ((header-pane :accessor tab-layout-header-pane
		  :initarg :header-pane)
     (header-display-function
      :accessor header-display-function
      :initarg :header-display-function
      :initform 'default-display-tab-header))
  (:documentation "A pure-lisp implementation of the tab-layout, this is
the generic implementation chosen by the CLX frame manager automatically.
Users should create panes for type TAB-LAYOUT, not TAB-LAYOUT-PANE, so
that the frame manager can customize the implementation."))

(defmethod (setf tab-layout-enabled-page)
    (page (parent tab-layout-pane))
  (let ((old-page (tab-layout-enabled-page parent)))
    (unless (equal page old-page)
      (when old-page
	(setf (sheet-enabled-p (tab-page-pane old-page)) nil))
      (when page
	(setf (sheet-enabled-p (tab-page-pane page)) t)))
    (when page
	(setf (sheet-enabled-p (tab-page-pane page)) t)))
  (call-next-method))

(defun default-display-tab-header (tab-layout pane)
  (stream-increment-cursor-position pane 0 3)
  (draw-line* pane
	      0
	      17
	      (slot-value pane 'climi::current-width)
	      17
	      :ink +black+)
  (mapc (lambda (page)
	  (with-output-as-presentation
	      (pane (tab-page-pane page)
		    (tab-page-presentation-type page))
	    (present page 'tab-page :stream pane)))
	(tab-layout-pages tab-layout)))

(defclass tab-bar-pane (application-pane)
  ()
  (:default-initargs :default-view +tab-bar-view+))

(defmethod compose-space ((pane tab-bar-pane) &key width height)
  (declare (ignore width height))
  (make-space-requirement :min-height 22 :height 22 :max-height 22))

(defmethod initialize-instance :after ((instance tab-layout-pane) &key pages)
  (let ((current (tab-layout-enabled-page instance)))
    (dolist (page pages)
      (setf (sheet-enabled-p (tab-page-pane page)) (eq page current))))
  (let ((header
	 (make-pane 'tab-bar-pane
	  :display-time :command-loop
	  :display-function
	  (lambda (frame pane)
	    (declare (ignore frame))	    
	    (funcall (header-display-function instance) instance pane)))))
    (setf (tab-layout-header-pane instance) header)
    (sheet-adopt-child instance header)
    (setf (sheet-enabled-p header) t)))

(defmethod compose-space ((pane tab-layout-pane) &key width height)
  (declare (ignore width height))
  (let ((q (compose-space (tab-layout-header-pane pane))))
    (space-requirement+*
     (reduce (lambda (x y)
	       (space-requirement-combine #'max x y))
	     (mapcar #'compose-space (sheet-children pane))
	     :initial-value
	     (make-space-requirement :width 0 :min-width 0 :max-width 0
				     :height 0 :min-height 0 :max-height 0))
     :height (space-requirement-height q)
     :min-height (space-requirement-min-height q)
     :max-height (space-requirement-max-height q))))

(defmethod allocate-space ((pane tab-layout-pane) width height)
  (let* ((header (tab-layout-header-pane pane))
	 (y (space-requirement-height (compose-space header))))
    (move-and-resize-sheet header 0 0 width y)
    (allocate-space header width y)
    (dolist (page (tab-layout-pages pane))
      (let ((child (tab-page-pane page)))
	(move-and-resize-sheet child 0 y width (- height y))
	(allocate-space child width (- height y))))))

(defmethod internal-child-p (child (parent tab-layout-pane))
  (eq child (tab-layout-header-pane parent)))

(defmethod clim-tab-layout:note-tab-page-changed
    ((layout tab-layout-pane) page)
  (redisplay-frame-pane (pane-frame layout)
			(tab-layout-header-pane layout)
			#+NIL
			(car (sheet-children
			      (car (sheet-children
				    (tab-layout-header-pane layout)))))
			:force-p t))
