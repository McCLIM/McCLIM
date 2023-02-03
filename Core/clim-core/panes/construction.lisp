;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 1998-2001 by Michael McDonald <mikemac@mikemac.com>
;;;  (c) copyright 2000 by Iban Hatchondo <hatchond@emi.u-bordeaux.fr>
;;;  (c) copyright 2000 by Julien Boninfante <boninfan@emi.u-bordeaux.fr>
;;;  (c) copyright 2001 by Lionel Salabartan <salabart@emi.u-bordeaux.fr>
;;;  (c) copyright 2001 by Arnaud Rouanet <rouanet@emi.u-bordeaux.fr>
;;;  (c) copyright 2001-2002, 2014 by Robert Strandh <robert.strandh@gmail.com>
;;;  (c) copyright 2002-2003 by Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;  (c) copyright 2020 by Daniel Kochmański <daniel@turtleware.eu>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Implementation of the 29.2 Basic Pane Construction protocol.
;;;

(in-package #:clim-internals)

;; For each of the builtin CLIM gadgets there is an abstract gadget class and
;; at least one "concrete" subclass which can be chosen by the frame
;; manager. The CLIM 2.0 spec names one concrete class for each abstract
;; class. Frame managers need a mechanism to look up these concrete
;; classes. The current practice of the CLX backend is to search for classes
;; of various names based on the name of the abstract class. This mostly works
;; as all but two of the specified concrete class names can be produced by
;; appending "-PANE" to the abstract class name. The classes GENERIC-LIST-PANE
;; and GENERIC-OPTION-PANE break this convention.

;; I've extended the CLX frame manager to additionally search the property
;; list of the pane class name when searching for a concrete pane class. The
;; function below can be used where needed to place the concrete class name
;; where it needs to go.

;; This could be easily extended to allow mappings for specific backends..

(defun define-abstract-pane-mapping (abstract-class-name concrete-class-name)
  (setf (get abstract-class-name 'concrete-pane-class-name)
        concrete-class-name))

(defgeneric find-concrete-pane-class (pane-realizer pane-type &optional errorp)
  (:documentation "Resolves abstract pane type PANE-TYPE to a concrete
pane class. Methods defined in backends should specialize on the
PANE-REALIZER argument. When the PANE-TYPE can't be resolved NIL is
returned or error is signaled depending on the argument ERRORP.")
  (:method ((realizer t) pane-type &optional (errorp t))
    ;; Default method tries to resolve the abstract pane type
    ;; PANE-TYPE as specified by a convention mentioned in the
    ;; spec. Function is a little complicated because we preserve old
    ;; semantics adding rules to the class name resolution. Resolution
    ;; works as follows:
    ;;
    ;; 1. Abstract mapping always takes a priority. When it exists we
    ;;    don't look further.
    ;; 2. When the symbol is in clim/climi/keyword package:
    ;;    - look for a class `climi::{SYMBOL-NAME}-pane'
    ;;    - look for a class `climi::{SYMBOL-NAME}'
    ;; 3. Otherwise find a class named by the symbol.
    (check-type pane-type symbol)
    (flet ((try-mapped (symbol)
             (when-let ((mapped (get symbol 'concrete-pane-class-name)))
               (return-from find-concrete-pane-class
                 (find-class mapped errorp)))))
      (try-mapped pane-type)
      (if (let ((symbol-package (symbol-package pane-type)))
            (or (eql symbol-package (find-package '#:clim))
                (eql symbol-package (find-package '#:climi))
                (eql symbol-package (find-package '#:keyword))))
          (let* ((symbol-name (symbol-name pane-type))
                 (clim-symbol (find-symbol symbol-name '#:climi)))
            (try-mapped clim-symbol)
            (let* ((proper-name   (concatenate 'string symbol-name (string '#:-pane)))
                   (proper-symbol (find-symbol proper-name '#:climi)))
              (try-mapped proper-symbol)
              (or (and proper-symbol (find-class proper-symbol nil))
                  (and clim-symbol   (find-class clim-symbol   nil))
                  (when errorp
                    (error "Concrete class for a pane ~s not found." pane-type)))))
          (find-class pane-type errorp)))))

(defvar *pane-realizer* nil)

(defun make-pane (type &rest args)
  (apply #'make-pane-1 (or *pane-realizer*
                           (frame-manager *application-frame*))
         *application-frame* type args))

(defconstant +clim-pane-wrapper-initargs+
  '(:label :label-alignment :scroll-bars :borders))

(defconstant +space-requirement-initargs+
  '(:width :min-width :max-width :height :min-height :max-height))

(defun separate-clim-pane-initargs (initargs)
  ;; If :scroll-bars isn't a cons the user space requirement options belong to
  ;; the outermost container of the stream (scroller-pane, label-pane or
  ;; outline-pane). If :scroll-bars is a cons the user space requirement options
  ;; belong to the clim stream and it is possible to set the space requirement
  ;; of the scroller using the cdr of :scroll-bars as: :SCROLL-BARS '(:VERTICAL
  ;; :WIDTH 300) -- admich 2020-10-13
  (loop with any-wrapper-p = nil
        with complex-scroll-bars-p = nil
        for (key value) on initargs by #'cddr
        if (and (member key +space-requirement-initargs+ :test #'eq)
                (not (eq value :compute)))
          nconc (list key value) into space-options
        else if (member key +clim-pane-wrapper-initargs+)
               nconc (list key value) into wrapper-options
               and do (case key
                        (:borders
                         (when value (setf any-wrapper-p t)))
                        (:label
                         (when value (setf any-wrapper-p t)))
                        (:scroll-bars
                         (when value (setf any-wrapper-p t))
                         (when (consp value) (setf complex-scroll-bars-p t))))
        else
          nconc (list key value) into pane-options
        finally
           (return
             (if (or (not any-wrapper-p) complex-scroll-bars-p)
                 (values (append pane-options space-options) wrapper-options '())
                 (values pane-options wrapper-options space-options)))))

;;; Default is "no wrapper".
(defun wrap-clim-pane (wrapped-pane user-space-requirements
                       &key borders
                            label
                            (label-alignment :top)
                            scroll-bars)
  (let ((pane wrapped-pane))
    (when scroll-bars
      (setf pane (make-pane 'viewport-pane :contents (list pane)))
      (setf pane (apply #'make-pane 'scroller-pane
                        :contents (list pane)
                        (append
                         ;; From the Franz manual if :scroll-bars is a cons the
                         ;; car is treated as the non-cons argument and the cdr
                         ;; is a list of keyword argument pairs to be used as
                         ;; options of the scroller-pane.
                         (if (consp scroll-bars)
                             `(:scroll-bars ,@scroll-bars)
                             `(:scroll-bars ,scroll-bars))
                         (when (and user-space-requirements
                                    (not (or label borders)))
                           user-space-requirements)))))
    (when label
      (setf pane (apply #'make-pane 'label-pane
                        :label label
                        :label-alignment label-alignment
                        :contents (list pane)
                        (when (and user-space-requirements (not borders))
                          user-space-requirements))))
    (when borders
      (setf pane (apply #'make-pane 'outlined-pane
                        :thickness (if (not (numberp borders))
                                       1
                                       borders)
                        :contents (list pane)
                        user-space-requirements)))
    (values pane wrapped-pane)))

(defun make-clim-pane (type &rest options)
  (multiple-value-bind (pane-options wrapper-options wrapper-space-options)
      (separate-clim-pane-initargs options)
    (let ((pane (apply #'make-pane type pane-options)))
      (apply #'wrap-clim-pane pane wrapper-space-options wrapper-options))))

(defmethod medium-foreground ((pane pane))
  (medium-foreground (sheet-medium pane)))

(defmethod (setf medium-foreground) (ink (pane pane))
  (setf (medium-foreground (sheet-medium pane)) ink))

(defmethod medium-background ((pane pane))
  (medium-background (sheet-medium pane)))

(defmethod (setf medium-background) (ink (pane pane))
  (setf (medium-background (sheet-medium pane)) ink))

(defmethod compose-space ((pane pane) &key (width 100) (height 100))
  (make-space-requirement :width width :height height))

(defmethod allocate-space ((pane pane) width height)
  (resize-sheet pane width height))

(defmethod pane-needs-redisplay ((pane pane))
  (let ((do-redisplay (pane-redisplay-needed pane)))
    (values do-redisplay
            (and do-redisplay (not (eq do-redisplay :no-clear))))))

(defmethod (setf pane-needs-redisplay) (value (pane pane))
  (setf (pane-redisplay-needed pane) value))

(defmethod window-clear ((pane pane))
  nil)

(defclass basic-pane (standard-space-requirement-options-mixin
                      sheet-parent-mixin ;mirrored-sheet-mixin
                      ;; UX mixins
                      always-repaint-background-mixin
                      mouse-wheel-scroll-mixin
                      permanent-medium-sheet-output-mixin
                      clim-repainting-mixin
                      clim-sheet-input-mixin
                      sheet-transformation-mixin
                      layout-protocol-mixin
                      pane
                      basic-sheet)
  ((name              :initarg :name
                      :reader pane-name
                      :initform nil)
   ;; Context
   (port              :initarg :port)
   (manager           :initarg :manager)
   (frame             :initarg :frame
                      :initform *application-frame*
                      :reader pane-frame)
   ;; Drawing defaults
   (foreground        :initarg :foreground
                      :reader pane-foreground
                      :reader foreground)
   (background        :initarg :background
                      :accessor pane-background
                      :reader background)
   (text-style        :initarg :text-style
                      :reader pane-text-style
                      :type text-style)
   ;; Display state
   (redisplay-needed  :accessor pane-redisplay-needed
                      :initarg :redisplay-needed :initform nil))
  (:default-initargs
   :foreground +black+
   :background *3d-normal-color*
   :text-style *default-text-style*))

;;; XXX this is for code ported from other CLIM implementations that seem to
;;; allow text-style being passed as a cons. -- jd 2022-09-16
(defmethod initialize-instance :before ((obj basic-pane) &key text-style)
  (check-type text-style text-style))

(defmethod print-object ((object basic-pane) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (prin1 (pane-name object) stream)))

(defmethod engraft-medium :after (medium port (pane basic-pane))
  (declare (ignore port))
  ;; implements 29.2.2, last sentence.
  (setf (medium-foreground medium) (pane-foreground pane)
        (medium-background medium) (pane-background pane)
        (medium-text-style medium) (pane-text-style pane)))

(defmethod handle-event ((sheet basic-pane) (event window-map-event))
  (setf (sheet-enabled-p sheet) t))

(defmethod handle-event ((sheet basic-pane) (event window-unmap-event))
  (setf (sheet-enabled-p sheet) nil))

(defmethod handle-repaint :around ((sheet basic-pane) region)
  (letf (((medium-background sheet) (pane-background sheet))
         ((medium-foreground sheet) (pane-foreground sheet)))
    (call-next-method)))
