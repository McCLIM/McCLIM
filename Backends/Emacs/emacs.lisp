;;; emacs.lisp -- Experimental McCLIM backend for Emacs (via SLIME)
;;;
;;; This file provides a backend to render McCLIM graphics in Emacs via SLIME.
;;; CLIM drawing operations are mapped onto an SVG canvas and shipped to Emacs
;;; via socket connection.
;;;
;;; This is all in one file for the moment because I find that I get lost
;;; otherwise.
;;;
;;; - [X] Define Emacs backend with SVG medium
;;; - [X] Display results in SLIME REPL
;;; - [ ] Drawing operations
;;;   - [X] Rectangle
;;;   - [X] Polygon
;;;   - [ ] Circle
;;;   - [X] Ellipse
;;;   - [ ] Line
;;;   - [ ] Points
;;;   - [ ] Text
;;; - [ ] Support mouse events
;;; - [ ] Support mouse-sensitive regions for accepting input
;;; - [ ] Commands
;;; - [ ] Menus
;;; - [ ] ... what else?

(in-package #:common-lisp-user)
(defpackage #:clim-emacs
  (:use #:clim #:climi #:clime #:climb #:clim-lisp)
  (:import-from #:climi #:left #:right #:top #:bottom
                #:filled #:ink
                #:center-x #:center-y
                #:radius-1-dx #:radius-1-dy
                #:radius-2-dx #:radius-2-dy
                #:draw-rectangle-output-record #:draw-rectangles-output-record
                #:draw-ellipse-output-record
                #:draw-polygon-output-record #:draw-text-output-record
                #:draw-point-output-record #:draw-points-output-record
                #:draw-line-output-record #:draw-lines-output-record))



(in-package #:clim-emacs)
(declaim (optimize (debug 3) (safety 3) (speed 1)))


;;;; Port

(defvar *emacs-command-table* (make-command-table "Emacs"))

(defclass emacs-port (basic-port)
  ((id)))

(defmethod find-port-type ((type (eql :emacs)))
  (values 'emacs-port 'identity))

(defmethod initialize-instance :after ((port emacs-port) &rest initargs)
  (declare (ignore initargs))
  (setf (slot-value port 'id) (gensym "EMACS-PORT-")))


;;;; Medium

(defclass emacs-medium (basic-medium)
  ())

(defmethod make-medium ((port emacs-port) sheet)
  (make-instance 'emacs-medium :port port :sheet sheet))

(defmethod medium-draw-rectangle* ((medium emacs-medium) x1 y1 x2 y2 filled))
(defmethod medium-draw-polygon* ((medium emacs-medium) coord-seq closed filled))
(defmethod medium-draw-ellipse* ((medium emacs-medium) cx cy r1dx r1dy r2dx r2dy sa ea filled))
(defmethod medium-draw-text* ((medium emacs-medium) string x y start end align-x align-y toward-x toward-y transform-glyphs))
(defmethod medium-draw-line* ((medium emacs-medium) x1 y1 x2 y2))
(defmethod medium-draw-lines* ((medium emacs-medium) coord-seq))
(defmethod medium-draw-point* ((medium emacs-medium) x y))
(defmethod medium-draw-points* ((medium emacs-medium) coord-seq))

;;;; Text

;; FIXME We don't provide an implementation of `text-style-mapping`,
;;  and perhaps we don't want to - jqs 2020-05-08

(defparameter *svg-text-size-map*
  '(:tiny "xx-small"
    :very-small "x-small"
    :small "small"
    :normal "medium"
    :large "large"
    :very-large "x-large"
    :huge "xx-large"
    :smaller "smaller"
    :larger "larger"))

(defun clim-to-svg-size (size)
  (etypecase size
    (symbol (getf *svg-text-size-map* size "medium"))
    (real (format nil "~Fpt" size))))

(defparameter *svg-text-family-map*
  '(:fix "monospace"
    :serif "serif"
    :sans-serif "sans-serif"))

(defun clim-to-svg-family (family)
  (getf *svg-text-family-map* family "serif"))

(defun clim-to-svg-face (face)
  (cond ((or (eq :roman face)
             (eq nil face))
         "font-style:normal")
        ((eq :bold face)
         "font-weight:bold")
        ((eq :italic face)
         "font-style:italic")
        ((equal '(:bold :italic) face)
         "font-style:italic;font-weight:bold")
        ;; assume '(:italic :bold) has been normalised to the above - jqs 2020-05-08
        (t (error "Unknown face: ~S" face))))

(defun svg-text-style (text-style)
  (let ((family (text-style-family text-style))
        (face (text-style-face text-style))
        (size (text-style-size text-style)))
    (format nil "font-family:~A;~A;font-size:~A"
            (clim-to-svg-family family)
            (clim-to-svg-face face)
            (clim-to-svg-size size))))

(defun text-to-svg (string text-style)
  ;; FiXME We include "xml:space = preserve" to avoid whitespace being collapsed.
  ;;  This might mean that multi-line text will get a bounding-rectangle using
  ;;  the libsvg layout algorithm, but a `text-size` using a different
  ;;  layout algorithm. Perhaps it doesn't matter, and this just
  ;;  shouldn't be called with multi-line text - jqs 2020-05-08
  (format nil "<svg xmlns='http://www.w3.org/2000/svg'><text style='~A;' xml:space='preserve'><tspan x='0' y='0'><tspan>~A</tspan></tspan></text></svg>"
          (svg-text-style text-style)
          string))

(defmethod climb:text-bounding-rectangle* ((medium emacs-medium) string
                                           &key text-style start end align-x align-y direction)
  (declare (ignore align-x align-y direction))
  (let* ((sub (subseq string (or start 0) (or end (length string))))
         (text-style (or text-style (medium-text-style medium)))
         (svg-data (text-to-svg string text-style))
         (image-size (svg-image-size svg-data)))
    (values 0 0 (car image-size) (cdr image-size))))

(defun svg-image-size (svg-data)
  (swank:ed-rpc 'svg-image-size svg-data))

(defmethod text-size ((medium emacs-medium) string &key text-style (start 0) end)
  (let* ((string (string string))
         (text-style (or text-style (medium-text-style medium)))
         (end (or end (length string)))
         (line-height (text-style-height text-style medium))
         (total-height 0)
         (width 0)
         (max-width 0))
    (climi::dolines (line (subseq string start end)
                          (values max-width total-height
                                  width (- total-height line-height)
                                  (- total-height (text-style-descent text-style medium))))
      (setf width (if (zerop (length line))
                      0
                      (car (svg-image-size (text-to-svg line text-style)))))
      (incf total-height line-height)
      (alexandria:maxf max-width width))))

;; FIXME - hacky, but perhaps we don't want to bother with real text metrics - jqs 2020-05-08

(defparameter *text-style-metrics-cache*
  (make-hash-table :test #'equal)
  "A hash-table, the KEYs of which are lists (family face size), and the VALUES of which are
four-element vector: width, height, ascent, descent")

(defun tsmetric->index (metric)
  (ecase metric
    (:width 0)
    (:height 1)
    (:ascent 2)
    (:descent 3)))

(defun make-text-style-metrics-cache-entry ()
  (make-array 4 :initial-element nil))

(defun text-style-metrics-cache-key (text-style)
  (list (text-style-family text-style)
        (text-style-face text-style)
        (text-style-size text-style)))

(defun get-text-style-metric (text-style metric if-not-found)
  (let ((key (text-style-metrics-cache-key text-style))
        (index (tsmetric->index metric)))
    (multiple-value-bind (entry foundp)
        (gethash key *text-style-metrics-cache*)
      (if foundp
          (alexandria:if-let ((val (svref entry index)))
            val
            (setf (svref entry index) (funcall if-not-found)))
          (let ((entry (make-text-style-metrics-cache-entry)))
            (prog1 (setf (svref entry index) (funcall if-not-found))
              (setf (gethash key *text-style-metrics-cache*) entry)))))))

(defun text-style-base (text-style)
  (svg-image-size (text-to-svg "M" text-style)))

(defmethod text-style-width ((text-style standard-text-style) (medium emacs-medium))
  (get-text-style-metric text-style
                         :width
                         #'(lambda () (car (text-style-base text-style)))))

(defmethod text-style-ascent ((text-style standard-text-style) (medium emacs-medium))
  (get-text-style-metric text-style
                         :ascent
                         #'(lambda () (cdr (svg-image-size (text-to-svg "A" text-style))))))

(defmethod text-style-descent ((text-style standard-text-style) (medium emacs-medium))
  (get-text-style-metric text-style
                         :descent
                         #'(lambda () (- (cdr (svg-image-size (text-to-svg "y" text-style)))
                                         (cdr (svg-image-size (text-to-svg "v" text-style)))))))

(defmethod text-style-height ((text-style standard-text-style) (medium emacs-medium))
  (get-text-style-metric text-style
                         :height
                         #'(lambda () (+ (text-style-ascent text-style medium)
                                         (text-style-descent text-style medium)))))

(defmethod text-style-fixed-width-p ((text-style standard-text-style) (medium emacs-medium))
  (eq :fix (text-style-family text-style)))

;;;; Lines

(defun svg-stroke-unit (line-style)
  (ecase (line-style-unit line-style)
    ((:normal :coordinate) "px") ;; ignore scaling voodoo
    (:point "pt")))

(defun svg-stroke-width (line-style)
  (format nil "~F~A"
          (line-style-thickness line-style)
          (svg-stroke-unit line-style)))

(defun svg-stroke-line-join (line-style)
  (ecase (line-style-joint-shape line-style)
    (:miter "miter")
    (:bevel "bevel")
    (:round "round")
    (:none nil)))

(defun svg-stroke-line-cap (line-style)
  (ecase (line-style-cap-shape line-style)
    (:butt "butt")
    (:square "square")
    (:round "round")
    (:no-end-point nil))) ; FIXME ??

(defun svg-stroke-dasharray (line-style)
  (let ((dashes (line-style-dashes line-style)))
    (cond ((null dashes)
           nil)
          ((eq t dashes)
           "3 3")
          ((listp dashes)
           (format nil "~{~D~^ ~}" dashes))
          ((vectorp dashes)
           (format nil "~{~D~^ ~}" (coerce dashes 'list)))
          (t
           (error "Unknown line-dash entry: ~S" dashes)))))

;;;; Stream

(defclass clim-emacs-stream (sheet-leaf-mixin
                             sheet-parent-mixin
                             sheet-transformation-mixin
                             sheet-mute-input-mixin
                             sheet-mute-repainting-mixin
                             climi::updating-output-stream-mixin
                             basic-sheet
                             standard-extended-output-stream
                             extended-input-stream
                             permanent-medium-sheet-output-mixin
                             standard-output-recording-stream)
  ((port :initform nil :initarg port :accessor port)))
  
(defmacro with-output-to-emacs ((stream-var) &body body)
  (let ((cont (gensym)))
    `(flet ((,cont (,stream-var)
              ,@body))
       (declare (dynamic-extent #',cont))
       (invoke-with-output-to-emacs-stream #',cont))))

(defun invoke-with-output-to-emacs-stream (continuation)
  (with-port (port :emacs)
    (let ((stream (make-instance 'clim-emacs-stream :port port)))
      (sheet-adopt-child (find-graft :port port) stream)
      (prog1 (funcall continuation stream)
        (let ((output (stream-output-history stream)))
          (swank::send-to-emacs (list :write-clime
                                      (output-record-to-svg output)
                                      (presentations-for-emacs stream))))))))

;; FIXME - for some reason CLIM acts as if we have an absurdly small right margin.
;;  For now this can be used in a `with-temporary-margins` call until I work
;;  out how to use it when initializing the stream - jqs 2020-05-08
(defun emacs-right-margin ()
  (swank:ed-rpc 'window-width-for-margin))

(defun output-record-to-svg (record)
  (multiple-value-bind (x-min y-min x-max y-max) (bounding-rectangle* record)
    (let ((width  (ceiling (- x-max x-min)))
          (height (ceiling (- y-max y-min))))
      (shapes-to-svg (output-history-shapes record) width height))))

(defun presentations-for-emacs (stream)
  (let (ids)
    (multiple-value-bind (x0 y0) (bounding-rectangle* (stream-output-history stream))
      (labels ((visit (record)
                 (when (typep record 'presentation)
                   (push (list (register-presentation record)
                               (emacs-map-area record x0 y0)
                               (tooltip record))
                         ids))
                 (map-over-output-records #'visit record)))
        (visit (stream-output-history stream))))
    ids))

(defun emacs-map-area (record x0 y0)
  (multiple-value-bind (x1 y1 x2 y2) (bounding-rectangle* record)
    ;; Syntax follows https://www.gnu.org/software/emacs/manual/html_node/elisp/Image-Descriptors.html
    (let ((left   (floor (- x1 x0)))
          (top    (floor (- y1 y0)))
          (right  (ceiling (- x2 x0)))
          (bottom (ceiling (- y2 y0))))
      (cons '#:rect (cons (cons left top) (cons right bottom))))))


;;;; Presentations and input context

(defvar *presentations* (make-array 0 :adjustable t :fill-pointer 0)
  "Vector of presentations (identified by index.)")

(defun register-presentation (presentation)
  (vector-push-extend presentation *presentations*))

#+swank
(defmethod stream-accept ((stream swank/gray::slime-input-stream) type &rest keywords)
  (declare (ignore keywords))
  (presentation-object (elt *presentations*
                            (swank:clime-accept-in-emacs (acceptable-presentations type)))))

(defun acceptable-presentations (presentation-type)
  (loop for index from 0
        for presentation across *presentations*
        when (presentation-typep (presentation-object presentation) presentation-type)
          collect index))


;;;; Tooltips

(defgeneric tooltip (presentation)
  (:documentation "Return a tooltip string describing PRESENTATION.")
  (:method ((p presentation))
    (with-output-to-string (s)
      (let ((*print-right-margin* 60))
        (cl:describe (presentation-object p) s)))))


;;;; Output records

(defvar *debug-output-tree* nil
  "Most recently processed output tree, for debugging purposes.")

;;; Convert McCLIM's internal output record format into a simple list
;;; representation with (0,0) as the upper-left corner.

(defun output-history-shapes (root)
  "Return the list of shapes in the output history rooted at ROOT."
  (setf *debug-output-tree* root)
  (let (shapes)
    (multiple-value-bind (x-min y-min x-max y-max) (bounding-rectangle* root)
      (assert (<= x-min x-max))
      (assert (<= y-min y-max))
      (map-over-output-record-tree (lambda (record)
                                     (push (output-record-to-list record x-min y-min) shapes))
                                   root)
      (remove nil shapes))))

(defun map-over-output-record-tree (fn record)
  "Call FN on RECORD and all descendents of RECORD."
  (flet ((visit (child)
           (map-over-output-record-tree fn child)))
  (funcall fn record)
  (map-over-output-records #'visit record)))
  
(defun output-record-to-list (record &optional (x-min 0) (y-min 0))
  "Return a simple list representation of an output record.
   Optionally translate coordinates relative to the given origin.
   Return NIL if RECORD is not a recognized shape-drawing output record."
  (labels ((x (x) (- x x-min))   ;; Translate top-left corner to (0,0)
           (y (y) (- y y-min))
           (xsys (seq)
             (loop for i below (length seq)
                   if (oddp i) collect (x (elt seq i))
                     else collect (y (elt seq i))))
           (rects (seq)
             (loop for i below (length seq) by 4
                   for left = (elt seq i)
                   for top = (elt seq (+ 1 i))
                   for right = (elt seq (+ 2 i))
                   for bottom = (elt seq (+ 3 i))
                   collect (min left right)
                   collect (min bottom right)
                   collect (abs (- right left))
                   collect (abs (- bottom top)))))
    (typecase record
      (draw-rectangle-output-record
       (with-slots (left top right bottom filled line-style ink) record
         ;; NB: left/right and top/bottom positions aren't dependable
         (list :rectangle
               (x (min left right))
               (y (min bottom top))
               (abs (- right left))
               (abs (- bottom top))
               filled
               line-style
               ink)))
      (draw-rectangles-output-record
       (with-slots (climi::coord-seq filled line-style ink) record
         (list :rectangles (rects climi::coord-seq) filled line-style ink)))
      (draw-ellipse-output-record
       (with-slots (center-x center-y radius-1-dx radius-1-dy radius-2-dx radius-2-dy
                    climi::start-angle climi::end-angle filled line-style ink)
           record
         (flet ((distance (x y)
                  (sqrt (+ (expt x 2) (expt y 2)))))
           (list :ellipse (x center-x) (y center-y)
                 (distance radius-1-dx radius-1-dy)
                 (distance radius-2-dx radius-2-dy)
                 climi::start-angle climi::end-angle filled line-style ink))))
      (draw-line-output-record
       (with-slots (climi::point-x1 climi::point-y1 climi::point-x2 climi::point-y2 line-style ink)
           record
         (list :line
               (x climi::point-x1) (y climi::point-y1)
               (x climi::point-x2) (y climi::point-y2)
               line-style ink)))
      (draw-lines-output-record
       (with-slots (climi::coord-seq line-style ink)
           record
         (list :lines (xsys climi::coord-seq) line-style ink)))
      (draw-point-output-record
       (with-slots (point-x point-y line-style ink)
           record
         (list :point (x point-x) (y point-y) line-style ink)))
      (draw-points-output-record
       (with-slots (climi::coord-seq line-style ink)
           record
         (list :points (xsys climi::coord-seq) line-style ink)))
      (draw-polygon-output-record
       (with-slots (climi::coord-seq filled climi::closed line-style ink)
           record
         (list :polygon (xsys climi::coord-seq) filled climi::closed line-style ink)))
      (draw-text-output-record
       (let ((medium (make-instance 'emacs-medium)))
         (with-slots (ink text-style string point-x point-y
                      climi::align-x climi::align-y
                      climi::toward-x climi::toward-y)
             record
           (multiple-value-bind (width height final-x final-y baseline)
               (text-size medium string :text-style text-style)
             (declare (ignore height final-x final-y))
             (ecase climi::align-y
               (:top (incf point-y (text-style-ascent text-style medium)))
               (:center (incf point-y (- baseline (/ (text-style-height text-style medium) 2))))
               (:baseline nil)
               (:bottom (decf point-y (text-style-descent text-style medium))))
             (ecase climi::align-x
               (:left nil)
               (:center (decf point-x (/ width 2)))
               (:right (decf point-x width)))
             (list :text
                   string
                   point-x
                   point-y
                   text-style
                   ink
                   ;; FIXME We don't actually use these. The mismatch between CLIM's
                   ;;  notions of ltr and tb etc. and CSS3's are too great for now
                   ;;  - jqs 2020-05-08
                   climi::toward-x
                   climi::toward-y))))))))

(defun ellipse-angle-to-point (angle r1 r2)
  (flet ((zeroize (n) (if (< (abs n) 1e-6) 0 n)))
    (let* ((p (atan (* (/ r1 r2) (- (tan angle)))))
           (x (zeroize (* r1 (cos p))))
           (y (zeroize (* r2 (sin p)))))
      (cons x y))))


;;;; SVG

;;; Convert the list representation into SVG shapes.

(defun shapes-to-svg (shapes width height)
  (with-output-to-string (stream)
    (format stream "~&<svg viewBox='0 0 ~D ~D' xmlns='http://www.w3.org/2000/svg'>~%"
            (ceiling width) (ceiling height))
    (loop for shape in shapes do (format-svg shape stream))
    (format stream "~&</svg>~%")))

(defun format-svg (shape &optional stream)
  "Print SHAPE to STREAM in SVG format.
   If STREAM is NIL then return the SVG shape as a string."
  (alexandria:destructuring-ecase shape
    ((:rectangle x y w h filled line-style ink)
     (format stream "~&<rect x='~F' y='~F' width='~F' height='~F' fill='~A' stroke='~A' ~
                     ~@[stroke-width='~A' ~]~
                     ~@[stroke-linejoin='~A' ~]~
                     ~@[stroke-dasharray='~A' ~]~
                     />~%"
             x y w h
             (svg-color (if filled ink nil))
             (svg-color (if filled nil ink))
             (and (not filled) (svg-stroke-width line-style))
             (svg-stroke-line-join line-style)
             (and (not filled) (svg-stroke-dasharray line-style))))
    ((:rectangles coord-seq filled line-style ink)
     (do ((i 0 (+ i 4)))
         ((= i (length coord-seq)))
       (format stream "~&<rect x='~F' y='~F' width='~F' height='~F' fill='~A' stroke='~A' ~
                       ~@[stroke-width='~A' ~]~
                       ~@[stroke-linejoin='~A' ~]~
                       ~@[stroke-dasharray='~A' ~]~
                       />~%"
             (elt coord-seq i) (elt coord-seq (+ i 1)) (elt coord-seq (+ i 2)) (elt coord-seq (+ i 3))
             (svg-color (if filled ink nil))
             (svg-color (if filled nil ink))
             (and (not filled) (svg-stroke-width line-style))
             (svg-stroke-line-join line-style)
             (and (not filled) (svg-stroke-dasharray line-style)))))
    ((:ellipse cx cy r1 r2 start-angle end-angle filled line-style ink)
     (if (or (null start-angle)
             (and (zerop start-angle) (= (* 2 pi) end-angle)))
         (format stream "~&<ellipse cx='~F' cy='~F' rx='~F' ry='~F' fill='~A' stroke='~A' ~
                         ~@[stroke-width='~A' ~]~
                         ~@[stroke-dasharray='~A' ~]~
                         />~%"
                 cx cy r1 r2
                 (svg-color (if filled ink nil))
                 (svg-color (if filled nil ink))
                 (and (not filled) (svg-stroke-width line-style))
                 (and (not filled) (svg-stroke-dasharray line-style)))
         (let ((start-point (ellipse-angle-to-point start-angle r1 r2))
               (end-point (ellipse-angle-to-point end-angle r1 r2)))
           (format stream "~&<path d='M ~F,~F ~
                                      l ~F,~F ~
                                      a ~F,~F ~
                                        0 ~D,~D ~
                                        ~F,~F z' ~
                           fill='~A' stroke='~A' ~
                           ~@[stroke-width='~A' ~]~
                           ~@[stroke-dasharray='~A' ~]~
                           />~&"
                   cx cy
                   (car start-point) (cdr start-point)
                   r1 r2
                   (if (>= (abs (- end-angle start-angle)) pi) 1 0)
                   (if (plusp (- end-angle start-angle)) 0 1)
                   (- (car end-point) (car start-point)) (- (cdr end-point) (cdr start-point))
                   (svg-color (if filled ink nil))
                   (svg-color (if filled nil ink))
                   (and (not filled) (svg-stroke-width line-style))
                   (and (not filled) (svg-stroke-dasharray line-style))))))
    ((:line x1 y1 x2 y2 line-style ink)
     (format stream (svg-line x1 y1 x2 y2 line-style ink)))
    ((:lines coord-seq line-style ink)
     (do ((i 0 (+ i 4)))
         ((= i (length coord-seq)))
       (format stream (svg-line (elt coord-seq i)       (elt coord-seq (+ i 1))
                                (elt coord-seq (+ i 2)) (elt coord-seq (+ i 3))
                                line-style ink))))
    ((:point x y line-style ink)
     (format stream (svg-point x y line-style ink)))
    ((:points position-seq line-style ink)
     (do ((i 0 (+ i 2)))
         ((= i (length position-seq)))
       (format stream (svg-point (elt position-seq i) (elt position-seq (+ i 1)) line-style ink))))
    ((:polygon coord-seq filled closed line-style ink)
     (format stream "~&<poly~A points='~{~F, ~F~^ ~}' ~
                     ~@[stroke='~A' ~]~
                     ~@[fill='~A' ~]~
                     ~@[stroke-width='~A' ~]~
                     ~@[stroke-linejoin='~A' ~]~
                     ~@[stroke-linecap='~A' ~]~
                     ~@[stroke-dasharray='~A' ~]~
                     />"
             (if (and (null filled) (null closed)) "line" "gon")
             coord-seq
             (svg-color (if filled nil ink))
             (svg-color (if filled ink nil))
             (and (not filled) (svg-stroke-width line-style))
             (svg-stroke-line-join line-style)
             (and (not filled) (not closed) (svg-stroke-line-cap line-style))
             (svg-stroke-dasharray line-style)))
    ((:text string x y text-style ink toward-x toward-y)
     (declare (ignore toward-x toward-y))
     (format stream "~&<text style='~A;' xml:space='preserve' fill='~A'><tspan x='~F' y='~F'><tspan>~A</tspan></tspan></text>"
             (svg-text-style text-style)
             (svg-color ink)
             x y
             string))))

(defun svg-point (x y line-style ink)
  (let ((radius (/ (line-style-thickness line-style) 2))
        (unit (svg-stroke-unit line-style)))
    (format nil "~&<circle cx='~F' cy='~F' r='~F~A' fill='~A' />~%"
            x y radius unit (svg-color ink))))

(defun svg-line (x1 y1 x2 y2 line-style ink)
  (let ((stroke-width (svg-stroke-width line-style))
        (stroke-linejoin (svg-stroke-line-join line-style))
        (stroke-linecap (svg-stroke-line-cap line-style))
        (stroke-dasharray (svg-stroke-dasharray line-style)))
    (format nil  "~&<line x1='~F' y1='~F' x2='~F' y2='~F' stroke='~A' ~
                  ~@[stroke-width='~A' ~]~
                  ~@[stroke-linejoin='~A' ~]~
                  ~@[stroke-linecap='~A' ~]~
                  ~@[stroke-dasharray='~A' ~]~
                  />"
            x1 y1 x2 y2 (svg-color ink)
            stroke-width
            stroke-linejoin
            stroke-linecap
            stroke-dasharray)))
  
(defun svg-color (ink)
  "Return an SVG color string representing INK (which may be NIL.)"
  (if ink
      (multiple-value-bind (r g b a) (color-rgba ink)
        (format nil "rgba(~f%, ~f%, ~f%, ~f)" (* r 100) (* g 100) (* b 100) a))
      "none"))

