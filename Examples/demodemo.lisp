;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2002 by Gilbert Baumann
;;;  (c) copyright 2017-2020 by Daniel Kochmański
;;;  (c) copyright 2018-2020 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Launcher application for various demos and graphical tests.
;;;

(in-package #:clim-demo)

(defun make-demo-button (title demo-frame-class)
  (make-pane 'push-button
             :label title
             :activate-callback
             (let ((frame nil))
               (lambda (gadget)
                 (let ((calling-frame (gadget-client gadget)))
                   (cond ((null frame) ; I broke this logic, sorry.. -Hefner
                          (setq frame
                                (run-frame-top-level
                                 (make-application-frame
                                  demo-frame-class
                                  :calling-frame calling-frame))))
                         (t
                          #+nil
                          (destroy-frame frame))))))))

(defun run-demo (name &key background)
  "Coerces `name' into symbol in package `clim-demo' and runs application
denoted by this symbol."
  (let ((frame (make-application-frame
                (find-symbol (string-upcase (string name))
                             (find-package "CLIM-DEMO")))))
    (if background
        (bt:make-thread (lambda () (run-frame-top-level frame))
                        :initial-bindings `((*default-server-path* . ',*default-server-path*)))
        (run-frame-top-level frame))
    frame))

(defgeneric display (frame pane)
  (:documentation "Generic method meant to be specialized at least on
the first argument to avoid creating too many functions with similar
name."))

(define-application-frame demodemo ()
  ()
  (:menu-bar nil)
  (:layouts
   (default
    (vertically (:equalize-width t)
      (labelling (:label "McCLIM Demos"
                  :text-style (make-text-style :sans-serif :roman :huge)
                  :align-x :center))
      (horizontally ()
        (labelling (:label "Demos")
          (vertically (:equalize-width t)
            (make-demo-button "CLIM-Fig"  'clim-demo.clim-fig:clim-fig)
            (make-demo-button "Calculator"  'clim-demo.calculator:calculator-app)
            (make-demo-button "Method Browser" 'method-browser)
            (make-demo-button "Address Book"  'clim-demo.address-book:address-book)
            (make-demo-button "Puzzle"  'puzzle)
            (make-demo-button "Colorslider" 'clim-demo.colorslider:colorslider)
            (make-demo-button "Logic Cube" 'logic-cube)
            (make-demo-button "Checkers" 'clim-demo.checkers:clim-checkers)
            (make-demo-button "Gadget Test"  'gadget-test)
            (make-demo-button "D&D Translator" 'drag-test)
            (make-demo-button "Draggable Graph" 'draggable-graph-demo)
            (make-pane 'push-button :label "Font Selector"
                                    :activate-callback
                                    (lambda (gadget)
                                      (let ((frame (gadget-client gadget)))
                                        (format *trace-output* "~&You chose: ~A~%"
                                                (select-font :calling-frame frame)))))
            (make-demo-button "Tab Layout" 'clim-demo.tabdemo:tabdemo)
            (make-demo-button "Summation" 'summation)
            (make-demo-button "German Towns" 'clim-demo.town-example:town-example)
            (make-demo-button "Data Graph Toy" 'graph-toy)
            (make-demo-button "Traffic lights" 'traffic-lights)
            (make-demo-button "Image Transform" 'clim-demo.image-transform-demo:image-transform-demo)
            (make-demo-button "Selection (clipboard)" 'selection-demo)
            (make-demo-button "DND various" 'clim-demo.drag-and-drop-example:dnd-commented)
            (make-demo-button "File manager" 'clim-demo.file-manager:file-manager)
            (make-demo-button "Stopwatch" 'clim-demo.stopwatch:stopwatch)))
        (labelling (:label "Tests")
          (vertically (:equalize-width t)
            (make-demo-button "Stream test" 'stream-test)
            (make-demo-button "Label Test" 'label-test)
            (make-demo-button "Table Test" 'table-test)
            (make-demo-button "Scroll Test" 'Scroll-test)
            (make-demo-button "List Test" 'list-test)
            (make-demo-button "Option Test" 'option-test)
            (make-demo-button "HBOX Test"  'hbox-test)
            (make-demo-button "Text Size Test"  'text-size-test)
            (make-demo-button "Drawing Benchmark" 'drawing-benchmark)
            (make-demo-button "Border Styles Test" 'bordered-output)
            (make-demo-button "Misc. Tests" 'clim-demo.misc:misc-tests)
            (make-demo-button "Render Image Tests" 'render-image-tests)
            (make-demo-button "Drawing Tests" 'clim-demo.drawing-tests:drawing-tests)
            (make-demo-button "Pixmaps" 'clim-demo.pixmaps:pixmaps)
            (make-demo-button "Accepting Values Test"  'av-test)
            (make-demo-button "Frame Icon and Name Test" 'clim-demo.names-and-icons:frame-sheet-name-test)
            (make-demo-button "Tracking Pointer test" 'tracking-pointer-test)
            (make-demo-button "Sheet geometry" 'clim-demo.sheet-geometry:frame)))
        (labelling (:label "Regression Tests")
          (vertically (:equalize-width t)
            (make-demo-button "Image viewer" 'image-viewer)
            (make-demo-button "Coordinate swizzling"
                              'clim-demo.coord-swizzling:coordinate-swizzling)
            (make-demo-button "Scroll Test 2" 'Scroll-test-2)
            (make-demo-button "Tables with borders" 'table-demo)
            (make-demo-button "Menu Test"  'clim-demo.menu-test:menu-test)
            (make-demo-button "Drag and Drop" 'dragndrop)
            (make-demo-button "Pane hierarchy viewer" 'clim-demo.hierarchy:hierarchy)
            (make-demo-button "Patterns, designs and inks" 'clim-demo.patterns:pattern-design-test)
            (make-demo-button "Flipping ink" 'flipping-ink)
            (make-demo-button "Overlapping patterns" 'patterns-overlap)
            (make-demo-button "Text transformations" 'text-transformations-test)
            (make-demo-button "Text multiline positioning" 'text-multiline-positioning)
            (make-demo-button "SEOS baseline and wrapping" 'seos-baseline)
            (make-demo-button "Indentation" 'indentation)
            (make-demo-button "Presentation translators" 'clim-demo.presentation-translators-test:presentation-translators-test)
            (make-demo-button "Graph formatting" 'clim-demo.graph-formatting-test:graph-formatting-test)
            (make-demo-button "Asynchronous commands" 'clim-demo.execute-frame-command:homogenous)
            (make-demo-button "Frame reinitialize" 'clim-demo.reinitialize-frame:example-frame)
            (make-demo-button "Nested clipping" 'clim-demo.nested-clipping:nested-clipping))))))))

(defun demodemo (&rest args)
  (apply #'find-application-frame 'demodemo args))

(define-application-frame hbox-test ()
  ()
  (:menu-bar nil)
  (:layouts
   (default
    (horizontally ()
      30
      (make-pane 'push-button :label "Okay"
                              :width '(50 :mm))
      '+fill+
      (make-pane 'push-button :label "Cancel")
      '+fill+
      (make-pane 'push-button :label "Help")
      5))))

(define-application-frame table-test ()
  ()
  (:menu-bar nil)
  (:layouts
   (default
    (tabling (:background +red+)
      (list (make-pane 'push-button :label "Last Name" :max-height +fill+)
            (make-pane 'push-button :label "First Name" #||:max-height +fill+||#))
      (list (make-pane 'push-button :label "C 1 0")
            (make-pane 'push-button :label "C 1 1"))))))

(defun make-label-test-column (title label content)
  (flet ((make-label (align-x align-y)
           (let ((alignment-text (format nil "~S" (list align-x align-y)))
                 (text-style (make-text-style :sans-serif :roman :normal)))
             (macrolet ((frob (label &body contents)
                          `(labelling (:label ,label
                                       :align-x align-x
                                       :label-alignment align-y
                                       :foreground +white+
                                       :background +paleturquoise4+
                                       :text-style text-style)
                             ,@contents)))
               (ecase content
                 (:child (frob label
                           (make-pane 'push-button :label alignment-text
                                                   :text-style text-style
                                                   :max-width 1000
                                                   :max-height 1000)))
                 (:alignment (frob alignment-text))
                 (:label (frob label)))))))
    (labelling (:label title)
      (vertically (:spacing 5 :equalize-width t)
        (make-label :left :top)
        (make-label :center :top)
        (make-label :right :top)
        (make-label :left :bottom)
        (make-label :center :bottom)
        (make-label :right :bottom)))))

(define-application-frame label-test ()
  ()
  (:menu-bar nil)
  (:layouts
   (default
    (vertically (:equalize-width t)
      10
      (labelling (:label "CLIM Label Tests"
                  :align-x :center
                  :text-style (make-text-style :sans-serif :roman :huge)))
      10
      (9/10 (horizontally (:equalize-height t)
              ;; Please keep the silly "good" so the label text goes
              ;; below the baseline. -- jm 2019-12-14
              (1/4 (make-label-test-column
                    "Labels with content" #1="Some good label" :child))
              (1/4 (make-label-test-column
                    "Labels without content" #1# :alignment))
              (1/4 (make-label-test-column
                    "Multi-line w/ content"
                    #2=#.(format nil "Multi-line~%label")
                    :child))
              (1/4 (make-label-test-column
                    "Multi-line w/o content" #2# :label))))))))

(defclass foo-pane (basic-pane)
  ())

(defmethod compose-space ((pane foo-pane) &key width height)
  (declare (ignore width height))
  (make-space-requirement :width 800
                          :height 1e3))

(defmethod handle-repaint ((pane foo-pane) region)
  (draw-line* pane 50 50 200 50)
  (multiple-value-bind (x1 y1 x2 y2) (bounding-rectangle* region)
    (declare (ignore x1 x2))
    (let ((k 20))
      (loop for y from (* k (floor (- y1 10) k)) below (+ y2 10) by k do
            (draw-text* pane (format nil "~D" y) 20 y)))))

(define-application-frame scroll-test
    () ()
    (:menu-bar nil)
    (:layouts
     (defaults
         (scrolling (:width 400 :height 400)
           (make-pane 'foo-pane)))))


;;; Scroll test 2
(define-application-frame scroll-test-2 ()
  ()
  (:menu-bar nil)
  (:geometry :width 1200 :height 400)
  (:panes (out :application-pane :display-function #'scroll-test-display)
          (bam :application-pane :display-function #'scroll-test-display)
          (foo :application-pane :display-function #'scroll-test-display)
          (qux :application-pane :display-function #'scroll-test-display))
  (:layouts (default
                (horizontally ()
                  (labelling (:label "bam") (scrolling () bam))
                  (labelling (:label "foo") (scrolling () foo))
                  (labelling (:label "qux") (scrolling () qux))))))

(defmethod scroll-test-display ((frame scroll-test-2) pane)
  (flet ((draw-rectangle (x1 y1 x2 y2)
           (draw-rectangle* pane x1 y1 x2 y2
                            :filled nil
                            :ink +red+
                            :line-thickness 3
                            :line-dashes t)
           (draw-point* pane x1 y1 :line-thickness 7 :ink +blue+)
           (draw-point* pane x2 y2 :line-thickness 7 :ink +blue+)
           (draw-text* pane (format nil "(~s, ~s)" x1 y1)
                       (+ x1 5) (+ 5 y1)
                       :align-x :left
                       :align-y :top)
           (draw-text* pane (format nil "(~s, ~s)" x2 y2)
                       (- x2 5) (- y2 5)
                       :align-x :right
                       :align-y :bottom)))
    (when (member (pane-name pane) '(bam qux))
      (draw-rectangle -100 -100 +100 +100))
    (when (member (pane-name pane) '(foo qux))
      (draw-rectangle +300 +300 +500 +500))))

(define-application-frame list-test ()
  ()
  (:menu-bar nil)
  (:panes
   (substring :text-field :value "inter"
                          :value-changed-callback
              (lambda (pane value)
                (alexandria:when-let*
                    ((frame (gadget-client pane))
                     (result-list (find-pane-named frame 'result-list)))
                  (setf (list-pane-items result-list)
                        (apropos-list value :clim #+sbcl t)))))
   (result-list
    (make-pane 'list-pane
               :value 'region-intersection
               :items (apropos-list "inter" :clim #+sbcl t)
               :presentation-type-key (constantly 'list-test-symbol)
               :name-key #'string-downcase))
   (interactor :interactor :height 200))
  (:layouts
   (defaults
    (vertically ()
      (labelling (:label "Matching symbols")
        (scrolling (:height 200)
          result-list))
      (spacing (:thickness 4)
        substring)
      interactor))))

(define-presentation-type list-test-symbol ())

(define-list-test-command com-describe-symbol
    ((sym 'list-test-symbol :gesture :select))
  ;; Let's print only three lines, we don't have space for more.
  (with-input-from-string (s (with-output-to-string (s) (describe sym s)))
    (dotimes (x 3)
      (write-line (read-line s nil "") *standard-input*))))

(define-application-frame option-test ()
  ()
  (:menu-bar nil)
  (:panes (option-pane-1 :option-pane
                         :value 1
                         :items '(1 2 3 4 6 7)
                         :value-changed-callback (constantly nil))
          (option-pane-2 :option-pane
                         :value "Option 1"
                         :items '("Option 1" "Option 2" "Option 3" "Option 4" "Option 6" "Option 7")
                         :value-changed-callback (constantly nil)))
  (:layouts
   (:default
    (labelling (:label "Option panes example")
      (vertically ()
        (1/2 option-pane-1)
        (1/2 option-pane-2))))))

(format t "~&;; try (CLIM-DEMO:DEMODEMO)~%")
