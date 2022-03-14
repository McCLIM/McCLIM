(in-package #:clim-user)

(defun test-drawing (stream)
  (with-drawing-options (stream :transformation (make-reflection-transformation* 0 180 100 180))
    (draw-ellipse* stream 320 180 300 0 0 150 :ink +grey+)
    (draw-ellipse* stream 320 180 300 0 0 150 :ink +black+ :filled nil
                                              :line-dashes '(8 16)
                                              :line-thickness 8)
    (draw-circle* stream 220 250 25 :ink +blue+)
    (draw-circle* stream 420 250 25 :ink +blue+)
    (draw-point* stream 220 250 :ink +cyan+ :line-thickness 15)
    (draw-point* stream 420 250 :ink +cyan+ :line-thickness 15)
    (draw-rectangle* stream 125 150 150 175 :ink +deep-pink+ )
    (draw-rectangle* stream 515 150 490 175 :ink +deep-pink+ )
    (draw-polygon* stream (list 320 225 280 150 360 150) :filled t :ink +orange+)
    (draw-polygon* stream (list 320 225 280 150 360 150) :filled nil :ink +black+
                                                         :line-thickness 4
                                                         :line-joint-shape :round)
    (draw-line* stream 175 125 465 125 :ink +blue+ :line-thickness 4 :line-cap-shape :no-end-point)
    (draw-line* stream 175 130 465 130 :ink +blue+ :line-thickness 4 :line-cap-shape :butt)
    (draw-line* stream 175 135 465 135 :ink +blue+ :line-thickness 4 :line-cap-shape :square)
    (draw-line* stream 175 140 465 140 :ink +blue+ :line-thickness 4 :line-cap-shape :round)

    (let ((smile '(200 100 280 75 360 75 440 100 200 100 200 100 200 100)))
      (draw-bezigon* stream smile :filled t :ink +red+)
      (draw-bezigon* stream smile :filled nil :ink +black+ :line-dashes nil :line-thickness 4))
    (draw-circle* stream 0 0 25 :ink +red+)
    (draw-circle* stream 320 300 20 :ink +cyan+)
    (draw-line* stream 300 300 340 300 :line-thickness 1)
    (draw-text* stream "McCLIM" 320 300
                :align-x :center :align-y :center :text-size :large
                :transform-glyphs t
                :ink +dark-green+
                :transformation (make-rotation-transformation* (/ pi 6) 320 300))))

(defun test-clipping (stream show-hints-p)
  (let* ((regions (list (make-rectangle* 25 25 175 175)
                        (make-rectangle* 150 25 300 175)
                        (make-rectangle* 25 150 175 300)
                        (make-ellipse* 225 225 90 0 0 90)))
         (inter (reduce #'region-intersection regions :initial-value +everywhere+))
         (negative (make-ellipse* 162.5 162.5 8 0 0 8))
         (clipping (region-difference inter negative)))
    ;; Show the clipping region.
    (when show-hints-p
      (loop for region in regions
            for base-ink in (list +cyan+ +cyan+ +cyan+   +orange+)
            do (draw-design stream region :ink (compose-in base-ink (make-opacity 0.1))))
      (loop for region in regions
            do (draw-design stream region :filled nil :line-dashes t)))
    ;; ;; Draw the thing.
    (with-drawing-options (stream :clipping-region clipping)
      (draw-rectangle* stream 5 5 345 345 :filled t :ink +dark-green+))
    ;; Draw the negative region outline.
    (when show-hints-p
      (draw-design stream negative :ink +dark-red+ :filled nil :line-dashes '(4 4) :line-thickness 2))))

(defun test-ellipse (stream)
  (draw-ellipse* stream 200 180 100 0 0 50 :ink +red+)
  (draw-ellipse* stream 440 180 100 0 0 50
                 :ink +blue+ :start-angle (/ pi 6) :end-angle (* 7/4 pi))
  (draw-ellipse* stream 440 180 100 0 0 50
                 :ink +white+ :filled nil :start-angle (/ pi 6) :end-angle (* 7/4 pi)))

(defun test-line-style (stream)
  (labels ((draw-lines (&rest options)
             (apply #'draw-line* stream 10 10 90 10 options)
             (apply #'draw-line* stream 10 20 90 20 :line-thickness 4 options)
             (apply #'draw-line* stream 10 30 90 30 :line-thickness 4 :line-dashes t options)
             (apply #'draw-line* stream 10 40 90 40 :line-thickness 4 :line-dashes #(8 8 4 4) options)
             (apply #'draw-polygon* stream  '(10 75 30 50 50 100 100 75)
                    :line-thickness 2 :filled nil options)))
    (with-translation (stream 0 0)
      (draw-lines))
    (with-translation (stream 0 100)
      (draw-lines :line-style (make-line-style :joint-shape :round :cap-shape :round)))
    (with-translation (stream 100 0)
      (with-scaling (stream 2 2)
        (draw-lines :line-style (make-line-style :unit :coordinate :joint-shape :round))))
    (with-translation (stream 300 0)
      (with-scaling (stream 2 2)
        (draw-lines :line-unit :normal)))))

(defun test-text-style (stream)
  (draw-rectangle* stream 0 0 300 300 :ink +light-cyan+)
  (loop for dy from 50 by 50
        for family in '(:serif :sans-serif :fix "fantasy")
        for style  in '(:roman :bold (:bold :italic) :italic)
        for size in   '(:normal :small :large :smaller)
        for ts = (make-text-style family style size) do
          (draw-text* stream "Hello World" 20 dy :align-y :top :text-style ts :ink +dark-red+)))

(defvar *kitten*
  (make-pattern-from-bitmap-file
   (asdf:component-pathname
    (asdf:find-component "clim-examples" '("images" "kitten.jpg")))))

(defun draw-the-internet (stream precision)
  (assert (< precision 90))
  (draw-circle* stream 550 180 150 :ink *kitten*))


(defvar *checkers-array*
  (let ((array (make-array '(100 100) :element-type 'fixnum :initial-element 0)))
    (loop for row from 0 below 50 do
      (loop for col from 0 below 50 do
        (setf (aref array row col) 1)
        (setf (aref array (- 99 row) (- 99 col)) 1)))
    array))

(defun make-checkers (design1 design2 width height)
  (make-rectangular-tile
   (make-pattern *checkers-array* (list design1 design2))
   width height))

(defun draw-rectangular-tile (stream)
  (let ((pattern (make-checkers +black+ +white+ 75 75)))
    (draw-circle* stream 550 180 150 :ink pattern)
    (draw-circle* stream 550 180 150 :ink +dark-blue+ :filled nil :line-thickness 4)))

(defun draw-transformed-tile (stream)
  (let* ((pattern (transform-region
                   (make-rotation-transformation (/ pi 6))
                   (make-checkers +black+ +white+ 75 75))))
    (draw-circle* stream 550 180 150 :ink pattern)
    (draw-circle* stream 550 180 150 :ink +dark-blue+ :filled nil :line-thickness 4)))

(defun draw-recursive-pattern (stream)
  (let ((pattern (make-checkers +black+ *kitten* 100 100)))
    (draw-circle* stream 550 180 150 :ink pattern)
    (draw-circle* stream 550 180 150 :ink +dark-blue+ :filled nil :line-thickness 4)))

(defvar *glider*
  (make-pattern-from-bitmap-file
   (asdf:component-pathname
    (asdf:find-component "clim-examples" '("images" "glider.png")))))

(defun draw-masked-compositums (stream mask)
  (let* ((glider (make-rectangular-tile *glider* 100 100))
         (pattern-1 (compose-in glider mask))
         (pattern-2 (compose-out glider mask)))
    (draw-circle* stream 50 50 150 :ink (make-checkers +dark-green+ +dark-blue+ 75 75))
    (draw-circle* stream 50 50 50 :ink pattern-1)
    (draw-circle* stream 100 100 50 :ink pattern-2)
    (draw-circle* stream 50 50 150 :ink +dark-blue+ :filled nil :line-thickness 4)))


(defvar *stencil*
  (let ((array (make-array '(100 100) :element-type '(single-float 0.0 1.0) :initial-element 0.0)))
    (loop for row from 0 below 100 do
      (loop for col from 0 below 100 do
        (setf (aref array row col)
              (- 1.0
                 (/ (sqrt (+ (expt (- row 50) 2)
                             (expt (- col 50) 2)))
                    50.0)))))
    (make-stencil array)))

(defun draw-stencil (stream stencil)
  (let* ((glider (make-rectangular-tile *glider* 100 100))
         (pattern-1 (compose-in glider stencil))
         (pattern-2 (compose-out glider stencil)))
    (draw-circle* stream 50 50 150 :ink (make-checkers +dark-green+ +dark-blue+ 75 75))
    (draw-rectangle* stream 0 0 50 150 :ink pattern-1)
    (draw-rectangle* stream 50 0 100 150 :ink pattern-2)
    (draw-circle* stream 50 50 150 :ink +dark-blue+ :filled nil :line-thickness 4)))


;;; streams

#+bug
(progn
  (defparameter *glidie*
    (transform-region (make-scaling-transformation 1/4 1/4) *glider*))

  (defun format-table (stream rows cols)
    (formatting-table (stream)
      (dotimes (row rows)
        (formatting-row (stream)
          (dotimes (col cols)
            (formatting-cell (stream)
              (draw-pattern* stream *glidie* 0 0 )
              #+ (or)
              (format stream "Row ~s, Col ~s" row col)))))))

  (defun format-graph (stream depth breadth)
    (format-graph-from-roots
     (list depth)
     (lambda (obj str)
       (draw-pattern* stream *glidie* 0 0 )
       #+ (or)
       (format str "Node ~s" obj))
     (lambda (obj)
       (when (plusp obj)
         (make-list breadth :initial-element (1- obj))))
     :stream stream :orientation :horizontal)))

(defun format-table (stream rows cols)
  (formatting-table (stream)
    (dotimes (row rows)
      (formatting-row (stream)
        (dotimes (col cols)
          (formatting-cell (stream)
            (surrounding-output-with-border (stream)
              (format stream "Row ~s, Col ~s" row col))))))))

(defun format-graph (stream depth breadth)
  (format-graph-from-roots
   (list depth)
   (lambda (obj str)
     (surrounding-output-with-border (stream)
      (format str "Node ~s" obj)))
   (lambda (obj)
     (when (plusp obj)
       (make-list breadth :initial-element (1- obj))))
   :stream stream :orientation :vertical))

#+ (or)
(with-output-to-drawing-stream (stream :svg "/tmp/truncated-inches.svg" :preview t
                                       :width 640 :height 360
                                       :orientation :graphics
                                       :units :inches
                                       :scale-to-fit t)
  (medium-clear-area stream 0 0 640 360)
  (draw-circle* stream 0 0 100 :ink +dark-red+)
  (format stream "~a ~a~%"
          (graft-units (graft stream))
          (graft-orientation (graft stream)))
  (format-graph stream 3 2)
  (terpri stream)
  (format-table stream 10 8))

