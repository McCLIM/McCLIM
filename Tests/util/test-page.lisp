(cl:in-package #:clim-test-util)

(defun %print-test-page (stream)
  (labels ((text (x y width)
             (surrounding-output-with-border (stream :shape :rounded
                                                     :background +salmon+)
               (with-temporary-margins (stream :left  `(:absolute ,x)
                                               :top   `(:absolute ,y)
                                               :right `(:absolute ,(+ y width)))
                 (with-end-of-line-action (stream :wrap)
                   (loop :for i :from 0 :to 30
                         :for size = (+ 2 i)
                         :do (with-drawing-options (stream :text-size i)
                               (princ i stream)))))))
           (wheel (x y r color-function)
             (loop :with di = .1
                   :for i :from 0 :to (* 2 pi) :by di
                   :for color = (funcall color-function (/ i 2 pi))
                   :do (draw-circle* stream x y r
                                     :start-angle i :end-angle (+ i di)
                                     :filled t :ink color))
             (draw-circle* stream x y r
                           :filled nil :ink +black+
                           :line-thickness 4 :line-dashes '(8 8)))
           (wheels (x y r)
             (let* ((cx1 (+ x r))
                    (cx2 (+ cx1 (* 2 r) 8))
                    (cy  y))
               (wheel cx1 cy r (lambda (hue) (make-ihs-color 1 hue 1)))
               (wheel cx2 cy r (lambda (i) (make-ihs-color i 0 0)))))
           (graph (x y width height)
             (let ((end (* 20 pi)))
               (with-translation (stream x (+ y (/ height 2)))
                 (with-scaling (stream (/ width (sqrt end)) (/ height 2))
                   (draw-polygon*
                    stream (loop :for i :from 0 :to end :by .1
                                 :collect (sqrt i)
                                 :collect (* (/ i end) (sin i)))
                    :filled nil :closed nil :line-thickness 2)))))
           (character-width (x y)
             (with-translation (stream x y)
               (loop :with width = (stream-character-width stream #\M)
                     :with height = (nth-value 1 (text-size stream "Ty"))
                     :for position :from 0 :by width
                     :for character :across "AILTMy"
                     :do (draw-text* stream (string character) position 0)
                         (draw-rectangle* stream position 0 (+ position width) (- height)
                                          :filled nil :ink +red+
                                          :line-dashes '(4 4) :line-thickness .3)))))
    (text 0 0 200)
    (wheels 208 0 80)
    (graph 0 -108 200 100)
    (character-width 0 140)))

(defun print-test-page (stream)
  (with-room-for-graphics (stream :first-quadrant nil)
    (%print-test-page stream)))
