
;;; in order to load this, one has to do (asdf:load-system 'clim-pdf)
;;; -- it doesn't get loaded automatically yet.

(defpackage #:clim-pdf-test
  (:use #:clim #:clim-lisp #:clim-extensions))

(in-package #:clim-pdf-test)

(defun grid-test-1-ps (&key (file #p"grid-test-1.ps"))
  (format t ";; Creating ~S.~%" file)
  (with-open-file (file-stream file :direction :output
                               :if-exists :supersede)
    (with-output-to-postscript-stream
        (stream file-stream
                :header-comments '(:title "Foo")
                :scale-to-fit t)
      (loop for i from 0 to 100 by 10
         do
           (draw-line* stream i 0 i 100 :line-thickness 2 :ink +black+)
           (draw-line* stream 0 i 100 i :line-thickness 2 :ink +black+))
      (draw-line* stream 0 0 100 100 :ink +red+ :line-thickness 6)
      (draw-line* stream 20 0 80 20)
      (draw-lines* stream
                   #(320 0 380 20
                     320 30 380 60)
                   :line-thickness 3
                   :ink +orange+)
      (draw-point* stream 200 100 :ink +blue+)
      (draw-points* stream #(10 150 30 150 50 150) :ink +brown+ :line-thickness 10)
      (draw-polygon* stream #(120 120
                              150 120
                              150 150
                              135 160
                              120 150)
                     :ink +green+ :filled t :closed t)
      (draw-rectangle* stream 180 50 220 70 :ink +pink+ :filled nil :line-thickness 4)

      (draw-rectangles* stream
                        #(180 150 220 170
                          180 190 220 210)
                        :ink +purple+ :line-thickness 3 :filled nil)
      ))
  file)

(grid-test-1-ps)

(defun grid-test-1-pdf (&key (file #p"grid-test-1.pdf"))
  (format t ";; Creating ~S.~%" file)
  (with-open-file (file-stream file :direction :output
                               :if-exists :supersede
                               :element-type '(unsigned-byte 8))
    (clim-pdf::with-output-to-pdf-stream
        (stream file-stream
                :header-comments '(:title "Foo")
                :scale-to-fit t)
      (loop for i from 0 to 100 by 10
         do
           (draw-line* stream i 0 i 100 :line-thickness 2 :ink +black+)
           (draw-line* stream 0 i 100 i :line-thickness 2 :ink +black+))
      (draw-line* stream 0 0 100 100 :ink +red+ :line-thickness 6)
      (draw-line* stream 20 0 80 20)
      (draw-lines* stream
                   #(320 0 380 20
                     320 30 380 60)
                   :line-thickness 3
                   :ink +orange+)
      (draw-point* stream 200 100 :ink +blue+)
      (draw-points* stream #(10 150 30 150 50 150) :ink +brown+ :line-thickness 10)
      (draw-polygon* stream #(120 120
                              150 120
                              150 150
                              135 160
                              120 150)
                     :ink +green+ :filled t :closed t)
      (draw-rectangle* stream 180 50 220 70 :ink +pink+ :filled nil :line-thickness 4)

      (draw-rectangles* stream
                        #(180 150 220 170
                          180 190 220 210)
                        :ink +purple+ :line-thickness 3 :filled nil)
      ))
  file)

(grid-test-1-pdf)
