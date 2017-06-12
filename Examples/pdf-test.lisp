
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
      (progn
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
        (let ((design
               (mcclim-bezier:make-bezier-curve*
                (list 20 150 20 80 90 110 90 170 90 220 140 210 140 140))))
          (mcclim-bezier:draw-bezier-design* stream design
                                             :line-thickness 5
                                             :ink +orange+))
        (let ((design
               (mcclim-bezier:make-bezier-area*
                (list 34 44 34 128 147 44 47 156 34 128 50 50 34 44))))
          (mcclim-bezier:draw-bezier-design* stream design
                                             :line-thickness 4
                                             :ink +sea-green+)))

      (draw-text* stream "Test Page" 170 130
                  :text-style (make-text-style :fix :bold :huge))
      (draw-text* stream "(Bogus)" 250 250)))
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
      (progn
        (loop for i from 0 to 100 by 10
           do
             (draw-line* stream i 0 i 100 :line-thickness 2 :ink +black+)
             (draw-line* stream 0 i 100 i :line-thickness 2 :ink +black+))

        (draw-polygon* stream '(172  22   228  40   227  59   264  60   256  80   277  88
                                319  54   336  58   345  43   353  55   345  71   370  86
                                384 124   375 141   393 159   398 208   416 237   410 258
                                397 252   314 302   333 351   380 389   341 426   350 461
                                324 452   280 471   252 462   240 474   172 448   166 460
                                132 457   140 410   160 378   116 368    92 346    79 307
                                94 295    82 252    90 229    84 204   113 201   112 162
                                129 142   130 104   157 102   174 118   182  96   204  96
                                186  58   196 50)
                       :ink +dark-green+)
        
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
        (let ((design
               (mcclim-bezier:make-bezier-curve*
                (list 20 150 20 80 90 110 90 170 90 220 140 210 140 140))))
          (mcclim-bezier:draw-bezier-design* stream design
                                             :line-thickness 5
                                             :ink +orange+))
        (let ((design
               (mcclim-bezier:make-bezier-area*
                (list 34 44 34 128 147 44 47 156 34 128 50 50 34 44))))
          (mcclim-bezier:draw-bezier-design* stream design
                                             :line-thickness 4
                                             :ink +sea-green+)))
      (draw-text* stream "Test Page" 170 200
                  :text-style (make-text-style :fix :bold :huge))
      (draw-text* stream "Bogus" 250 250)))
  file)

(grid-test-1-pdf)

