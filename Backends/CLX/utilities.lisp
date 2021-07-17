(in-package #:clim-clx)

(deftype clx-coordinate () '(signed-byte 16))

(declaim (inline round-coordinate))
(defun round-coordinate (x)
  "Function used for rounding coordinates."
  ;; When in doubt we use "round half up" rounding, instead of the CL:ROUND
  ;; "round half to even".
  ;;
  ;; Reason: As the CLIM drawing model is specified, you quite often want to
  ;; operate with coordinates, which are multiples of 1/2.  Using CL:ROUND gives
  ;; "random" results. Using "round half up" gives you more consistent results.
  ;;
  ;; Note that CLIM defines pixel coordinates to be at the corners, while in X11
  ;; they are at the centers. We don't do much about the discrepancy, but
  ;; rounding up at half pixel boundaries seems to work well.
  (etypecase x
    (integer      x)
    (single-float (values (floor (+ x .5f0))))
    (double-float (values (floor (+ x .5d0))))
    (long-float   (values (floor (+ x .5l0))))
    (ratio        (values (floor (+ x 1/2))))))

(defmacro with-round-positions ((transformation &rest coordinates) &body body)
  (destructuring-bind (x y &rest rest-coords) coordinates
    (let ((with (if (or (null transformation)
                        (eq transformation '+identity-transformation+))
                    `(progn)
                    `(with-transformed-position (,transformation ,x ,y)))))
     (if (null rest-coords)
         `(,@with
           (setf ,x (round-coordinate ,x)
                 ,y (round-coordinate ,y))
           ,@body)
         `(,@with
            (setf ,x (round-coordinate ,x)
                  ,y (round-coordinate ,y))
            (with-round-positions (,transformation ,@rest-coords)
              ,@body))))))

(defmacro with-round-coordinates ((transformation coordinates) &body body)
  `(,@(if (or (null transformation)
              (eq transformation '+identity-transformation+))
          `(progn)
          `(with-transformed-positions (,transformation ,coordinates)))
    ,@body))

(defconstant +clx-clip+
  (load-time-value (make-rectangle* #x-8000 #x-8000 #x7FFF #x7FFF)))

;;; FIXME it is undefined behavior when we provide (valid) coordinates that do
;;; not fall inside the drawable region. Experiments show that rendering works
;;; fine for rectangles and breaks for polygons. -- jd 2021-04-07

(defmacro with-clipped-line ((tr x1 y1 x2 y2) &body body)
  (with-gensyms (src-line dst-line)
    `(with-round-positions (,tr ,x1 ,y1 ,x2 ,y2)
       (if (and (<= #x-8000 ,x1 #x7FFF) (<= #x-8000 ,y1 #x7FFF)
                (<= #x-8000 ,x2 #x7FFF) (<= #x-8000 ,y2 #x7FFF))
           (progn ,@body)
           (let* ((,src-line (make-line* ,x1 ,y1 ,x2 ,y2))
                  (,dst-line (region-intersection +clx-clip+ ,src-line)))
             (unless (region-equal ,dst-line +nowhere+)
               (with-bounding-rectangle* (,x1 ,y1 ,x2 ,y2) ,dst-line
                 (with-round-positions (nil ,x1 ,y1 ,x2 ,y2)
                   ,@body))))))))

(defmacro with-clipped-rect ((tr x1 y1 x2 y2) &body body)
  `(with-round-positions (,tr ,x1 ,y1 ,x2 ,y2)
     (let ((,x1 (clamp (min ,x1 ,x2) #x-8000 #x7FFF))
           (,y1 (clamp (min ,y1 ,y2) #x-8000 #x7FFF))
           (,x2 (clamp (max ,x1 ,x2) #x-8000 #x7FFF))
           (,y2 (clamp (max ,y1 ,y2) #x-8000 #x7FFF)))
       (unless (or (coordinate= ,x1 ,x2)
                   (coordinate= ,y1 ,y2))
         ,@body))))

(defmacro with-clipped-poly ((tr coords closed filled) &body body)
  (with-gensyms (src-poly dst-poly split)
    `(with-round-coordinates (,tr ,coords)
       (if (every (lambda (n) (typep n 'clx-coordinate)) ,coords)
           (let ((,coords ,(let ((b-null coords)
                                 (b-true
                                   `(concatenate 'vector ,coords
                                                 (vector (elt ,coords 0)
                                                         (elt ,coords 1)))))
                             (case closed
                               ((t)       b-true)
                               ((nil)     b-null)
                               (otherwise `(if ,closed ,b-true ,b-null))))))
             ,@body)
           (let* ((,src-poly (if ,filled
                                 (make-polygon* ,coords)
                                 (make-polyline* ,coords :closed ,closed)))
                  (,dst-poly (region-intersection +clx-clip+ ,src-poly)))
             (flet ((,split (polygon)
                      (climi::collect (result)
                        (map-over-polygon-coordinates
                         (lambda (x y)
                           (result (round-coordinate x)
                                   (round-coordinate y)))
                         polygon)
                        (when ,closed
                          (let ((point (elt (polygon-points polygon) 0)))
                            (result (round-coordinate (point-x point))
                                    (round-coordinate (point-y point)))))
                        (result))))
               (etypecase ,dst-poly
                 (climi::nowhere-region nil)
                 ((or polyline polygon)
                  (let ((,coords (,split ,dst-poly)))
                    ,@body))
                 (standard-region-union
                  (loop for r in (region-set-regions ,dst-poly :normalize t)
                        for ,coords = (,split r)
                        do (progn ,@body))))))))))
