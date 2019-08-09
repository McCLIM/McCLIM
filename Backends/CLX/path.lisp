;;; Code from ck: https://gist.github.com/kc-/9f1e0318ed1971fe826a66e2159c7371

(in-package #:clim-clx)

(defun and-there-it-is--my-polygon ()
  (let ((path (paths:stroke-path (let ((p (paths:create-path :open-polyline)))
                                   (paths:path-reset p (paths:make-point 50 200))
                                   (paths:path-extend p (paths:make-straight-line)
                                                      (paths:make-point 500 200))
                                   (paths:path-extend p (paths:make-straight-line)
                                                      (paths:make-point 400 600))
                                   (paths:path-extend p (paths:make-straight-line)
                                                      (paths:make-point 500 500))
                                   p)
                                 50 :caps :round :joint :miter)))
    ;; PATH-ITERATOR can be replaced with PATH-ITERATOR-SEGMENTED
    ;; which will return a series of line segments instead of arcs
    (loop with iterator = (paths:path-iterator-segmented (car path))
       for e = (multiple-value-list (paths:path-iterator-next iterator))
       collect (subseq e 0 2)
       until (third e))))

(defun lexicographically-< (a b)
  "Predicate for sorting in lexicographic order."
  (or (< (car a) (car b))
      (and (= (car a) (car b))
           (< (cdr a) (cdr b)))))

(defun min-point (a b &optional (order-fn #'lexicographically-<))
  (if (funcall order-fn a b) a b))

(defun point-norm (p)
  (sqrt (+ (expt (car p) 2)
           (expt (cdr p) 2))))

(defun normalized (p &optional (length 1.0))
  (let ((inv-norm (if (and (zerop (car p)) (zerop (cdr p)))
                      0
                      (/ length (point-norm p)))))
    (cons (* inv-norm (car p))
          (* inv-norm (cdr p)))))

(defun point-arithmetic (fn a b)
  (cons (funcall fn (car a) (car b))
        (funcall fn (cdr a) (cdr b))))

(defun point-+ (a b) (point-arithmetic #'+ a b))
(defun point-- (a b) (point-arithmetic #'- a b))

(defun points-+ (&rest points) (reduce #'point-+ points))

(defun point-scale (p s)
  (cons (* (car p) s) (* (cdr p) s)))

(defun point-scalar-* (a b)
  (+ (* (car a) (car b))
     (* (cdr a) (cdr b))))

(defun cross-product-2d (a b)
  (let ((x1 (car a)) (y1 (cdr a))
        (x2 (car b)) (y2 (cdr b)))
    (- (* x1 y2) (* x2 y1))))

(defun point-perp (p)
  (cons (- (cdr p)) (car p)))

(defun point-of-intersection (pa pb px py)
  "Determine the intersection of the lines PA-PB and PX-PY.
   Returns NIL if the lines are close to parallel."
  (let* ((v-ab (point-- pb pa))
         (v-xy (point-- py px))
         (v-xa (point-- pa px))
         (cross-vab-vyx (cross-product-2d v-ab v-xy))
         (s
          (and (< 1e-3 (abs cross-vab-vyx))
               (- (/ (cross-product-2d v-xa v-xy)
                     cross-vab-vyx)))))
    (and s (point-+ pa (point-scale v-ab s)))))

(defun left-of-line-p (point line-start line-end)
  "Is POINT left of the line from LINE-START through LINE-END?"
  (minusp (cross-product-2d (point-- line-start point)
                            (point-- line-end point))))

(defun right-of-line-p (point line-start line-end)
  (not (left-of-line-p point line-start line-end)))

;;; jd's code
(defun poly-contains-position-p (polygon x y)
  (flet ((is-left (x0 y0 x1 y1 x2 y2)
           (- (* (- x1 x0) (- y2 y0))
              (* (- x2 x0) (- y1 y0)))))
    (let ((wn 0))
      (loop
        for points on (cons (car (last polygon)) polygon)
        for p1 = (car points)
        for p2 = (cadr points) 
        for x1 = (car p1) for y1 = (cdr p1)
        for x2 = (car p2) for y2 = (cdr p2)
        while (cdr points)
        do
           (if (<= y1 y)
               (when (and (> y2 y)
                          (> (is-left x1 y1 x2 y2 x y) 0))
                 (incf wn))
               (when (and (<= y2 y)
                          (< (is-left x1 y1 x2 y2 x y) 0))
                 (decf wn))))
      (values (= wn 0) wn))))

(defun find-non-simple-segments (polygon)
  (flet ((checkpoint (pt-a pt-b)
           (let* ((v-delta (point-- pt-b pt-a))
                  (midpoint (point-+
                             (point-scale v-delta 1/2)
                             pt-a))
                  (v-perp-normalized (normalized (point-perp v-delta))))
             (point-+ midpoint (point-scale v-perp-normalized 0.1)))))
    (loop
       for pts on polygon
       for pt-a = (car pts)
       for pt-b = (cadr pts)
       while pt-b
       for checkpoint = (checkpoint pt-a pt-b)
       when (not (poly-contains-position-p polygon (car checkpoint) (cdr checkpoint)))
       collect (subseq pts 0 2))))

(defun triangulate-polygon (outline-path)
  "Naive implementation for triangulating a path, that is, a polygonal outline
of a pen-stroked polyline, as cl-vectors generates it:
 :STRAIGHT-LINE (5.002131984792475d0 . 20.145997611506445d0)
 :STRAIGHT-LINE (5.0727513500576995d0 . 19.150164285498796d0)
 :STRAIGHT-LINE (5.130761845609023d0 . 21.136010473465433d0)
 :STRAIGHT-LINE (5.339804570163868d0 . 18.188211227616634d0)
 ..."
  (let* ((polygon (if outline-path
                      (loop with iterator = (paths:path-iterator-segmented outline-path)
                         for e = (multiple-value-list (paths:path-iterator-next iterator))
                         collect (subseq e 0 2)
                         until (third e))
                      (and-there-it-is--my-polygon)))
         (p (mapcar #'second polygon))
         (p (cons (car (last p)) p))
         (minelt (reduce #'min-point p))
         (minpos (position minelt p))
         (p (append (subseq p minpos) (subseq p 0 minpos)))
         (q (reverse p)))
    ;; throughout this, we extend the triangle mesh by connecting a point of the
    ;; polygon to an advancing line. P and Q are the 'upper'- and 'lower'-first
    ;; parts of the boundary, as seen from the lexicographic minimum.
    (loop
       with triangles = (list)
       with lower = (rest p)
       with upper = (butlast q)
       with hp = (car upper)
       with lp = (pop lower)
       with prefer-lower = T
       while (and lower upper
                  (not (eq (car upper) (car lower))))
       if (and (or prefer-lower
                   (right-of-line-p (first upper) hp lp))
               (right-of-line-p (first lower) lp hp))
       do
         (push (list lp (first lower) hp) triangles)
         (setf lp (pop lower)
               prefer-lower nil)
       else do
         (push (list hp lp (first upper)) triangles)
         (setf hp (pop upper)
               prefer-lower T)

       finally (return triangles))))

(defun dump-triangles-to-file (triangles &optional (pathname "/tmp/tris.dat"))
  (with-open-file (s pathname :direction :output :if-exists :supersede)
    (loop for triangle in triangles do
         (loop for point in triangle do
              (format s "~f ~f~%" (car point) (cdr point)))
         (format s "~f ~f~%" (caar triangle) (cdar triangle)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun triangulate-line (pt-a pt-b stroke-width)
  "Returns a list of two triangles, covering the STROKE-WIDTH from PT-A to PT-B."
  (let* ((v-delta (point-- pt-b pt-a))
         (v-perp-width/2 (normalized (point-perp v-delta) (/ stroke-width 2)))
         (pt-1 (point-- pt-a v-perp-width/2))
         (pt-2 (point-+ pt-a v-perp-width/2))
         (pt-3 (point-+ pt-b v-perp-width/2))
         (pt-4 (point-- pt-b v-perp-width/2)))
    (list (list pt-1 pt-2 pt-3)
          (list pt-1 pt-3 pt-4))))

(defun triangulate-end-cap (pt-a pt-b stroke-width &key (type :round) (steps 10))
  "Returns a list of triangles (or NIL) covering the end cap at PT-B. TYPE is one of :no-end-point, :butt, :square, and :round."
  (ecase type
    ((:no-end-point :butt nil) nil)
    (:square
     (let* ((v-delta (normalized (point-- pt-b pt-a) (/ stroke-width 2))))
       (triangulate-line pt-b (point-+ pt-b v-delta) stroke-width)))
    (:round (let* ((v-delta (point-- pt-b pt-a))
                   (v-xx (normalized v-delta (/ stroke-width 2)))
                   (v-yy (normalized (point-perp v-delta) (/ stroke-width 2)))
                   (delta (/ pi steps)))
              (loop
                 for i from 1 to steps
                 for rad = (* delta i)
                 for pt-3 = (points-+ pt-b
                                      (point-scale v-yy (cos rad))
                                      (point-scale v-xx (sin rad)))
                 and pt-2 = (point-+ pt-b v-yy) then pt-3
                 collect
                   (list pt-b pt-2 pt-3))))))

(defun triangulate-joint (pt-a pt-b pt-c stroke-width &key (type :miter) (round-steps 5))
  "Connect the stroked lines pt-a..pt-b and pt-b..pt-c with a joint of the specified type.
TYPE is is one of :miter, :bevel, :round and :none."
  (ecase type
    ((:none nil) nil)
    (:bevel
     (let* ((v-ab (point-- pt-b pt-a))
            (v-bc (point-- pt-c pt-b))
            (left-turn (left-of-line-p pt-c pt-a pt-b))
            (signed-half-stroke-width (* (if left-turn 1 -1) (/ stroke-width 2)))
            (v-ab-perp-width/2 (normalized (point-perp v-ab) signed-half-stroke-width))
            (v-bc-perp-width/2 (normalized (point-perp v-bc) signed-half-stroke-width))
            (p-a-outer (point-+ pt-a v-ab-perp-width/2))
            (p-b-outer-a (point-+ p-a-outer v-ab))
            (p-c-outer (point-+ pt-c v-bc-perp-width/2))
            (p-b-outer-c (point-- p-c-outer v-bc)))
       (list (list pt-b p-b-outer-a p-b-outer-c))))
    (:miter
     (let* ((v-ab (point-- pt-b pt-a))
            (v-bc (point-- pt-c pt-b))
            (left-turn (left-of-line-p pt-c pt-a pt-b))
            (signed-half-stroke-width (* (if left-turn 1 -1) (/ stroke-width 2)))
            (v-ab-perp-width/2 (normalized (point-perp v-ab) signed-half-stroke-width))
            (v-bc-perp-width/2 (normalized (point-perp v-bc) signed-half-stroke-width))

            (p-a-outer (point-+ pt-a v-ab-perp-width/2))
            (p-b-outer-a (point-+ p-a-outer v-ab))
            (p-c-outer (point-+ pt-c v-bc-perp-width/2))
            (p-b-outer-c (point-- p-c-outer v-bc))

            (point-of-intersection (point-of-intersection p-a-outer p-b-outer-a p-b-outer-c p-c-outer)))
       (and point-of-intersection
            (list (list p-b-outer-a point-of-intersection p-b-outer-c)
                  (list p-b-outer-a p-b-outer-c pt-b)))))
    (:round
     (let* ((v-ab (point-- pt-b pt-a))
            (v-bc (point-- pt-c pt-b))
            (left-turn (left-of-line-p pt-c pt-a pt-b))
            (direction-sign (if left-turn 1 -1))
            (signed-half-stroke-width (* direction-sign (/ stroke-width 2)))
            (v-ab-perp-width/2 (normalized (point-perp v-ab) signed-half-stroke-width))
            (v-bc-perp-width/2 (normalized (point-perp v-bc) signed-half-stroke-width))
            (v-yy (point-perp v-bc-perp-width/2))
            (span-angle (acos (/ (point-scalar-* v-ab-perp-width/2 v-bc-perp-width/2)
                                 (expt signed-half-stroke-width 2))))
            (p-b-outer-c (point-+ pt-b v-bc-perp-width/2))
            (delta (/ span-angle round-steps)))
       (loop
          for i from 1 to round-steps
          for rad = (* direction-sign delta i)
          for pt-3 = (points-+ pt-b
                               (point-scale v-bc-perp-width/2 (cos rad))
                               (point-scale v-yy (sin rad)))
          and pt-2 = p-b-outer-c then pt-3
          collect (list pt-b pt-2 pt-3))))))

(defun triangulate-path (points-list stroke-width &key (line-joint-shape :miter) (line-cap-shape :butt))
  (let* ((triangles nil)
         (tail triangles)
         (start-cap (triangulate-end-cap (cadr points-list) (car points-list) stroke-width :type line-cap-shape)))
    (setf triangles start-cap
          tail (last start-cap))
    (flet ((append-triangles (new-triangles)
             (when new-triangles
               (if tail
                   (rplacd tail new-triangles)
                   (setf triangles new-triangles))
               (setf tail (last new-triangles)))))
      (loop
         for points on points-list
         while (cdr points)
         for pt-a = (car points)
         for pt-b = (cadr points)
         for pt-c = (caddr points)
         for line-triangles = (triangulate-line pt-a pt-b stroke-width)
         for joint-triangles = (and pt-c (triangulate-joint pt-a pt-b pt-c stroke-width :type line-joint-shape))
         do
           (append-triangles line-triangles)
           (append-triangles joint-triangles)
         finally
           (let ((last-points (last points-list 2)))
             (rplacd tail (triangulate-end-cap (car last-points)
                                               (cadr last-points)
                                               stroke-width
                                               :type line-cap-shape)))))
    triangles))
