;;; Downloaded from: https://gist.github.com/kc-/9f1e0318ed1971fe826a66e2159c7371

;;; This is a throwaway implementation of what a triangulator for paths
;;; polygonized by cl-vector paths might look like. It is incomplete, probably
;;; faulty and if any of it ends up in production, Carl Friedrich Gauss himself
;;; will visit and hit you over the head with an obtuse triangle.

(in-package :cl-user)

(defun point-norm (p)
  (sqrt (+ (expt (car p) 2)
           (expt (cdr p) 2))))

(defun normalized (p)
  (let ((inv-norm (/ 1.0 (point-norm p))))
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
    ((:no-end-point :butt) nil)
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

(defun triangulate-joint (pt-a pt-b pt-c stroke-width &key (type :miter))
  "Connect the stroked lines pt-a..pt-b and pt-b..pt-c with a joint of the specified type.
  TYPE is is one of :miter, :bevel, :round and :none."
  (ecase type
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
                  (list p-b-outer-a p-b-outer-c pt-b)))))))

(defun triangulate-path (points-list stroke-width)
  (let* ((triangles nil)
         (tail triangles)
         (start-cap (triangulate-end-cap (cadr points-list) (car points-list) stroke-width)))
    (setf triangles start-cap
          tail (last start-cap))
    (flet ((append-triangles (new-triangles)
             (when new-triangles
               (if tail
                   (rplacd tail new-triangles)
                   (setf triangles new-triangles))
               (setf tail (cdr new-triangles)))))
      (loop
         for points on points-list
         while (cdr points)
         for pt-a = (car points)
         for pt-b = (cadr points)
         for pt-c = (caddr points)
         for line-triangles = (triangulate-line pt-a pt-b stroke-width)
         for joint-triangles = (and pt-c (triangulate-joint pt-a pt-b pt-c stroke-width))
         do
           (append-triangles line-triangles)
           (append-triangles joint-triangles)
         finally
           (let ((last-points (last points-list 2)))
             (rplacd tail (triangulate-end-cap (car last-points)
                                               (cadr last-points)
                                               stroke-width)))))
    triangles))

(defparameter *sample-path* '((50 . 200)
                              (500 . 200)
                              (400 . 600)
                              (500 . 500)))

;; (triangulate-path *sample-path* 50)
