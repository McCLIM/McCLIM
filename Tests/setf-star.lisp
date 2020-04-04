(cl:in-package #:clim-tests)

(def-suite* :mcclim.setf-star
  :in :mcclim)

(defclass my-point ()
  ((x :accessor my-point-x :initarg :x)
   (y :accessor my-point-y :initarg :y)))

(defgeneric my-point-position (point))

(defmethod my-point-position ((point my-point))
  (values (my-point-x point) (my-point-y point)))

(climi::defgeneric* (setf my-point-position)
    (nx ny point &optional return-polar &key coordinates))

(climi::defmethod* (setf my-point-position)
    (nx ny (point my-point) &optional return-polar &key (coordinates :cartesian))
  "Set X an Y positions of POINT. If COORDINATES is :CARTESIAN, NX and
NY are the new cartesian coordinates. If COORDINATES is :POLAR, NX is
the radial coordinate and NY is the angular coordinate. By default
cartesian coordinates are returned. When the optional argument
RETURN-POLAR is true then return the polar coordinates."
  (with-slots (x y) point
    (case coordinates
      (:polar (multiple-value-bind (nx ny)
                  (polar-to-caresian nx ny)
                (setf x nx y ny)))
      (:cartesian (setf x nx y ny))
      (otherwise
       (error "Invalid coordinates. Must be one of :cartesian or :polar.")))
    (if return-polar
        (cartesian-to-polar x y)
        (values x y))))

(defun polar-to-caresian (r a)
  (let ((x (* r (cos a)))
        (y (* r (sin a))))
    (values x y)))

(defun cartesian-to-polar (x y)
  (let ((r (sqrt (+ (expt x 2) (expt y 2))))
        (a (atan (/ y x))))
    (values r a)))

(test required-args
  (let ((point (make-instance 'my-point :x 1 :y 2)))
    (setf (my-point-position point) (values 3 4))
    (is (equal (my-point-position point) (values 3 4))
        "The new coordinates of the point are correct.")))

(test optional-args
  (let* ((point (make-instance 'my-point :x 1 :y 2))
         (nx 3)
         (ny 4)
         (polar-point-coordinates (cartesian-to-polar nx ny)))
    (is (equal (setf (my-point-position point t) (values nx ny))
               polar-point-coordinates)
        "Return polar coordinates are correct.")
    (is (equal (my-point-position point) (values nx ny))
        "The new coordinates of the point are correct.")))

(test key-args
  (let ((point (make-instance 'my-point :x 1 :y 2)))
    (setf (my-point-position point nil :coordinates :cartesian) (values 1 pi))
    (is (equal (my-point-position point) (values 1 pi))
        "New position values use cartesian coordinates when the keyword argument is passed")

    (setf (my-point-position point nil :coordinates :polar) (values 0 (/ pi 4)))
    (is (<= 0 (abs (- (my-point-x point) (my-point-y point))) 0.000001)
        "New position values use polar coordinates when the keyword argument is passed")

    ;; Error is signalled because :angular is not a valid coordinates
    ;; type
    (signals error
      (setf (my-point-position point :coordinates :angular) (values 1 pi)))))

