(in-package :clim-clx)

(defmethod port-set-mirror-region ((port clx-basic-port) mirror mirror-region)
  (setf (xlib:drawable-width mirror)
	(floor (bounding-rectangle-max-x mirror-region)))
  (setf (xlib:drawable-height mirror)
	(floor (bounding-rectangle-max-y mirror-region))))
                                   
(defmethod port-set-mirror-transformation
    ((port clx-basic-port) mirror mirror-transformation)
  (setf (xlib:drawable-x mirror)
	(floor (nth-value 0 (transform-position mirror-transformation 0 0))))
  (setf (xlib:drawable-y mirror)
	(floor (nth-value 1 (transform-position mirror-transformation 0 0)))))

(defun invent-sheet-mirror-transformation-and-region (sheet)
  ;; -> tr region
  (let* ((r (sheet-region sheet))
         (r* (transform-region
              (sheet-native-transformation (sheet-parent sheet))
              (transform-region (sheet-transformation sheet) r)))
         (mirror-transformation
          (if (region-equal r* +nowhere+)
              (make-translation-transformation 0 0)
            (make-translation-transformation 
             (bounding-rectangle-min-x r*)
             (bounding-rectangle-min-y r*))))
         (mirror-region
          (untransform-region mirror-transformation r*)))
    (values
     mirror-transformation
     mirror-region)))

(defmethod destroy-mirror ((port clx-basic-port) (sheet mirrored-sheet-mixin))
  (when (sheet-xmirror sheet)
    (xlib:destroy-window (sheet-xmirror sheet)))
  (when (port-lookup-mirror port sheet)
    (port-unregister-mirror port sheet (sheet-mirror sheet))))

(defmethod raise-mirror ((port clx-basic-port) (sheet basic-sheet))
  (let ((mirror (sheet-xmirror sheet)))
    (when (and mirror
	       (typep mirror 'xlib:window))
      (xlib:circulate-window-up mirror))))

(defmethod bury-mirror ((port clx-basic-port) (sheet basic-sheet))
  (let ((mirror (sheet-xmirror sheet)))
    (when (and mirror
	       (typep mirror 'xlib:window))
      (xlib:circulate-window-down mirror))))

(defmethod mirror-transformation ((port clx-basic-port) mirror)
  (make-translation-transformation (xlib:drawable-x mirror)
                                   (xlib:drawable-y mirror)))



(defmethod port-set-sheet-region ((port clx-basic-port) (graft graft) region)
  (declare (ignore region))
  nil)

(defmethod port-set-sheet-transformation ((port clx-basic-port) (graft graft) transformation)
  (declare (ignore transformation))
  nil)

(defmethod port-set-sheet-transformation ((port clx-basic-port) (sheet mirrored-sheet-mixin) transformation)
  (declare (ignore transformation)) ;; why?
  (break)                               ;obsolete now
  (let ((mirror (sheet-direct-xmirror sheet)))
    (multiple-value-bind (tr rg) (invent-sheet-mirror-transformation-and-region sheet)
      (multiple-value-bind (x y) (transform-position tr 0 0)
        (multiple-value-bind (x1 y1 x2 y2) (if (eql rg +nowhere+)
                                               (values 0 0 0 0)
                                               (bounding-rectangle* rg))
          (declare (ignore x1 y1))      ;XXX assumed to be 0
          (setf (xlib:drawable-x mirror) (round x)
                (xlib:drawable-y mirror) (round y))
          (setf (xlib:drawable-width mirror)  (clamp 1 (round x2) #xFFFF)
                (xlib:drawable-height mirror) (clamp 1 (round y2) #xFFFF))
          ;;(xlib:clear-area mirror :exposures-p t)
          (invalidate-cached-transformations sheet)
          )))))

(defmethod port-set-sheet-region ((port clx-basic-port) (sheet mirrored-sheet-mixin) region)
  (declare (ignore region)) ;; why?
  (let ((mirror (sheet-direct-xmirror sheet)))
    (multiple-value-bind (tr rg) (invent-sheet-mirror-transformation-and-region sheet)
      (declare (ignore tr))
      (multiple-value-bind (x1 y1 x2 y2) (if (eql rg +nowhere+)
                                             (values 0 0 0 0)
                                             (bounding-rectangle* rg))
        (declare (ignore x1 y1))      ;XXX assumed to be 0
        (setf x2 (round x2))
        (setf y2 (round y2))
        (cond ((or (<= x2 0) (<= y2 0))
               ;; XXX
               ;; now X does not allow for a zero width/height window,
               ;; we should unmap instead ...
               ;; Nevertheless we simply clamp
               ))
        (setf (xlib:drawable-width mirror)  (clamp x2 1 #xFFFF)
              (xlib:drawable-height mirror) (clamp y2 1 #xFFFF))))))

(defmethod port-enable-sheet ((port clx-basic-port) (mirror mirrored-sheet-mixin))
  (xlib:map-window (sheet-direct-xmirror mirror)) )

(defmethod port-disable-sheet ((port clx-basic-port) (mirror mirrored-sheet-mixin))
  (xlib:unmap-window (sheet-direct-xmirror mirror)) )

(defmethod port-mirror-width ((port clx-basic-port) sheet)
  (let ((mirror (port-lookup-mirror port sheet)))
    (xlib:drawable-width mirror)))

(defmethod port-mirror-height ((port clx-basic-port) sheet)
  (let ((mirror (port-lookup-mirror port sheet)))
    (xlib:drawable-height mirror)))
