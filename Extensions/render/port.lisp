(in-package :mcclim-render)

;;;
;;; Port
;;;

(defclass render-port-mixin (basic-port)
  ())

;;; change geometry
(defmethod port-set-mirror-region ((port render-port-mixin) mirror region)
  (declare (ignore port mirror transformation))
  (let ((sheet (port-lookup-sheet port mirror)))
    (let ((new-mirror (%set-image-region sheet region)))
      (when new-mirror
	(port-register-mirror port sheet new-mirror)))))
                                   
(defmethod port-set-mirror-transformation ((port render-port-mixin) mirror transrormation)
  (declare (ignore port mirror transformation))
  nil)

;;; realize/destroy mirrors
(defmethod realize-mirror ((port render-port-mixin) (sheet image-sheet-mixin))
  (setf (sheet-parent sheet) (graft port))
  (unless (port-lookup-mirror port sheet)
    (port-register-mirror port sheet (%make-image sheet))))

(defmethod destroy-mirror :before ((port render-port-mixin) (sheet image-sheet-mixin))
  (declare (ignore port))
  nil)

(defmethod realize-mirror ((port render-port-mixin) (sheet image-pixmap-mixin))
  (setf (sheet-parent sheet) (graft port))
  (unless (port-lookup-mirror port sheet)
    (port-register-mirror port sheet (%make-image sheet))))

(defmethod destroy-mirror :before ((port render-port-mixin) (sheet image-pixmap-mixin))
  (declare (ignore port sheet))
  nil)

;;; Font's utilities
(defparameter *text-sizes* '(:normal         14
			     :tiny            8
			     :very-small     10
			     :small          12
			     :large          18
			     :very-large     20
			     :huge           24))

(defmethod text-style-to-font ((port render-port-mixin)
			       (text-style standard-text-style))
  (labels
      ((find-and-make-truetype-font (family face size)
         (let* ((font-path-maybe-relative
                 (cdr (assoc (list family face) *families/faces*
                             :test #'equal)))
                (font-path
                 (and font-path-maybe-relative
                      (case (car (pathname-directory
                                  font-path-maybe-relative))
                        (:absolute font-path-maybe-relative)
                        (otherwise (merge-pathnames
                                    font-path-maybe-relative
                                    (or *truetype-font-path* "")))))))
           (if (and font-path (probe-file font-path))
	       (make-truetype-font port font-path size)
               ;; We could error here, but we want to fallback to
               ;; fonts provided by CLX server. Its better to have
               ;; ugly fonts than none at all.
	       (error 'missing-font
		      :filename font-path
		      :text-style text-style))))
       (find-font ()
         (multiple-value-bind (family face size)
             (clim:text-style-components text-style)

           (setf face   (or face :roman)
                 family (or family :fix)
                 size   (or size :normal)
		 size (getf *text-sizes* size size))
                 	   
           (when (eq family :fixed)
             (setf family :fix))
           (find-and-make-truetype-font family face size))))
    (or (text-style-mapping port text-style)
        (setf (climi::text-style-mapping port text-style)
              (or (find-truetype-font text-style)
                  (invoke-with-truetype-path-restart #'find-font))))))


(defmethod clim-extensions:port-all-font-families :around
    ((port mcclim-render::render-port-mixin) &key invalidate-cache)
  (register-all-ttf-fonts port)
  (append (call-next-method) ()))

