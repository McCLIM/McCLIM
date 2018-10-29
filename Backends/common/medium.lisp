(in-package #:climi)

(defclass multiline-text-medium-mixin () ()
  (:documentation "Takes care of splitting string into multiple lines and
adjusts Y-position."))

(defmethod clim:medium-draw-text* :around ((medium multiline-text-medium-mixin) string x y
                                           start end
                                           align-x align-y
                                           toward-x toward-y transform-glyphs)
  (unless (position #\newline string :start start :end end)
    (return-from clim:medium-draw-text* (call-next-method)))
  (setq string (subseq string start end))
  (let* ((font (text-style-to-font (port medium) (medium-text-style medium)))
         (y-dx (font-leading font)))
    ;; Single line centering is figured out in the primary method, we just fix
    ;; the X/Y if it will be different for the supplied positioning and then
    ;; increase it for each line. -- jd 2018-10-08
    (case align-y
      (:center
       (setq y (- y (/ (* y-dx (count #\newline string)) 2))))
      ((:bottom :baseline*)
       (setq y (- y (* y-dx (count #\newline string))))))
    (dolines (line string)
      (unless (alexandria:emptyp line)
        (call-next-method medium line x y 0 (length line)
                          align-x align-y toward-x toward-y
                          transform-glyphs))
      (incf y y-dx))))
