(in-package :pango)

(defconstant +pango-scale+ 1024)

(export '+pango-scale+)

(define-g-boxed-cstruct pango-rectangle "PangoRectangle"
  (x :int :initform 0)
  (y :int :initform 0)
  (width :int :initform 0)
  (height :int :initform 0))

(export (boxed-related-symbols 'pango-rectangle))

(defcfun ("pango_layout_get_pixel_extents" %pango-layout-get-pixel-extents) :void
  (layout (g-object pango-layout))
  (ink-rect (:pointer (:struct pango-rectangle-cstruct)))
  (logical-rect (:pointer (:struct pango-rectangle-cstruct))))

(defun pango-layout-get-pixel-extents (layout)
  (with-foreign-objects ((ink-rect '(:struct pango-rectangle-cstruct))
                         (logical-rect '(:struct pango-rectangle-cstruct)))
    (%pango-layout-get-pixel-extents layout ink-rect logical-rect)
    (let ((a (make-pango-rectangle :x (cffi:foreign-slot-value ink-rect '(:struct pango-rectangle-cstruct) 'x)
                                   :y (cffi:foreign-slot-value ink-rect '(:struct pango-rectangle-cstruct) 'y)
                                   :width (cffi:foreign-slot-value ink-rect '(:struct pango-rectangle-cstruct) 'width)
                                   :height (cffi:foreign-slot-value ink-rect '(:struct pango-rectangle-cstruct) 'height)))
          (b (make-pango-rectangle :x (cffi:foreign-slot-value logical-rect '(:struct pango-rectangle-cstruct) 'x)
                                   :y (cffi:foreign-slot-value logical-rect '(:struct pango-rectangle-cstruct) 'y)
                                   :width (cffi:foreign-slot-value logical-rect '(:struct pango-rectangle-cstruct) 'width)
                                   :height (cffi:foreign-slot-value logical-rect '(:struct pango-rectangle-cstruct) 'height))))
      (log:trace "a=~s b=~s" a b)
      (values a b))))

(export 'pango-layout-get-pixel-extents)

(defcfun ("pango_layout_get_iter" pango-layout-get-iter) :pointer
  (layout (g-object pango-layout)))

(export 'pango-layout-iter)

(defcfun ("pango_layout_get_baseline" pango-layout-get-baseline) :int
  (layout (g-object pango-layout)))

(export 'pango-layout-get-baseline)

(defcfun ("pango_font_description_new" pango-font-description-new)
    (g-boxed-foreign pango-font-description))

(export 'pango-font-description-new)

#+nil
(defcfun ("pango_font_description_free" pango-font-description-free) :void
  (desc :pointer))

#+nil
(export 'pango-font-description-free)

(defcfun ("pango_font_description_set_family" pango-font-description-set-family) :void
  (desc (g-boxed-foreign pango-font-description))
  (family :string))

(export 'pango-font-description-set-family)

(defcfun ("pango_font_description_get_family" pango-font-description-get-family) :string
  (desc (g-boxed-foreign pango-font-description)))

(export 'pango-font-description-get-family)

(defcfun ("pango_font_description_set_style" pango-font-description-set-style) :void
  (desc (g-boxed-foreign pango-font-description))
  (style pango-style))

(export 'pango-font-description-set-style)

(defcfun ("pango_font_description_set_weight" pango-font-description-set-weight) :void
  (desc (g-boxed-foreign pango-font-description))
  (weight pango-weight))

(export 'pango-font-description-set-weight)

(defcfun ("pango_font_description_set_size" pango-font-description-set-size) :void
  (desc (g-boxed-foreign pango-font-description))
  (size :int))

(export 'pango-font-description-set-size)

(defcfun ("pango_font_description_set_absolute_size" pango-font-description-set-absolute-size) :void
  (desc (g-boxed-foreign pango-font-description))
  (size :double))

(export 'pango-font-description-set-absolute-size)
