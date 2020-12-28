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

(defcfun ("pango_font_description_free" pango-font-description-free) :void
  (desc :pointer))

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

;; Returning a pointer here, since if I use g-boxed-foreign, the GC hook will try to free it
(defcfun ("pango_cairo_font_map_get_default" pango-cairo-font-map-get-default) :pointer)

(export 'pango-cairo-font-map-get-default)

(defcfun ("pango_font_map_load_font" pango-font-map-load-font) :pointer
  (font-map :pointer)
  (context (g-object pango-context))
  (desc (g-boxed-foreign pango-font-description)))

(export 'pango-font-map-load-font)

(export 'pango-shape)

(defcfun ("pango_glyph_string_new" pango-glyph-string-new) :pointer)

(export 'pango-glyph-string-new)

(defcfun ("pango_glyph_string_free" pango-glyph-string-free) :void
  (glyph-string :pointer))

(export 'pango-glyph-string-free)

(defcfun ("g_ptr_array_new" g-ptr-array-new) :pointer)

(define-g-boxed-cstruct pango-analysis "PangoAnalysis"
  (shape-engint :pointer)
  (lang-engine :pointer)
  (font :pointer)
  (level :unsigned-char)
  (gravity :unsigned-char)
  (language :pointer)
  (extra-attrs :pointer))

(define-g-boxed-cstruct pango-item "PangoItem"
  (offset :int :initform 0)
  (length :int :initform 0)
  (num-chars :int :initform 0)
  (analysis (:struct pango-analysis-cstruct)))

(export (boxed-related-symbols 'pango-item))

(defcfun ("pango_item_free" pango-item-free) :void
  (item :pointer))

(defcfun ("pango_itemize" %pango-itemize)
    (g-list (g-boxed-foreign pango-item))
  (context (g-object pango-context))
  (text (:pointer :char))
  (start-index :int)
  (length :int)
  (attrs (g-boxed-foreign pango-attr-list))
  (cached-iter :pointer))

(defun pango-itemize (context text attrs)
  (cffi:with-foreign-string ((ptr buf-len) text :null-terminated-p nil)
    (%pango-itemize context ptr 0 buf-len attrs (cffi:null-pointer))))

(export 'pango-itemize)

(defcfun ("pango_shape" %pango-shape) :void
  (text (:pointer :char))
  (length :int)
  (analysis (:pointer (:struct pango-analysis-cstruct)))
  (glyph-string :pointer))

(defun pango-shape (text analysis glyph-string)
  (cffi:with-foreign-string ((ptr buf-len) text :null-terminated-p nil)
    (cffi:with-foreign-objects ((analysis-buf '(:struct pango-analysis-cstruct)))
      (setf (cffi:mem-ref analysis-buf '(:struct pango-analysis-cstruct)) analysis)
      (%pango-shape ptr buf-len analysis-buf glyph-string))))

(export 'pango-shape)

(defcfun ("pango_cairo_show_glyph_string" pango-cairo-show-glyph-string) :void
  (cr (:pointer (:struct cairo-t)))
  (font :pointer)
  (glyph-string :pointer))

(export 'pango-cairo-show-glyph-string)
