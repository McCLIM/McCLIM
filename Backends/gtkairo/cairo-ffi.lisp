;;; -*- Mode: Lisp; -*-

;;;  (c) copyright 2005 by Gilbert Baumann <gilbert@base-engineering.com>
;;;  (c) copyright 2006 David Lichteblau (david@lichteblau.com)

;;;  Permission is hereby granted, free of charge, to any person obtaining
;;;  a copy of this software and associated documentation files (the
;;;  "Software"), to deal in the Software without restriction, including
;;;  without limitation the rights to use, copy, modify, merge, publish,
;;;  distribute, sublicense, and/or sell copies of the Software, and to
;;;  permit persons to whom the Software is furnished to do so, subject to
;;;  the following conditions:
;;; 
;;;  The above copyright notice and this permission notice shall be
;;;  included in all copies or substantial portions of the Software.
;;; 
;;;  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;;  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. 
;;;  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;;  CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;;  TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;;  SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(in-package :clim-gtkairo)


(defmacro def-cairo-fun (name rtype &rest args)
  (let* (#-scl
	 (str (string-upcase name))
	 #+scl
	 (str (if (eq ext:*case-mode* :upper)
		  (string-upcase name)
		  (string-downcase name)))
	 (actual (intern (concatenate 'string "%-" str) :clim-gtkairo))
	 (wrapper (intern str :clim-gtkairo))
	 (argnames (mapcar #'car args)))
    `(progn
       (cffi:defcfun (,name ,actual)
	   ,rtype
	 ,@args)
       (defun ,wrapper ,argnames
	 (multiple-value-prog1
	     #-scl (,actual ,@argnames)
	     #+scl 
	     (ext:with-float-traps-masked (:underflow :overflow :inexact
						      :divide-by-zero :invalid)
	       (,actual ,@argnames))
	     (let ((status (cairo_status ,(car argnames))))
	     (unless (eq status :success)
	       (error "~A returned with status ~A" ,name status))))))))


;; user-visible structures

(cffi:defcstruct cairo_text_extents
  (x_bearing :double)
  (y_bearing :double)
  (width :double)
  (height :double)
  (x_advance :double)
  (y_advance :double))

(cffi:defcstruct cairo_font_extents
  (ascent :double)
  (descent :double)
  (height :double)
  (max_x_advance :double)
  (max_y_advance :double))

(cffi:defcstruct cairo_glyph
  (index :unsigned-int)
  (x :double)
  (y :double))

(cffi:defcstruct cairo_matrix_t
  (xx :double)
  (yx :double)
  (xy :double)
  (yy :double)
  (x0 :double)
  (y0 :double))


;; enums

(cffi:defcenum cairo_format
    :argb32 :rgb24 :a8 :a1)

(cffi:defcenum cairo_operator
  :clear
  :src :over :in :out :atop
  :dest :dest_over :dest_in :dest_out :dest_atop
  :xor :add :saturate)

(cffi:defcenum cairo_fill_rule
    :winding :even_odd)

(cffi:defcenum cairo_line_cap
    :butt :round :square)

(cffi:defcenum cairo_line_join
    :miter :round :bevel)

(cffi:defcenum cairo_font_slant
    :normal :italic :oblique)

(cffi:defcenum cairo_font_weight
    :normal :bold)

(cffi:defcenum cairo_status
  :success     
  :no_memory  
  :invalid_restore    
  :invalid_pop_group  
  :no_current_point   
  :invalid_matrix     
  :invalid_status     
  :null_pointer       
  :invalid_string     
  :invalid_path_data  
  :read_error         
  :write_error        
  :surface_finished   
  :surface_type_mismatch      
  :pattern_type_mismatch
  :invalid_content
  :invalid_format
  :invalid_visual
  :file_not_found     
  :invalid_dash)

(cffi:defcenum cairo_filter
    :fast :good :best :nearest :bilinear :gaussian)

(cffi:defcenum cairo_extend
    :none :repeat :reflect)


;;; Functions for manipulating state objects

(defcfun "cairo_create"
    :pointer
  (surface :pointer))

(defcfun "cairo_reference"
    :void
  (cr :pointer))

(defcfun "cairo_destroy"
    :void
  (cr :pointer))

(def-cairo-fun "cairo_save"
    :void
  (cr :pointer))

(def-cairo-fun "cairo_restore"
    :void
  (cr :pointer))

;;; XXX: Replace with cairo_current_gstate/cairo_set_gstate

;;;(defcfun "cairo_copy"
;;;    :void
;;;  (destination :pointer)
;;;  (source :pointer))

;;; Modify state

;;;(defcfun "cairo_set_target_surface"
;;;    :void
;;;  (cr :pointer)
;;;  (surface :pointer))
;;;
;;;(defcfun "cairo_set_target_image"
;;;    :void
;;;  (cr :pointer)
;;;  (data :pointer)			;(* (unsigned 8))
;;;  (format cairo_format)
;;;  (width :int)
;;;  (height :int)
;;;  (stride :int))

(def-cairo-fun "cairo_set_operator"
    :void
  (cr :pointer)
  (op cairo_operator))

;;; Colors

(def-cairo-fun "cairo_set_source_rgb"
    :void
  (cr :pointer)
  (red :double)
  (green :double)
  (blue :double))

(def-cairo-fun "cairo_set_source_rgba"
    :void
  (cr :pointer)
  (red :double)
  (green :double)
  (blue :double)
  (alpha :double))

(def-cairo-fun "cairo_set_source"
    :void
  (cr :pointer)
  (pattern :pointer))

(def-cairo-fun "cairo_set_tolerance"
    :void
  (cr :pointer)
  (tolerance :double))

(def-cairo-fun "cairo_set_fill_rule"
    :void
  (cr :pointer)
  (fill_rule cairo_fill_rule))

(def-cairo-fun "cairo_set_line_width"
    :void
  (cr :pointer)
  (w :double))

(def-cairo-fun "cairo_set_line_cap"
    :void
  (cr :pointer)
  (line_cap cairo_line_cap))

(def-cairo-fun "cairo_set_line_join"
    :void
  (cr :pointer)
  (line_join cairo_line_join))

(def-cairo-fun "cairo_set_dash"
    :void
  (cr :pointer)
  (dashes :pointer)			;*double
  (ndash :int)
  (offset :double))

(def-cairo-fun "cairo_set_miter_limit"
    :int
  (cr :pointer)
  (limit :double))

;;; Transformations

(def-cairo-fun "cairo_translate"
    :void
  (cr :pointer)
  (tx :double)
  (ty :double))

(def-cairo-fun "cairo_scale"
    :void
  (cr :pointer)
  (sx :double)
  (sy :double))

(def-cairo-fun "cairo_rotate"
    :void
  (cr :pointer)
  (angle :double))

(def-cairo-fun "cairo_set_matrix"
    :void
  (cr :pointer)
  (matrix :pointer))

(def-cairo-fun "cairo_identity_matrix"
    :void
  (cr :pointer))

;;;(defcfun "cairo_transform_point"
;;;    :void
;;;  (cr :pointer)
;;;  (x :pointer)				;*double
;;;  (y :pointer)				;*double
;;;  )

;;;(defcfun "cairo_transform_distance"
;;;    :void
;;;  (cr :pointer)
;;;  (dx :pointer)				;*double
;;;  (dy :pointer)				;*double
;;;  )

;;;(defcfun "cairo_inverse_transform_point"
;;;    :void
;;;  (cr :pointer)
;;;  (x :pointer)				;*double
;;;  (y :pointer)				;*double
;;;  )
;;;
;;;(defcfun "cairo_inverse_transform_distance"
;;;    :void
;;;  (cr :pointer)
;;;  (dx :pointer)				;*double
;;;  (dy :pointer)				;*double
;;;  )

;;; Path creation functions

(def-cairo-fun "cairo_new_path"
    :void
  (cr :pointer))

(def-cairo-fun "cairo_move_to"
    :void
  (cr :pointer)
  (x :double)
  (y :double))

(def-cairo-fun "cairo_line_to"
    :void
  (cr :pointer)
  (x :double)
  (y :double))

(def-cairo-fun "cairo_curve_to"
    :void
  (cr :pointer)
  (x1 :double)
  (y1 :double)
  (x2 :double)
  (y2 :double)
  (x3 :double)
  (y3 :double))

(def-cairo-fun "cairo_arc"
    :void
  (cr :pointer)
  (xc :double)
  (yc :double)
  (radius :double)
  (angle1 :double)
  (angle2 :double))

(def-cairo-fun "cairo_arc_negative"
    :void
  (cr :pointer)
  (xc :double)
  (yc :double)
  (radius :double)
  (angle1 :double)
  (angle2 :double))

(def-cairo-fun "cairo_rel_move_to"
    :void
  (cr :pointer)
  (dx :double)
  (dy :double))

(def-cairo-fun "cairo_rel_line_to"
    :void
  (cr :pointer)
  (dx :double)
  (dy :double))

(def-cairo-fun "cairo_rel_curve_to"
    :void
  (cr :pointer)
  (dx1 :double)
  (dy1 :double)
  (dx2 :double)
  (dy2 :double)
  (dx3 :double)
  (dy3 :double))

(def-cairo-fun "cairo_rectangle"
    :void
  (cr :pointer)
  (x :double)
  (y :double)
  (w :double)
  (h :double))

(def-cairo-fun "cairo_close_path"
    :void
  (cr :pointer))

(def-cairo-fun "cairo_stroke"
    :void
  (cr :pointer))

(def-cairo-fun "cairo_fill"
    :void
  (cr :pointer))

(def-cairo-fun "cairo_copy_page"
    :void
  (cr :pointer))

(def-cairo-fun "cairo_show_page"
    :void
  (cr :pointer))

;;; Insideness testing

(def-cairo-fun "cairo_in_stroke"
    :int
  (cr :pointer)
  (x :double)
  (y :double))

(def-cairo-fun "cairo_in_fill"
    :int
  (cr :pointer)
  (x :double)
  (y :double))

;;; Rectangular extents

(def-cairo-fun "cairo_stroke_extents"
    :void
  (cr :pointer)
  (x1 :pointer)				;*double
  (y1 :pointer)				;*double
  (x2 :pointer)				;*double
  (y2 :pointer)				;*double
  )

(def-cairo-fun "cairo_fill_extents"
    :void
  (cr :pointer)
  (x1 :pointer)				;*double
  (y1 :pointer)				;*double
  (x2 :pointer)				;*double
  (y2 :pointer)				;*double
  )

(def-cairo-fun "cairo_reset_clip"
    :void
  (cr :pointer))

;; Note: cairo_clip does not consume the current path
(def-cairo-fun "cairo_clip"
    :void
  (cr :pointer))

;;; Font/Text functions

 
;; This interface is for dealing with text as text, not caring about the
;; font object inside the the cairo_t.

(def-cairo-fun "cairo_select_font_face"
    :void
    (cr :pointer)
    (family :string)
    (slant cairo_font_slant)
    (weight cairo_font_weight))

(def-cairo-fun "cairo_set_font_size"
    :void
  (cr :pointer)
  (size :double))

;;;(defcfun "cairo_transform_font"
;;;    :void
;;;  (cr :pointer)
;;;  (matrix :pointer))

(def-cairo-fun "cairo_show_text"
    :void
  (cr :pointer)
  (string :string))

(def-cairo-fun "cairo_show_glyphs"
    :void
  (cr :pointer)
  (glyphs :pointer)
  (num_glyphs :int))

;;;(def-cairo-fun "cairo_current_font"
;;;    :pointer
;;;  (cr :pointer))
;;;
(def-cairo-fun "cairo_font_extents"
    :void
  (cr :pointer)
  (extents :pointer))

;;;(def-cairo-fun "cairo_set_font"
;;;    :void
;;;  (cr :pointer)
;;;  (font :pointer))

(def-cairo-fun "cairo_text_extents"
    :void
  (cr :pointer)
  (string :string)			;### utf_8
  (extents :pointer))

(def-cairo-fun "cairo_glyph_extents"
    :void
  (cr :pointer)
  (glyphs :pointer)
  (num_glyphs :int)
  (extents :pointer))

(def-cairo-fun "cairo_text_path"
    :void
  (cr :pointer)
  (string :string))			;### utf_8

(def-cairo-fun "cairo_glyph_path"
    :void
  (cr :pointer)
  (glyphs :pointer)
  (num_glyphs :int))
  
;;; Portable interface to general font features.

;;;(defcfun "cairo_font_reference"
;;;    :void
;;;  (font :pointer))
;;;
;;;(defcfun "cairo_font_destroy"
;;;    :void
;;;  (font :pointer))


;;; Image functions

;;;(def-cairo-fun "cairo_show_surface"
;;;    :void
;;;  (cr :pointer)
;;;  (surface :pointer)
;;;  (width :int)
;;;  (height :int))

;;; Query functions 

;;;(def-cairo-fun "cairo_current_operator"
;;;    cairo_operator
;;;  (cr :pointer))
;;;
;;;(def-cairo-fun "cairo_current_rgb_color"
;;;    :void
;;;  (cr :pointer)
;;;  (red :pointer)			;*double
;;;  (green :pointer)			;*double
;;;  (blue :pointer)			;*double
;;;  )
;;;
;;;(def-cairo-fun "cairo_current_pattern"
;;;    :pointer
;;;  (cr :pointer))
;;;
;;;(def-cairo-fun "cairo_current_alpha"
;;;    :double
;;;  (cr :pointer))
;;;
;;;(def-cairo-fun "cairo_current_tolerance"
;;;    :double
;;;  (cr :pointer))
;;;
;;;(def-cairo-fun "cairo_current_point"
;;;    :void
;;;  (cr :pointer)
;;;  (x :pointer)				;*double
;;;  (y :pointer)				;*double
;;;  )
;;;
;;;(def-cairo-fun "cairo_current_fill_rule"
;;;    cairo_fill_rule
;;;  (cr :pointer))

;;;(def-cairo-fun "cairo_current_line_width"
;;;    :double
;;;  (cr :pointer))
;;;
;;;(def-cairo-fun "cairo_current_line_cap"
;;;    cairo_line_cap
;;;  (cr :pointer))
;;;
;;;(def-cairo-fun "cairo_current_line_join"
;;;    cairo_line_join
;;;  (cr :pointer))
;;;
;;;(def-cairo-fun "cairo_current_miter_limit"
;;;    :double
;;;  (cr :pointer))
;;;
;;;(def-cairo-fun "cairo_current_matrix"
;;;    :void
;;;  (cr :pointer)
;;;  (matrix :pointer))
;;;
;;;(def-cairo-fun "cairo_current_target_surface"
;;;    :pointer
;;;  (cr :pointer))


;;; 

(def-cairo-fun "cairo_get_target"
    :pointer
  (cr :pointer))

(def-cairo-fun "cairo_set_antialias"
    :void
  (cr :pointer)
  (antialias :int))

(def-cairo-fun "cairo_paint"
    :void
  (cr :pointer))

(def-cairo-fun "cairo_get_font_face"
    :pointer
  (cr :pointer))

(defcfun "cairo_font_face_status"
    cairo_status
  (font :pointer))


;;; Error status queries

(defcfun "cairo_status"
    cairo_status
  (cr :pointer))

;;;(defcfun "cairo_status_string"
;;;    :string
;;;  (cr :pointer))

;;; Surface manipulation

;;;(defcfun "cairo_surface_create_for_image"
;;;    :pointer
;;;  (data :pointer)			;(* (unsigned 8))
;;;  (format cairo_format)
;;;  (width :int)
;;;  (height :int)
;;;  (stride :int))

(defcfun "cairo_surface_create_similar"
    :pointer
  (other :pointer)
  (format cairo_format)
  (width :int)
  (height :int))

(defcfun "cairo_surface_reference"
    :void
  (surface :pointer))

(defcfun "cairo_surface_destroy"
    :void
  (surface :pointer))

;;;(defcfun "cairo_surface_set_repeat"
;;;    cairo_status
;;;  (surface :pointer)
;;;  (repeat :int))
;;;
;;;(defcfun "cairo_surface_set_matrix"
;;;    cairo_status
;;;  (surface :pointer)
;;;  (matrix :pointer))
;;;
;;;(defcfun "cairo_surface_get_matrix"
;;;    cairo_status
;;;  (surface :pointer)
;;;  (matrix :pointer))
;;;
;;;(defcfun "cairo_surface_set_filter"
;;;    cairo_status
;;;  (surface :pointer)
;;;  (filter cairo_filter))
;;;
;;;(defcfun "cairo_surface_get_filter"
;;;    cairo_filter
;;;  (surface :pointer))

;;; Image_surface functions

(defcfun "cairo_image_surface_create"
    :pointer
  (format cairo_format)
  (width :int)
  (height :int))

(defcfun "cairo_image_surface_create_for_data"
    :pointer
  (data :pointer)			;(* (unsigned 8))
  (format cairo_format)
  (width :int)
  (height :int)
  (stride :int))

;;; Pattern creation functions

(defcfun "cairo_pattern_create_for_surface"
    :pointer
  (surface :pointer))

(defcfun "cairo_pattern_create_linear"
    :pointer
  (x1 :double)
  (y1 :double)
  (x2 :double)
  (y2 :double))

(defcfun "cairo_pattern_create_radial"
    :pointer
  (cx1 :double)
  (cy1 :double)
  (cr1 :double)
  (cx2 :double)
  (cy2 :double)
  (cr2 :double))

(defcfun "cairo_pattern_reference"
    :void
  (pattern :pointer))

(defcfun "cairo_pattern_destroy"
    :void
  (pattern :pointer))

;;;(defcfun "cairo_pattern_add_color_stop"
;;;    :int                          ;### cairo_status_t
;;;  (pattern :pointer)
;;;  (offset :double)
;;;  (red :double)
;;;  (green :double)
;;;  (blue :double)
;;;  (alpha :double))

(defcfun "cairo_pattern_set_matrix"
    :void
  (pattern :pointer)
  (martix :pointer))

(defcfun "cairo_pattern_get_matrix"
    :void
  (pattern :pointer)
  (martix :pointer))

(defcfun "cairo_pattern_set_extend"
    :void
  (pattern :pointer)
  (extend cairo_extend))

(defcfun "cairo_pattern_get_extend"
    cairo_extend
  (pattern :pointer))

(defcfun "cairo_pattern_set_filter"
    :void
  (pattern :pointer)
  (filter cairo_filter))

(defcfun "cairo_pattern_get_filter"
    cairo_filter
  (pattern :pointer))

;;; Matrix functions

;;;(defcfun "cairo_matrix_destroy"
;;;    :void
;;;  (matrix :pointer))

;;;(defcfun "cairo_matrix_copy"
;;;    cairo_status
;;;  (matrix :pointer)
;;;  (other :pointer))

;;;(defcfun "cairo_matrix_set_identity"
;;;    cairo_status
;;;  (matrix :pointer))

(defcfun "cairo_matrix_init"
    :void
  (matrix :pointer)
  (a :double)
  (b :double)
  (c :double)
  (d :double)
  (tx :double)
  (ty :double))

;;;(defcfun "cairo_matrix_get_affine"
;;;    cairo_status
;;;  (matrix :pointer)
;;;  (a :pointer)				;*double
;;;  (b :pointer)				;*double
;;;  (c :pointer)				;*double
;;;  (d :pointer)				;*double
;;;  (tx :pointer)				;*double
;;;  (ty :pointer)				;*double
;;;  )

(defcfun "cairo_matrix_translate"
    :void
  (matrix :pointer)
  (tx :double)
  (ty :double))

(defcfun "cairo_matrix_scale"
    :void
  (matrix :pointer)
  (sx :double)
  (sy :double))

(defcfun "cairo_matrix_rotate"
    :void
  (matrix :pointer)
  (angle :double))

(defcfun "cairo_matrix_invert"
    cairo_status
  (matrix :pointer))

(defcfun "cairo_matrix_multiply"
    :void
  (result :pointer)
  (a :pointer)
  (b :pointer))

(defcfun "cairo_matrix_transform_distance"
    :void
  (matrix :pointer)
  (dx :pointer)				;*double
  (dy :pointer)				;*double
  )

(defcfun "cairo_matrix_transform_point"
    :void
  (matrix :pointer)
  (x :pointer)				;*double
  (y :pointer)				;*double
  )



(defcfun "cairo_surface_flush"
    :void
  (surface :pointer))

(defcfun "cairo_surface_mark_dirty"
    :void
  (surface :pointer))
