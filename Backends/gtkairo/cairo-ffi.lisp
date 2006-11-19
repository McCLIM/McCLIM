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
	     (,actual ,@argnames)
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
;; (can't look these up yet, why?)

(cffi:defcenum cairo_format_t
    :argb32 :rgb24 :a8 :a1)

(cffi:defcenum cairo_operator_t
  :clear
  :src :over :in :out :atop
  :dest :dest_over :dest_in :dest_out :dest_atop
  :xor :add :saturate)

(cffi:defcenum cairo_fill_rule_t
    :winding :even_odd)

(cffi:defcenum cairo_line_cap_t
    :butt :round :square)

(cffi:defcenum cairo_line_join_t
    :miter :round :bevel)

(cffi:defcenum cairo_font_slant_t
    :normal :italic :oblique)

(cffi:defcenum cairo_font_weight_t
    :normal :bold)

(cffi:defcenum cairo_status_t
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

(cffi:defcenum cairo_filter_t
    :fast :good :best :nearest :bilinear :gaussian)

(cffi:defcenum cairo_extend_t
    :none :repeat :reflect)

(cffi:defcenum cairo_content_t
  (:cairo_content_color #x1000)
  (:cairo_content_alpha #x2000)
  (:cairo_content_color_alpha #x3000))

(cffi:defcenum cairo_antialias_t
  :CAIRO_ANTIALIAS_DEFAULT
  :CAIRO_ANTIALIAS_NONE
  :CAIRO_ANTIALIAS_GRAY
  :CAIRO_ANTIALIAS_SUBPIXEL)
