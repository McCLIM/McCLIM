;;; automatically generated, hand tweaked, do not regenerate.

(in-package :freetype)

(define-foreign-library libfreetype
    (:unix (:or "libfreetype.so.6" "libfreetype"))
  (t (:default "libfreetype")))
(use-foreign-library libfreetype)

(defmacro define-alien-type (&rest rest)
  ;; cffi seems to have a much simpler model of pointer
  ;; types... [2006/05/23:rpg]
  (cond ((and (= (length rest) 2)
	      (eq (car (second rest)) '*))
	 `(defctype ,(first rest) :pointer))
	((error "Don't understand how to translate alien type definition ~S"
		`(define-alien-type ,@rest)))))  

(defmacro define-alien-routine (name retval &rest args)
  `(defcfun ,name ,retval
     ,@(loop for (name type) in args
	     for new-type = (if (and (listp type)
				     (eq (car type) '*))
				:pointer
				type)
	     collect (list name new-type))))

(defmacro defcstruct (name &rest slots)
  `(cffi:defcstruct ,name 
     ,@(loop for (name type) in slots
	     for new-type = (if (and (listp type)
				     (eq (car type) '*))
				:pointer
				type)
	     collect (list name new-type))))
				     
(declaim (optimize (speed 3)))

(define-alien-type freetype:memory (* (struct freetype::memory-rec-)))
(define-alien-type freetype:stream (* (struct freetype::stream-rec-)))
(define-alien-type freetype:raster (* (struct freetype::raster-rec-)))
(define-alien-type freetype:list-node (* (struct freetype::list-node-rec-)))
(define-alien-type freetype:list (* (struct freetype::list-rec-)))
(define-alien-type freetype:library (* (struct freetype::library-rec-)))
(define-alien-type freetype:module (* (struct freetype::module-rec-)))
(define-alien-type freetype:driver (* (struct freetype::driver-rec-)))
(define-alien-type freetype:renderer (* (struct freetype::renderer-rec-)))
(define-alien-type freetype:char-map (* (struct freetype::char-map-rec-)))
(define-alien-type freetype:face-internal (* (struct freetype::face-internal-rec-)))
(define-alien-type freetype:slot-internal (* (struct freetype::slot-internal-rec-)))
(define-alien-type freetype:size-internal (* (struct freetype::size-internal-rec-)))

(defctype freetype:int16 :int16)

(defctype freetype:uint16 :uint16)

(defctype freetype:int32 :int32)

(defctype freetype:uint32 :uint32)

(defctype freetype:fast :int32)

(defctype freetype:ufast :uint32)

(defctype freetype:ptrdiff-t :int32)

(defctype freetype:size-t :uint32)

(defctype freetype:wchar-t :int32)

(defctype freetype:wint-t :uint32)

(defctype freetype:bool :uint8)

(defctype freetype:fword :int16)

(defctype freetype:ufword :uint16)

(defctype freetype:char :int8)

(defctype freetype:byte :uint8)

(defctype freetype:string :int8)

(defctype freetype:short :int16)

(defctype freetype:ushort :uint16)

(defctype freetype:int :int32)

(defctype freetype:uint :uint32)

(defctype freetype:long :long)

(defctype freetype:ulong :unsigned-long)

(defctype freetype:f2dot14 :int16)

(defctype freetype:f26dot6 :long)

(defctype freetype:fixed :long)

(defctype freetype:error :int32)

(defctype freetype:pointer :pointer)

(defctype freetype:offset freetype:size-t)

(defctype freetype:ptr-dist freetype:size-t)

(define-alien-type freetype:face (* freetype:face-rec))


(defcenum mod-err
    (:mod-err-base #.#x000)
  (:mod-err-autohint #.#x100) (:mod-err-cache #.#x200) (:mod-err-cff #.#x300)
  (:mod-err-cid #.#x400) (:mod-err-pcf #.#x500) (:mod-err-psaux #.#x600) (:mod-err-psnames #.#x700) (:mod-err-raster #.#x800)
  (:mod-err-sfnt #.#x900) (:mod-err-smooth #.#xA00) (:mod-err-true-type #.#xB00) (:mod-err-type1 #.#xC00)
  (:mod-err-winfonts #.#xD00)
  :mod-err-max)

(defcenum error-enum
 (:err-ok #.#x00) (:err-cannot-open-resource #.(+ #x01 0)) (:err-unknown-file-format #.(+ #x02 0))
  (:err-invalid-file-format #.(+ #x03 0)) (:err-invalid-version #.(+ #x04 0)) (:err-lower-module-version #.(+ #x05 0))
  (:err-invalid-argument #.(+ #x06 0)) (:err-unimplemented-feature #.(+ #x07 0)) (:err-invalid-glyph-index #.(+ #x10 0))
  (:err-invalid-character-code #.(+ #x11 0)) (:err-invalid-glyph-format #.(+ #x12 0)) (:err-cannot-render-glyph #.(+ #x13 0))
  (:err-invalid-outline #.(+ #x14 0)) (:err-invalid-composite #.(+ #x15 0)) (:err-too-many-hints #.(+ #x16 0))
  (:err-invalid-pixel-size #.(+ #x17 0)) (:err-invalid-handle #.(+ #x20 0)) (:err-invalid-library-handle #.(+ #x21 0))
  (:err-invalid-driver-handle #.(+ #x22 0)) (:err-invalid-face-handle #.(+ #x23 0)) (:err-invalid-size-handle #.(+ #x24 0))
  (:err-invalid-slot-handle #.(+ #x25 0)) (:err-invalid-char-map-handle #.(+ #x26 0)) (:err-invalid-cache-handle #.(+ #x27 0))
  (:err-invalid-stream-handle #.(+ #x28 0)) (:err-too-many-drivers #.(+ #x30 0)) (:err-too-many-extensions #.(+ #x31 0))
  (:err-out-of-memory #.(+ #x40 0)) (:err-unlisted-object #.(+ #x41 0)) (:err-cannot-open-stream #.(+ #x51 0))
  (:err-invalid-stream-seek #.(+ #x52 0)) (:err-invalid-stream-skip #.(+ #x53 0)) (:err-invalid-stream-read #.(+ #x54 0))
  (:err-invalid-stream-operation #.(+ #x55 0)) (:err-invalid-frame-operation #.(+ #x56 0))
  (:err-nested-frame-access #.(+ #x57 0)) (:err-invalid-frame-read #.(+ #x58 0)) (:err-raster-uninitialized #.(+ #x60 0))
  (:err-raster-corrupted #.(+ #x61 0)) (:err-raster-overflow #.(+ #x62 0)) (:err-raster-negative-height #.(+ #x63 0))
  (:err-too-many-caches #.(+ #x70 0)) (:err-invalid-opcode #.(+ #x80 0)) (:err-too-few-arguments #.(+ #x81 0))
  (:err-stack-overflow #.(+ #x82 0)) (:err-code-overflow #.(+ #x83 0)) (:err-bad-argument #.(+ #x84 0))
  (:err-divide-by-zero #.(+ #x85 0)) (:err-invalid-reference #.(+ #x86 0)) (:err-debug-op-code #.(+ #x87 0))
  (:err-endf-in-exec-stream #.(+ #x88 0)) (:err-nested-defs #.(+ #x89 0)) (:err-invalid-code-range #.(+ #x8A 0))
  (:err-execution-too-long #.(+ #x8B 0)) (:err-too-many-function-defs #.(+ #x8C 0))
  (:err-too-many-instruction-defs #.(+ #x8D 0)) (:err-table-missing #.(+ #x8E 0)) (:err-horiz-header-missing #.(+ #x8F 0))
  (:err-locations-missing #.(+ #x90 0)) (:err-name-table-missing #.(+ #x91 0)) (:err-cmap-table-missing #.(+ #x92 0))
  (:err-hmtx-table-missing #.(+ #x93 0)) (:err-post-table-missing #.(+ #x94 0)) (:err-invalid-horiz-metrics #.(+ #x95 0))
  (:err-invalid-char-map-format #.(+ #x96 0)) (:err-invalid-ppem #.(+ #x97 0)) (:err-invalid-vert-metrics #.(+ #x98 0))
  (:err-could-not-find-context #.(+ #x99 0)) (:err-invalid-post-table-format #.(+ #x9A 0))
  (:err-invalid-post-table #.(+ #x9B 0)) (:err-syntax-error #.(+ #xA0 0)) (:err-stack-underflow #.(+ #xA1 0)) :err-max)

(defctype freetype:alloc-func :pointer)

(defctype freetype:free-func :pointer)

(defctype freetype:realloc-func :pointer)

(defcstruct freetype::memory-rec-
    (freetype:user :pointer) (freetype:alloc freetype:alloc-func) (freetype:free freetype:free-func)
  (freetype:realloc freetype:realloc-func))

(defcunion freetype:stream-desc 
      (freetype:value :long) 
      (freetype:pointer :pointer))

(defctype freetype:stream-io :pointer)

(defctype freetype:stream-close :pointer)

(defcstruct freetype::stream-rec-
            (freetype:base (* :uint8))
            (freetype:size freetype:ulong)
            (freetype:pos freetype:ulong)
            (freetype:descriptor freetype:stream-desc)
            (freetype:pathname freetype:stream-desc)
            (freetype:read freetype:stream-io)
            (freetype:close freetype:stream-close)
            (freetype:memory freetype:memory)
            (freetype:cursor (* :uint8))
            (freetype:limit (* :uint8)))

(defctype freetype:pos :long)

(defcstruct freetype:vector (freetype:x freetype:pos) (freetype:y freetype:pos))

(defcstruct freetype:bbox
    (freetype:x-min freetype:pos) (freetype:y-min freetype:pos) (freetype:x-max freetype:pos)
    (freetype:y-max freetype:pos))

;; seems like pixel-mode- might possibly be an alias for this...
(defcenum freetype:pixel-mode
    (:ft-pixel-mode-none #.#o0) :ft-pixel-mode-mono :ft-pixel-mode-grays :ft-pixel-mode-pal2
  :ft-pixel-mode-pal4 :ft-pixel-mode-pal8 :ft-pixel-mode-rgb15 :ft-pixel-mode-rgb16 :ft-pixel-mode-rgb24 :ft-pixel-mode-rgb32
  :ft-pixel-mode-max)

;;; palette-mode-
(defcenum freetype:palette-mode
 (:ft-palette-mode-rgb #.#o0) :ft-palette-mode-rgba :ft-palettte-mode-max)

(defcstruct freetype:bitmap
 (freetype:rows :int32) (freetype:width :int32) (freetype:pitch :int32)
  (freetype:buffer (* :uint8)) (freetype:num-grays :int16) (freetype:pixel-mode :int8)
  (freetype:palette-mode :int8) (freetype:palette :pointer))

(defcstruct freetype:outline
 (freetype:n-contours :int16) (freetype:n-points :int16)
  (freetype:points (* freetype:vector)) (freetype:tags (* :int8)) (freetype:contours (* :int16))
  (freetype:flags :int32))

(defcenum freetype:outline-flags
 (:ft-outline-none #.#o0) (:ft-outline-owner #.1) (:ft-outline-even-odd-fill #.2)
  (:ft-outline-reverse-fill #.4) (:ft-outline-ignore-dropouts #.8) (:ft-outline-high-precision #.256)
  (:ft-outline-single-pass #.512))

(defctype freetype:outline-move-to-func :pointer)

(defctype freetype:outline-line-to-func :pointer)

(defctype freetype:outline-conic-to-func :pointer)

(defctype freetype:outline-cubic-to-func :pointer)

(defcstruct freetype:outline-funcs
  (freetype:move-to freetype:outline-move-to-func)
  (freetype:line-to freetype:outline-line-to-func) (freetype:conic-to freetype:outline-conic-to-func)
  (freetype:cubic-to freetype:outline-cubic-to-func) (freetype:shift :int32) (freetype:delta freetype:pos))

(defcenum freetype:glyph-format
  (:ft-glyph-format-none #.(logior (logior (logior (ash #o0 24) (ash #o0 16)) (ash #o0 8)) #o0))
  (:ft-glyph-format-composite
   #.(logior (logior (logior (ash #.(char-code #\c) 24) (ash #.(char-code #\o) 16)) (ash #.(char-code #\m) 8))
             #.(char-code #\p)))
  (:ft-glyph-format-bitmap
   #.(logior (logior (logior (ash #.(char-code #\b) 24) (ash #.(char-code #\i) 16)) (ash #.(char-code #\t) 8))
             #.(char-code #\s)))
  (:ft-glyph-format-outline
   #.(logior (logior (logior (ash #.(char-code #\o) 24) (ash #.(char-code #\u) 16)) (ash #.(char-code #\t) 8))
             #.(char-code #\l)))
  (:ft-glyph-format-plotter
   #.(logior (logior (logior (ash #.(char-code #\p) 24) (ash #.(char-code #\l) 16)) (ash #.(char-code #\o) 8))
             #.(char-code #\t))))

(defcstruct freetype:span
  (freetype:x :int16) (freetype:len :uint16) (freetype:coverage :uint8))

(defctype freetype:raster-span-func :pointer)

(defctype freetype:raster-bit-test-func :pointer)

(defctype freetype:raster-bit-set-func :pointer)

(defcenum freetype:raster-flag
  (:ft-raster-flag-default #.#o0) (:ft-raster-flag-aa #.1) (:ft-raster-flag-direct #.2) (:ft-raster-flag-clip #.4))

(defcstruct freetype:raster-params
  (freetype:target (* freetype:bitmap)) (freetype:source :pointer) (freetype:flags :int32)
  (freetype:gray-spans freetype:raster-span-func) (freetype:black-spans freetype:raster-span-func)
  (freetype:bit-test freetype:raster-bit-test-func) (freetype:bit-set freetype:raster-bit-set-func) (freetype:user :pointer)
  (freetype:clip-box freetype:bbox))

(defctype freetype:raster-new-func :pointer)

(defctype freetype:raster-done-func :pointer)

(defctype freetype:raster-reset-func :pointer)

(defctype freetype:raster-set-mode-func :pointer)

(defctype freetype:raster-render-func :pointer)

(defcstruct freetype:raster-funcs
 (freetype:glyph-format freetype:glyph-format) (freetype:raster-new freetype:raster-new-func)
  (freetype:raster-reset freetype:raster-reset-func) (freetype:raster-set-mode freetype:raster-set-mode-func)
  (freetype:raster-render freetype:raster-render-func) (freetype:raster-done freetype:raster-done-func))

(defcstruct freetype:unit-vector
 (freetype:x freetype:f2dot14) (freetype:y freetype:f2dot14))

(defcstruct freetype:matrix
 (freetype:xx freetype:fixed) (freetype:xy freetype:fixed) (freetype:yx freetype:fixed)
  (freetype:yy freetype:fixed))

(defctype freetype:generic-finalizer :pointer)

(defcstruct freetype:generic
 (freetype:data :pointer) (freetype:finalizer freetype:generic-finalizer))

(defcstruct freetype:list-node-rec
 (freetype:prev freetype:list-node) (freetype:next freetype:list-node) (freetype:data :pointer))

(defcstruct freetype:list-rec
 (freetype:head freetype:list-node) (freetype:tail freetype:list-node))

(defcstruct freetype:glyph-metrics
 (freetype:width freetype:pos) (freetype:height freetype:pos)
  (freetype:hori-bearing-x freetype:pos) (freetype:hori-bearing-y freetype:pos) (freetype:hori-advance freetype:pos)
  (freetype:vert-bearing-x freetype:pos) (freetype:vert-bearing-y freetype:pos) (freetype:vert-advance freetype:pos))

(defcstruct freetype:bitmap-size
 (freetype:height freetype:short) (freetype:width freetype:short))

(defctype freetype:sub-glyph :pointer)
;;    (struct freetype::sub-glyph-))

(defcstruct freetype:glyph-slot-rec
 (freetype:library freetype:library) (freetype:face (* (struct freetype::face-rec-)))
  (freetype:next (* (struct freetype::glyph-slot-rec-))) (freetype:flags freetype:uint) (freetype:generic freetype:generic)
  (freetype:metrics freetype:glyph-metrics) (freetype:linear-hori-advance freetype:fixed)
  (freetype:linear-vert-advance freetype:fixed) (freetype:advance freetype:vector) (freetype:format freetype:glyph-format)
  (freetype:bitmap freetype:bitmap) (freetype:bitmap-left freetype:int) (freetype:bitmap-top freetype:int)
  (freetype:outline freetype:outline) (freetype:num-subglyphs freetype:uint) (freetype:subglyphs (* freetype:sub-glyph))
  (freetype:control-data :pointer) (freetype:control-len :long) (freetype:other :pointer)
  (freetype:internal freetype:slot-internal))

(defcstruct freetype:size-metrics
 (freetype:x-ppem freetype:ushort) (freetype:y-ppem freetype:ushort)
  (freetype:x-scale freetype:fixed) (freetype:y-scale freetype:fixed) (freetype:ascender freetype:pos)
  (freetype:descender freetype:pos) (freetype:height freetype:pos) (freetype:max-advance freetype:pos))

(defcstruct freetype:size-rec
 (freetype:face (* (struct freetype::face-rec-))) (freetype:generic freetype:generic)
  (freetype:metrics freetype:size-metrics) (freetype:internal freetype:size-internal))

(defcstruct freetype:face-rec
 (freetype:num-faces freetype:long) (freetype:face-index freetype:long)
            (freetype:face-flags freetype:long) (freetype:style-flags freetype:long) (freetype:num-glyphs freetype:long)
            (freetype:family-name (* freetype:string)) (freetype:style-name (* freetype:string)) (freetype:num-fixed-sizes freetype:int)
            (freetype:available-sizes (* freetype:bitmap-size)) (freetype:num-charmaps freetype:int)
            (freetype:charmaps (* freetype:char-map)) (freetype:generic freetype:generic) (freetype:bbox freetype:bbox)
            (freetype:units-per-em freetype:ushort) (freetype:ascender freetype:short) (freetype:descender freetype:short)
            (freetype:height freetype:short) (freetype:max-advance-width freetype:short) (freetype:max-advance-height freetype:short)
            (freetype:underline-position freetype:short) (freetype:underline-thickness freetype:short)
            (freetype:glyph (* (struct freetype::glyph-slot-rec-)))
	    (freetype:size_s (* (struct freetype:size-rec))) (freetype:charmap freetype:char-map)
            (freetype:driver freetype:driver) (freetype:memory freetype:memory) (freetype:stream freetype:stream)
            (freetype:sizes-list freetype:list-rec) (freetype:autohint freetype:generic) (freetype:extensions :pointer)
            (freetype:internal freetype:face-internal))

(defcstruct freetype:size-rec
    (freetype:face (* freetype:face-rec))
  (freetype:generic freetype:generic)
  (freetype:metrics freetype:size-metrics)
  (freetype:internal freetype:size-internal))

(defcstruct freetype:glyph-slot-rec
 (freetype:library freetype:library) (freetype:face (* freetype:face-rec))
  (freetype:next (* (struct freetype::glyph-slot-rec-))) (freetype:flags freetype:uint) (freetype:generic freetype:generic)
  (freetype:metrics freetype:glyph-metrics) (freetype:linear-hori-advance freetype:fixed)
  (freetype:linear-vert-advance freetype:fixed) (freetype:advance freetype:vector) (freetype:format freetype:glyph-format)
  (freetype:bitmap freetype:bitmap) (freetype:bitmap-left freetype:int) (freetype:bitmap-top freetype:int)
  (freetype:outline freetype:outline) (freetype:num-subglyphs freetype:uint) (freetype:subglyphs (* freetype:sub-glyph))
  (freetype:control-data :pointer) (freetype:control-len :long) (freetype:other :pointer)
  (freetype:internal freetype:slot-internal))

(define-alien-type freetype:glyph-slot (* freetype:glyph-slot-rec))
(define-alien-type freetype:size (* freetype:size-rec))

(define-alien-routine ("FT_Init_FreeType" freetype:init-free-type) freetype:error (freetype::alibrary (* freetype:library)))

(define-alien-routine ("FT_Done_FreeType" freetype:done-free-type) freetype:error (freetype:library freetype:library))

(defcenum freetype:open-flags
  (:ft-open-memory #.1) (:ft-open-stream #.2) (:ft-open-pathname #.4) (:ft-open-driver #.8) (:ft-open-params #.16))

(defcstruct freetype:parameter (freetype:tag freetype:ulong) (freetype:data freetype:pointer))

(defcstruct freetype:open-args
 (freetype:flags freetype:open-flags) (freetype:memory-base (* freetype:byte))
  (freetype:memory-size freetype:long) (freetype:pathname (* freetype:string)) (freetype:stream freetype:stream)
  (freetype:driver freetype:module) (freetype:num-params freetype:int) (freetype:params (* freetype:parameter)))

(define-alien-routine ("FT_New_Face" freetype:new-face) freetype:error
  (freetype:library freetype:library)
  (freetype::filepathname :string)
  (freetype::face_index freetype:long)
  ;; this is a pointer to a pointer to a face-rec...
  (freetype::aface (* (* freetype:face-rec))))

(define-alien-routine ("FT_New_Memory_Face" freetype:new-memory-face) freetype:error (freetype:library freetype:library)
 (freetype::file_base (* freetype:byte)) (freetype::file_size freetype:long) (freetype::face_index freetype:long)
 (freetype::aface (* freetype:face)))

(define-alien-routine ("FT_Open_Face" freetype:open-face) freetype:error (freetype:library freetype:library)
 (freetype::args (* freetype:open-args)) (freetype::face_index freetype:long) (freetype::aface (* freetype:face)))

(define-alien-routine ("FT_Attach_File" freetype:attach-file) freetype:error (freetype:face freetype:face)
 (freetype::filepathname (* :int8)))

(define-alien-routine ("FT_Attach_Stream" freetype:attach-stream) freetype:error (freetype:face freetype:face)
 (freetype::parameters (* freetype:open-args)))

(define-alien-routine ("FT_Done_Face" freetype:done-face) freetype:error (freetype:face freetype:face))

(define-alien-routine ("FT_Set_Char_Size" freetype:set-char-size) freetype:error (freetype:face freetype:face)
 (freetype::char_width freetype:f26dot6) (freetype::char_height freetype:f26dot6) (freetype::horz_resolution freetype:uint)
 (freetype::vert_resolution freetype:uint))

(define-alien-routine ("FT_Set_Pixel_Sizes" freetype:set-pixel-sizes) freetype:error (freetype:face freetype:face)
 (freetype::pixel_width freetype:uint) (freetype::pixel_height freetype:uint))

(define-alien-routine ("FT_Load_Glyph" freetype:load-glyph) freetype:error (freetype:face freetype:face)
 (freetype::glyph_index freetype:uint) (freetype::load_flags freetype:int))

(define-alien-routine ("FT_Load_Char" freetype:load-char) freetype:error (freetype:face freetype:face)
 (freetype::char_code freetype:ulong) (freetype::load_flags freetype:int))

(define-alien-routine ("FT_Set_Transform" freetype:set-transform) :void (freetype:face freetype:face)
 (freetype:matrix (* freetype:matrix)) (freetype:delta (* freetype:vector)))

(defcenum freetype:render-mode
    (:ft-render-mode-normal #.#o0) (:ft-render-mode-mono #.1))

(define-alien-routine ("FT_Render_Glyph" freetype:render-glyph) freetype:error (freetype::slot freetype:glyph-slot)
 (freetype::render_mode freetype:uint))

(defcenum freetype::kerning-mode-
    (:ft-kerning-default #.#o0) :ft-kerning-unfitted :ft-kerning-unscaled)

(define-alien-routine ("FT_Get_Kerning" freetype:get-kerning) freetype:error (freetype:face freetype:face)
 (freetype::left_glyph freetype:uint) (freetype::right_glyph freetype:uint) (freetype::kern_mode freetype:uint)
 (freetype::akerning (* freetype:vector)))

(define-alien-routine ("FT_Get_Glyph_Name" freetype:get-glyph-name) freetype:error (freetype:face freetype:face)
 (freetype::glyph_index freetype:uint) (freetype:buffer freetype:pointer) (freetype::buffer_max freetype:uint))

(define-alien-routine ("FT_Get_Char_Index" freetype:get-char-index) freetype:uint (freetype:face freetype:face)
 (freetype::charcode freetype:ulong))

(define-alien-routine ("FT_MulDiv" freetype:mul-div) freetype:long (freetype::a freetype:long) (freetype::b freetype:long)
 (freetype::c freetype:long))

(define-alien-routine ("FT_MulFix" freetype:mul-fix) freetype:long (freetype::a freetype:long) (freetype::b freetype:long))

(define-alien-routine ("FT_DivFix" freetype:div-fix) freetype:long (freetype::a freetype:long) (freetype::b freetype:long))

(define-alien-routine ("FT_RoundFix" freetype:round-fix) freetype:fixed (freetype::a freetype:fixed))

(define-alien-routine ("FT_CeilFix" freetype:ceil-fix) freetype:fixed (freetype::a freetype:fixed))

(define-alien-routine ("FT_FloorFix" freetype:floor-fix) freetype:fixed (freetype::a freetype:fixed))

(define-alien-routine ("FT_Vector_Transform" freetype:vector-transform) :void (freetype::vec (* freetype:vector))
 (freetype:matrix (* freetype:matrix)))

(defcenum freetype:encoding
 (:ft-encoding-none #.(logior (logior (logior (ash #o0 24) (ash #o0 16)) (ash #o0 8)) #o0))
  (:ft-encoding-symbol
   #.(logior (logior (logior (ash #.(char-code #\s) 24) (ash #.(char-code #\y) 16)) (ash #.(char-code #\m) 8))
             #.(char-code #\b)))
  (:ft-encoding-unicode
   #.(logior (logior (logior (ash #.(char-code #\u) 24) (ash #.(char-code #\n) 16)) (ash #.(char-code #\i) 8))
             #.(char-code #\c)))
  (:ft-encoding-latin-2
   #.(logior (logior (logior (ash #.(char-code #\l) 24) (ash #.(char-code #\a) 16)) (ash #.(char-code #\t) 8))
             #.(char-code #\2)))
  (:ft-encoding-sjis
   #.(logior (logior (logior (ash #.(char-code #\s) 24) (ash #.(char-code #\j) 16)) (ash #.(char-code #\i) 8))
             #.(char-code #\s)))
  (:ft-encoding-gb2312
   #.(logior (logior (logior (ash #.(char-code #\g) 24) (ash #.(char-code #\b) 16)) (ash #.(char-code #\ ) 8))
             #.(char-code #\ )))
  (:ft-encoding-big5
   #.(logior (logior (logior (ash #.(char-code #\b) 24) (ash #.(char-code #\i) 16)) (ash #.(char-code #\g) 8))
             #.(char-code #\5)))
  (:ft-encoding-wansung
   #.(logior (logior (logior (ash #.(char-code #\w) 24) (ash #.(char-code #\a) 16)) (ash #.(char-code #\n) 8))
             #.(char-code #\s)))
  (:ft-encoding-johab
   #.(logior (logior (logior (ash #.(char-code #\j) 24) (ash #.(char-code #\o) 16)) (ash #.(char-code #\h) 8))
             #.(char-code #\a)))
  (:ft-encoding-adobe-standard
   #.(logior (logior (logior (ash #.(char-code #\A) 24) (ash #.(char-code #\D) 16)) (ash #.(char-code #\O) 8))
             #.(char-code #\B)))
  (:ft-encoding-adobe-expert
   #.(logior (logior (logior (ash #.(char-code #\A) 24) (ash #.(char-code #\D) 16)) (ash #.(char-code #\B) 8))
             #.(char-code #\E)))
  (:ft-encoding-adobe-custom
   #.(logior (logior (logior (ash #.(char-code #\A) 24) (ash #.(char-code #\D) 16)) (ash #.(char-code #\B) 8))
             #.(char-code #\C)))
  (:ft-encoding-apple-roman
   #.(logior (logior (logior (ash #.(char-code #\a) 24) (ash #.(char-code #\r) 16)) (ash #.(char-code #\m) 8))
             #.(char-code #\n))))

#|
(define-alien-type freetype:char-map-rec
 (struct freetype::char-map-rec- (freetype:face freetype:face) (freetype:encoding freetype:encoding)
  (freetype:platform-id freetype:ushort) (freetype:encoding-id freetype:ushort)))
|#

(define-alien-routine ("FT_Select_Charmap" freetype:select-charmap) freetype:error (freetype:face freetype:face)
 (freetype:encoding freetype:encoding))

(define-alien-routine ("FT_Set_Charmap" freetype:set-charmap) freetype:error (freetype:face freetype:face) (freetype:charmap freetype:char-map))
