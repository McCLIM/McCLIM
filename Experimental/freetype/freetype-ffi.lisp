;;; automatically generated, hand tweaked, do not regenerate.

(DEFPACKAGE :FREETYPE 
  (:USE :cl 
        #+sbcl :sb-alien 
        #+(or cmu scl) :alien #+(or cmu scl) :c-call)
            (:EXPORT "MEMORY-BASE" "DESCENDER" "LINEAR-VERT-ADVANCE" "YX" "XX" "FREE" "AVAILABLE-SIZES" "COVERAGE" "METRICS"
             "RASTER-FLAG" "GLYPH" "GET-CHAR-INDEX" "LIMIT" "STRING" "SHIFT" "LEN" "UNDERLINE-POSITION" "RASTER-NEW-FUNC"
             "POINTS" "TAG" "SIZE-INTERNAL" "NUM-SUBGLYPHS" "UNITS-PER-EM" "LIBRARY" "ALLOC" "OPEN-FACE" "ATTACH-FILE"
             "BITMAP-TOP" "CURSOR" "BITMAP-LEFT" "MODULE" "PIXEL-MODE" "FREE-FUNC" "PITCH" "EXTENSIONS" "RASTER-RENDER-FUNC"
             "GET-KERNING" "UFWORD" "OPEN-ARGS" "RASTER-FUNCS" "INT32" "PREV" "LOAD-CHAR" "PATHNAME" "HORI-BEARING-Y"
             "RASTER-RENDER" "ENCODING" "OUTLINE-CONIC-TO-FUNC" "STREAM" "RASTER-RESET" "MOVE-TO" "GENERIC" "ATTACH-STREAM"
             "Y-MAX" "X-MAX" "FACE-INDEX" "SUBGLYPHS" "BITMAP" "BITMAP-SIZE" "ADVANCE" "MUL-FIX" "SET-PIXEL-SIZES"
             "OUTLINE-CUBIC-TO-FUNC" "FACE-INTERNAL" "WCHAR-T" "BLACK-SPANS" "TAGS" "N-CONTOURS" "YY" "XY" "CONIC-TO" "INT"
             "UNDERLINE-THICKNESS" "NUM-FACES" "Y-PPEM" "X-PPEM" "PLATFORM-ID" "ASCENDER" "DIV-FIX" "USHORT" "WINT-T"
             "CONTROL-LEN" "WIDTH" "NEW-FACE" "CHAR-MAP-REC" "Y-SCALE" "X-SCALE" "ALLOC-FUNC" "OUTLINE-FUNCS" "RASTER-DONE"
             "UINT16" "FINALIZER" "RENDER-GLYPH" "GLYPH-METRICS" "RASTER-SPAN-FUNC" "CONTOURS" "GLYPH-SLOT-REC"
             "VERT-BEARING-Y" "INIT-FREE-TYPE" "CLIP-BOX" "RASTER-RESET-FUNC" "FLAGS" "USER" "MEMORY-SIZE" "HEIGHT" "N-POINTS"
             "UINT" "VECTOR" "NEXT" "LIST" "MEMORY" "BUFFER" "MUL-DIV" "PARAMS" "GET-GLYPH-NAME" "PTRDIFF-T" "CHAR-MAP" "FWORD"
             "OUTLINE-MOVE-TO-FUNC" "STREAM-CLOSE" "SET-TRANSFORM" "FLOOR-FIX" "GLYPH-FORMAT" "GLYPH-SLOT" "KERNING-MODE"
             "INT16" "POS" "FAST" "CONTROL-DATA" "NUM-PARAMS" "STYLE-FLAGS" "CHARMAP" "OUTLINE-LINE-TO-FUNC" "SELECT-CHARMAP"
             "TARGET" "TAIL" "SLOT-INTERNAL" "OUTLINE-FLAGS" "RASTER-SET-MODE" "LIST-NODE" "NEW-MEMORY-FACE" "SIZE-REC"
             "RASTER-DONE-FUNC" "HORI-ADVANCE" "RASTER" "REALLOC-FUNC" "READ" "ROUND-FIX" "LIST-REC" "UINT32" "ULONG" "HEAD"
             "DRIVER" "MAX-ADVANCE-HEIGHT" "SIZE-T" "Y-MIN" "X-MIN" "RASTER-SET-MODE-FUNC" "SIZE-METRICS" "ROWS" "OUTLINE" "Y"
             "X" "ENCODING-ID" "FORMAT" "REALLOC" "INTERNAL" "SIZES-LIST" "MAX-ADVANCE-WIDTH" "DONE-FACE" "SIZE" "SUB-GLYPH"
             "BBOX" "POINTER" "DELTA" "CEIL-FIX" "PARAMETER" "OFFSET" "MATRIX" "FACE-REC" "GRAY-SPANS" "PALETTE-MODE"
             "NUM-FIXED-SIZES" "BYTE" "SPAN" "VECTOR-TRANSFORM" "NUM-GRAYS" "RASTER-BIT-SET-FUNC" "RENDERER" "VALUE" "BIT-TEST"
             "SOURCE" "STYLE-NAME" "LINE-TO" "STREAM-DESC" "LONG" "UNIT-VECTOR" "LIST-NODE-REC" "HORI-BEARING-X" "VERT-ADVANCE"
             "CUBIC-TO" "GENERIC-FINALIZER" "CHAR" "PTR-DIST" "UFAST" "DESCRIPTOR" "SET-CHAR-SIZE" "LINEAR-HORI-ADVANCE"
             "MAX-ADVANCE" "AUTOHINT" "FIXED" "OPEN-FLAGS" "BASE" "ERROR" "NUM-GLYPHS" "DATA" "RENDER-MODE" "CHARMAPS" "FACE"
             "F26DOT6" "OTHER" "RASTER-PARAMS" "SET-CHARMAP" "NUM-CHARMAPS" "LOAD-GLYPH" "RASTER-BIT-TEST-FUNC" "RASTER-NEW"
             "FAMILY-NAME" "DONE-FREE-TYPE" "BIT-SET" "VERT-BEARING-X" "F2DOT14" "CLOSE" "STREAM-IO" "FACE-FLAGS" "BOOL"
             "SHORT" "PALETTE" "C" "PALETTE-MODE-" "SIZE-REC-" "B" "A" "RENDER_MODE" "RIGHT_GLYPH" "SIZE-INTERNAL-REC-"
             "CHAR-MAP-REC-" "RASTER-FUNCS-" "SUB-GLYPH-" "SLOT-INTERNAL-REC-" "OUTLINE-" "RENDER-MODE-" "GLYPH-METRICS-"
             "PARAMETER-" "LOAD_FLAGS" "MATRIX-" "ARGS" "GLYPH_INDEX" "OUTLINE-FLAGS-" "MEMORY-REC-" "PIXEL_WIDTH" "VEC"
             "HORZ_RESOLUTION" "LIST-REC-" "STREAM-REC-" "BUFFER_MAX" "LIBRARY-REC-" "SPAN-" "GLYPH-SLOT-REC-"
             "VERT_RESOLUTION" "LEFT_GLYPH" "PIXEL-MODE-" "AFACE" "MODULE-REC-" "PIXEL_HEIGHT" "CHARCODE" "AKERNING" "ALIBRARY"
             "LIST-NODE-REC-" "KERNING-MODE-" "FACE-REC-" "BITMAP-SIZE-" "DRIVER-REC-" "FILE_SIZE" "STREAM-DESC-"
             "FACE-INTERNAL-REC-" "FILEPATHNAME" "UNIT-VECTOR-" "PARAMETERS" "RASTER-PARAMS-" "OUTLINE-FUNCS-" "CHAR_HEIGHT"
             "BITMAP-" "FILE_BASE" "KERN_MODE" "CHAR_CODE" "RENDERER-REC-" "RASTER-REC-" "VECTOR-" "SIZE-METRICS-" "CHAR_WIDTH"
             "GENERIC-" "ENCODING-" "FACE_INDEX" "SLOT" "GLYPH-FORMAT-" "OPEN-ARGS-" "BBOX-" "SIZE_S"))

(in-package :freetype)

#+cmu
(alien:load-foreign "/usr/lib/libfreetype.so.6")

#+scl
(alien:load-dynamic-object #+64bit "/usr/lib64/libfreetype.so.6"
			   #-64bit "/usr/lib/libfreetype.so.6")

#+(or scl cmu)
(defmacro define-alien-type (&rest rest)
  `(def-alien-type ,@rest))
#+(or scl cmu)
(defmacro define-alien-routine (&rest rest)
  `(def-alien-routine ,@rest))

#+sbcl
(load-shared-object #+darwin "/usr/X11R6/lib/libfreetype.6.dylib" #-darwin "libfreetype.so.6")

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

(define-alien-type freetype:int16 (signed 16))

(define-alien-type freetype:uint16 (unsigned 16))

(define-alien-type freetype:int32 (signed 32))

(define-alien-type freetype:uint32 (unsigned 32))

(define-alien-type freetype:fast (signed 32))

(define-alien-type freetype:ufast (unsigned 32))

(define-alien-type freetype:ptrdiff-t (signed 32))

(define-alien-type freetype:size-t (unsigned 32))

(define-alien-type freetype:wchar-t (signed 32))

(define-alien-type freetype:wint-t (unsigned 32))

(define-alien-type freetype:bool (unsigned 8))

(define-alien-type freetype:fword (signed 16))

(define-alien-type freetype:ufword (unsigned 16))

(define-alien-type freetype:char (signed 8))

(define-alien-type freetype:byte (unsigned 8))

(define-alien-type freetype:string (signed 8))

(define-alien-type freetype:short (signed 16))

(define-alien-type freetype:ushort (unsigned 16))

(define-alien-type freetype:int (signed 32))

(define-alien-type freetype:uint (unsigned 32))

(define-alien-type freetype:long long)

(define-alien-type freetype:ulong unsigned-long)

(define-alien-type freetype:f2dot14 (signed 16))

(define-alien-type freetype:f26dot6 long)

(define-alien-type freetype:fixed long)

(define-alien-type freetype:error (signed 32))

(define-alien-type freetype:pointer (* t))

(define-alien-type freetype:offset freetype:size-t)

(define-alien-type freetype:ptr-dist freetype:size-t)

(define-alien-type nil
 (enum nil (:mod-err-base #.#x000) (:mod-err-autohint #.#x100) (:mod-err-cache #.#x200) (:mod-err-cff #.#x300)
  (:mod-err-cid #.#x400) (:mod-err-pcf #.#x500) (:mod-err-psaux #.#x600) (:mod-err-psnames #.#x700) (:mod-err-raster #.#x800)
  (:mod-err-sfnt #.#x900) (:mod-err-smooth #.#xA00) (:mod-err-true-type #.#xB00) (:mod-err-type1 #.#xC00)
  (:mod-err-winfonts #.#xD00) :mod-err-max))

(define-alien-type nil
 (enum nil (:err-ok #.#x00) (:err-cannot-open-resource #.(+ #x01 0)) (:err-unknown-file-format #.(+ #x02 0))
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
  (:err-invalid-post-table #.(+ #x9B 0)) (:err-syntax-error #.(+ #xA0 0)) (:err-stack-underflow #.(+ #xA1 0)) :err-max))

(define-alien-type freetype:alloc-func (* t))

(define-alien-type freetype:free-func (* t))

(define-alien-type freetype:realloc-func (* t))

(define-alien-type nil
 (struct freetype::memory-rec- (freetype:user (* t)) (freetype:alloc freetype:alloc-func) (freetype:free freetype:free-func)
  (freetype:realloc freetype:realloc-func)))

(define-alien-type freetype:stream-desc 
    (union freetype::stream-desc- 
      (freetype:value long) 
      (freetype:pointer (* t))))

(define-alien-type freetype:stream-io (* t))

(define-alien-type freetype:stream-close (* t))

(define-alien-type nil
    (struct freetype::stream-rec-
            (freetype:base (* (unsigned 8)))
            (freetype:size freetype:ulong)
            (freetype:pos freetype:ulong)
            (freetype:descriptor freetype:stream-desc)
            (freetype:pathname freetype:stream-desc)
            (freetype:read freetype:stream-io)
            (freetype:close freetype:stream-close)
            (freetype:memory freetype:memory)
            (freetype:cursor (* (unsigned 8)))
            (freetype:limit (* (unsigned 8)))))

(define-alien-type freetype:pos long)

(define-alien-type freetype:vector (struct freetype::vector- (freetype:x freetype:pos) (freetype:y freetype:pos)))

(define-alien-type freetype:bbox
 (struct freetype::bbox- (freetype:x-min freetype:pos) (freetype:y-min freetype:pos) (freetype:x-max freetype:pos)
  (freetype:y-max freetype:pos)))

(define-alien-type freetype:pixel-mode
 (enum freetype::pixel-mode- (:ft-pixel-mode-none #.#o0) :ft-pixel-mode-mono :ft-pixel-mode-grays :ft-pixel-mode-pal2
  :ft-pixel-mode-pal4 :ft-pixel-mode-pal8 :ft-pixel-mode-rgb15 :ft-pixel-mode-rgb16 :ft-pixel-mode-rgb24 :ft-pixel-mode-rgb32
  :ft-pixel-mode-max))

(define-alien-type freetype:palette-mode
 (enum freetype::palette-mode- (:ft-palette-mode-rgb #.#o0) :ft-palette-mode-rgba :ft-palettte-mode-max))

(define-alien-type freetype:bitmap
 (struct freetype::bitmap- (freetype:rows (signed 32)) (freetype:width (signed 32)) (freetype:pitch (signed 32))
  (freetype:buffer (* (unsigned 8))) (freetype:num-grays (signed 16)) (freetype:pixel-mode (signed 8))
  (freetype:palette-mode (signed 8)) (freetype:palette (* t))))

(define-alien-type freetype:outline
 (struct freetype::outline- (freetype:n-contours (signed 16)) (freetype:n-points (signed 16))
  (freetype:points (* freetype:vector)) (freetype:tags (* (signed 8))) (freetype:contours (* (signed 16)))
  (freetype:flags (signed 32))))

(define-alien-type freetype:outline-flags
 (enum freetype::outline-flags- (:ft-outline-none #.#o0) (:ft-outline-owner #.1) (:ft-outline-even-odd-fill #.2)
  (:ft-outline-reverse-fill #.4) (:ft-outline-ignore-dropouts #.8) (:ft-outline-high-precision #.256)
  (:ft-outline-single-pass #.512)))

(define-alien-type freetype:outline-move-to-func (* t))

(define-alien-type freetype:outline-line-to-func (* t))

(define-alien-type freetype:outline-conic-to-func (* t))

(define-alien-type freetype:outline-cubic-to-func (* t))

(define-alien-type freetype:outline-funcs
 (struct freetype::outline-funcs- (freetype:move-to freetype:outline-move-to-func)
  (freetype:line-to freetype:outline-line-to-func) (freetype:conic-to freetype:outline-conic-to-func)
  (freetype:cubic-to freetype:outline-cubic-to-func) (freetype:shift (signed 32)) (freetype:delta freetype:pos)))

(define-alien-type freetype:glyph-format
 (enum freetype::glyph-format- (:ft-glyph-format-none #.(logior (logior (logior (ash #o0 24) (ash #o0 16)) (ash #o0 8)) #o0))
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
             #.(char-code #\t)))))

(define-alien-type freetype:span
 (struct freetype::span- (freetype:x (signed 16)) (freetype:len (unsigned 16)) (freetype:coverage (unsigned 8))))

(define-alien-type freetype:raster-span-func (* t))

(define-alien-type freetype:raster-bit-test-func (* t))

(define-alien-type freetype:raster-bit-set-func (* t))

(define-alien-type freetype:raster-flag
 (enum nil (:ft-raster-flag-default #.#o0) (:ft-raster-flag-aa #.1) (:ft-raster-flag-direct #.2) (:ft-raster-flag-clip #.4)))

(define-alien-type freetype:raster-params
 (struct freetype::raster-params- (freetype:target (* freetype:bitmap)) (freetype:source (* t)) (freetype:flags (signed 32))
  (freetype:gray-spans freetype:raster-span-func) (freetype:black-spans freetype:raster-span-func)
  (freetype:bit-test freetype:raster-bit-test-func) (freetype:bit-set freetype:raster-bit-set-func) (freetype:user (* t))
  (freetype:clip-box freetype:bbox)))

(define-alien-type freetype:raster-new-func (* t))

(define-alien-type freetype:raster-done-func (* t))

(define-alien-type freetype:raster-reset-func (* t))

(define-alien-type freetype:raster-set-mode-func (* t))

(define-alien-type freetype:raster-render-func (* t))

(define-alien-type freetype:raster-funcs
 (struct freetype::raster-funcs- (freetype:glyph-format freetype:glyph-format) (freetype:raster-new freetype:raster-new-func)
  (freetype:raster-reset freetype:raster-reset-func) (freetype:raster-set-mode freetype:raster-set-mode-func)
  (freetype:raster-render freetype:raster-render-func) (freetype:raster-done freetype:raster-done-func)))

(define-alien-type freetype:unit-vector
 (struct freetype::unit-vector- (freetype:x freetype:f2dot14) (freetype:y freetype:f2dot14)))

(define-alien-type freetype:matrix
 (struct freetype::matrix- (freetype:xx freetype:fixed) (freetype:xy freetype:fixed) (freetype:yx freetype:fixed)
  (freetype:yy freetype:fixed)))

(define-alien-type freetype:generic-finalizer (* t))

(define-alien-type freetype:generic
 (struct freetype::generic- (freetype:data (* t)) (freetype:finalizer freetype:generic-finalizer)))

(define-alien-type freetype:list-node-rec
 (struct freetype::list-node-rec- (freetype:prev freetype:list-node) (freetype:next freetype:list-node) (freetype:data (* t))))

(define-alien-type freetype:list-rec
 (struct freetype::list-rec- (freetype:head freetype:list-node) (freetype:tail freetype:list-node)))

(define-alien-type freetype:glyph-metrics
 (struct freetype::glyph-metrics- (freetype:width freetype:pos) (freetype:height freetype:pos)
  (freetype:hori-bearing-x freetype:pos) (freetype:hori-bearing-y freetype:pos) (freetype:hori-advance freetype:pos)
  (freetype:vert-bearing-x freetype:pos) (freetype:vert-bearing-y freetype:pos) (freetype:vert-advance freetype:pos)))

(define-alien-type freetype:bitmap-size
 (struct freetype::bitmap-size- (freetype:height freetype:short) (freetype:width freetype:short)))

(define-alien-type freetype:sub-glyph (struct freetype::sub-glyph-))

(define-alien-type freetype:glyph-slot-rec
 (struct freetype::glyph-slot-rec- (freetype:library freetype:library) (freetype:face (* (struct freetype::face-rec-)))
  (freetype:next (* (struct freetype::glyph-slot-rec-))) (freetype:flags freetype:uint) (freetype:generic freetype:generic)
  (freetype:metrics freetype:glyph-metrics) (freetype:linear-hori-advance freetype:fixed)
  (freetype:linear-vert-advance freetype:fixed) (freetype:advance freetype:vector) (freetype:format freetype:glyph-format)
  (freetype:bitmap freetype:bitmap) (freetype:bitmap-left freetype:int) (freetype:bitmap-top freetype:int)
  (freetype:outline freetype:outline) (freetype:num-subglyphs freetype:uint) (freetype:subglyphs (* freetype:sub-glyph))
  (freetype:control-data (* t)) (freetype:control-len long) (freetype:other (* t))
  (freetype:internal freetype:slot-internal)))

(define-alien-type freetype:size-metrics
 (struct freetype::size-metrics- (freetype:x-ppem freetype:ushort) (freetype:y-ppem freetype:ushort)
  (freetype:x-scale freetype:fixed) (freetype:y-scale freetype:fixed) (freetype:ascender freetype:pos)
  (freetype:descender freetype:pos) (freetype:height freetype:pos) (freetype:max-advance freetype:pos)))

(define-alien-type freetype:size-rec
 (struct freetype::size-rec- (freetype:face (* (struct freetype::face-rec-))) (freetype:generic freetype:generic)
  (freetype:metrics freetype:size-metrics) (freetype:internal freetype:size-internal)))

(define-alien-type freetype:face-rec
    (struct freetype::face-rec- (freetype:num-faces freetype:long) (freetype:face-index freetype:long)
            (freetype:face-flags freetype:long) (freetype:style-flags freetype:long) (freetype:num-glyphs freetype:long)
            (freetype:family-name (* freetype:string)) (freetype:style-name (* freetype:string)) (freetype:num-fixed-sizes freetype:int)
            (freetype:available-sizes (* freetype:bitmap-size)) (freetype:num-charmaps freetype:int)
            (freetype:charmaps (* freetype:char-map)) (freetype:generic freetype:generic) (freetype:bbox freetype:bbox)
            (freetype:units-per-em freetype:ushort) (freetype:ascender freetype:short) (freetype:descender freetype:short)
            (freetype:height freetype:short) (freetype:max-advance-width freetype:short) (freetype:max-advance-height freetype:short)
            (freetype:underline-position freetype:short) (freetype:underline-thickness freetype:short)
            (freetype:glyph (* (struct freetype::glyph-slot-rec-))) (freetype:size_s (* (struct freetype:size-rec-))) (freetype:charmap freetype:char-map)
            (freetype:driver freetype:driver) (freetype:memory freetype:memory) (freetype:stream freetype:stream)
            (freetype:sizes-list freetype:list-rec) (freetype:autohint freetype:generic) (freetype:extensions (* t))
            (freetype:internal freetype:face-internal)))

(define-alien-type freetype:size-rec
 (struct freetype::size-rec- (freetype:face (* freetype:face-rec)) (freetype:generic freetype:generic)
  (freetype:metrics freetype:size-metrics) (freetype:internal freetype:size-internal)))

(define-alien-type freetype:glyph-slot-rec
 (struct freetype::glyph-slot-rec- (freetype:library freetype:library) (freetype:face (* freetype:face-rec))
  (freetype:next (* (struct freetype::glyph-slot-rec-))) (freetype:flags freetype:uint) (freetype:generic freetype:generic)
  (freetype:metrics freetype:glyph-metrics) (freetype:linear-hori-advance freetype:fixed)
  (freetype:linear-vert-advance freetype:fixed) (freetype:advance freetype:vector) (freetype:format freetype:glyph-format)
  (freetype:bitmap freetype:bitmap) (freetype:bitmap-left freetype:int) (freetype:bitmap-top freetype:int)
  (freetype:outline freetype:outline) (freetype:num-subglyphs freetype:uint) (freetype:subglyphs (* freetype:sub-glyph))
  (freetype:control-data (* t)) (freetype:control-len long) (freetype:other (* t))
  (freetype:internal freetype:slot-internal)))

(define-alien-type freetype:glyph-slot (* freetype:glyph-slot-rec))
(define-alien-type freetype:face (* freetype:face-rec))
(define-alien-type freetype:size (* freetype:size-rec))

(define-alien-routine ("FT_Init_FreeType" freetype:init-free-type) freetype:error (freetype::alibrary (* freetype:library)))

(define-alien-routine ("FT_Done_FreeType" freetype:done-free-type) freetype:error (freetype:library freetype:library))

(define-alien-type freetype:open-flags
 (enum nil (:ft-open-memory #.1) (:ft-open-stream #.2) (:ft-open-pathname #.4) (:ft-open-driver #.8) (:ft-open-params #.16)))

(define-alien-type freetype:parameter (struct freetype::parameter- (freetype:tag freetype:ulong) (freetype:data freetype:pointer)))

(define-alien-type freetype:open-args
 (struct freetype::open-args- (freetype:flags freetype:open-flags) (freetype:memory-base (* freetype:byte))
  (freetype:memory-size freetype:long) (freetype:pathname (* freetype:string)) (freetype:stream freetype:stream)
  (freetype:driver freetype:module) (freetype:num-params freetype:int) (freetype:params (* freetype:parameter))))

(define-alien-routine ("FT_New_Face" freetype:new-face) freetype:error (freetype:library freetype:library)
 (freetype::filepathname #+(or cmu scl) c-call:c-string #+sbcl c-string) (freetype::face_index freetype:long) (freetype::aface (* (* freetype:face-rec))))

(define-alien-routine ("FT_New_Memory_Face" freetype:new-memory-face) freetype:error (freetype:library freetype:library)
 (freetype::file_base (* freetype:byte)) (freetype::file_size freetype:long) (freetype::face_index freetype:long)
 (freetype::aface (* freetype:face)))

(define-alien-routine ("FT_Open_Face" freetype:open-face) freetype:error (freetype:library freetype:library)
 (freetype::args (* freetype:open-args)) (freetype::face_index freetype:long) (freetype::aface (* freetype:face)))

(define-alien-routine ("FT_Attach_File" freetype:attach-file) freetype:error (freetype:face freetype:face)
 (freetype::filepathname (* (signed 8))))

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

(define-alien-routine ("FT_Set_Transform" freetype:set-transform) #+(or cmu scl) c-call:void #+sbcl void (freetype:face freetype:face)
 (freetype:matrix (* freetype:matrix)) (freetype:delta (* freetype:vector)))

(define-alien-type freetype:render-mode (enum freetype::render-mode- (:ft-render-mode-normal #.#o0) (:ft-render-mode-mono #.1)))

(define-alien-routine ("FT_Render_Glyph" freetype:render-glyph) freetype:error (freetype::slot freetype:glyph-slot)
 (freetype::render_mode freetype:uint))

(define-alien-type freetype:kerning-mode
 (enum freetype::kerning-mode- (:ft-kerning-default #.#o0) :ft-kerning-unfitted :ft-kerning-unscaled))

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

(define-alien-routine ("FT_Vector_Transform" freetype:vector-transform) #+(or cmu scl) c-call:void #+sbcl void (freetype::vec (* freetype:vector))
 (freetype:matrix (* freetype:matrix)))

(define-alien-type freetype:encoding
 (enum freetype::encoding- (:ft-encoding-none #.(logior (logior (logior (ash #o0 24) (ash #o0 16)) (ash #o0 8)) #o0))
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
             #.(char-code #\n)))))

#|
(define-alien-type freetype:char-map-rec
 (struct freetype::char-map-rec- (freetype:face freetype:face) (freetype:encoding freetype:encoding)
  (freetype:platform-id freetype:ushort) (freetype:encoding-id freetype:ushort)))
|#

(define-alien-routine ("FT_Select_Charmap" freetype:select-charmap) freetype:error (freetype:face freetype:face)
 (freetype:encoding freetype:encoding))

(define-alien-routine ("FT_Set_Charmap" freetype:set-charmap) freetype:error (freetype:face freetype:face) (freetype:charmap freetype:char-map))
