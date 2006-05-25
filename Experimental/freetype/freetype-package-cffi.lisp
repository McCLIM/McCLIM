;;;---------------------------------------------------------------------------
;;; Moved this here to make loading more convenient for the way I edit
;;; code [2006/05/23:rpg]
;;;---------------------------------------------------------------------------
(DEFPACKAGE :FREETYPE
    (:import-from :cffi #:define-foreign-library
		  #:use-foreign-library
		  #:defctype
		  #:defcenum
		  ;;#:defcstruct
		  #:defcunion
		  #:defcfun
		  )
  (:USE :cl 
;;;        #+sbcl :sb-alien 
;;;        #+(or cmu scl) :alien #+(or cmu scl) :c-call
	)
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


    
(defpackage :mcclim-freetype
    (:use :climi :clim :clim-lisp)
    (:export :*freetype-font-path*)
    (:import-from :cffi
		  #:with-foreign-slots
		  #:foreign-slot-value
		  #:foreign-alloc)
    (:import-from :freetype
		  #:glyph
		  #:bitmap
		  #:width
		  #:pitch
		  #:rows
		  #:buffer
		  #:x #:y
		  #:bitmap-left
		  #:bitmap-top
		  #:advance
		  #:ascender
		  #:descender
		  #:size_s
		  #:metrics)
		  
;;;    (:import-from #+cmucl :alien
;;;                  #+sbcl :sb-alien
;;;                  :slot :make-alien :alien :deref)
    )
