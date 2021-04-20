;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) Copyright 2004 by Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Postscript Font Encodings
;;;

(in-package #:clim-postscript-font)

(defvar *iso-latin-1-symbolic-names*
    '#(NIL             NIL             NIL             NIL
       NIL             NIL             NIL             NIL
       NIL             NIL             NIL             NIL
       NIL             NIL             NIL             NIL
       NIL             NIL             NIL             NIL
       NIL             NIL             NIL             NIL
       NIL             NIL             NIL             NIL
       NIL             NIL             NIL             NIL

       "space"         "exclam"        "quotedbl"      "numbersign"
       "dollar"        "percent"       "ampersand"     "quoteright"
       "parenleft"     "parenright"    "asterisk"      "plus"
       "comma"         "hyphen"        "period"        "slash"
       "zero"          "one"           "two"           "three"
       "four"          "five"          "six"           "seven"
       "eight"         "nine"          "colon"         "semicolon"
       "less"          "equal"         "greater"       "question"

       "at"            "A"             "B"             "C"
       "D"             "E"             "F"             "G"
       "H"             "I"             "J"             "K"
       "L"             "M"             "N"             "O"
       "P"             "Q"             "R"             "S"
       "T"             "U"             "V"             "W"
       "X"             "Y"             "Z"             "bracketleft"
       "backslash"     "bracketright"  "asciicircum"   "underscore"

       "quoteleft"     "a"             "b"             "c"
       "d"             "e"             "f"             "g"
       "h"             "i"             "j"             "k"
       "l"             "m"             "n"             "o"
       "p"             "q"             "r"             "s"
       "t"             "u"             "v"             "w"
       "x"             "y"             "z"             "braceleft"
       "bar"           "braceright"    "asciitilde"    NIL

       NIL             NIL             NIL             NIL
       NIL             NIL             NIL             NIL
       NIL             NIL             NIL             NIL
       NIL             NIL             NIL             NIL
       NIL             NIL             NIL             NIL
       NIL             NIL             NIL             NIL
       NIL             NIL             NIL             NIL
       NIL             NIL             NIL             NIL

       "nbspace"       "exclamdown"    "cent"          "sterling"
       "currency"      "yen"           "brokenbar"     "section"
       NIL             "copyright"     "ordfeminine"   "guillemotleft"
       "logicalnot"    NIL             "registered"    NIL
       "degree"        "plusminus"     "twosuperior"   "threesuperior"
       "acute"         "mu"            "paragraph"     "periodcentered"
       "cedilla"       "onesuperior"   "ordmasculine"  "guillemotright"
       "onequarter"   "onehalf"       "threequarters" "questiondown"

       "Agrave"        "Aacute"        "Acircumflex"   "Atilde"
       "Adieresis"     "Aring"         "AE"            "Ccedilla"
       "Egrave"        "Eacute"        "Ecircumflex"   "Edieresis"
       "Igrave"        "Iacute"        "Icircumflex"   "Idieresis"
       "Eth"           "Ntilde"        "Ograve"        "Oacute"
       "Ocircumflex"   "Otilde"        "Odieresis"     "multiply"
       "Oslash"        "Ugrave"        "Uacute"        "Ucircumflex"
       "Udieresis"     "Yacute"        "Thorn"         "germandbls"

       "agrave"        "aacute"        "acircumflex"   "atilde"
       "adieresis"     "aring"         "ae"            "ccedilla"
       "egrave"        "eacute"        "ecircumflex"   "edieresis"
       "igrave"        "iacute"        "icirc"         "idieresis"
       "eth"           "ntilde"        "ograve"        "oacute"
       "ocircumflex"   "otilde"        "odieresis"     "divide"
       "oslash"        "ugrave"        "uacute"        "ucircumflex"
       "udieresis"     "yacute"        "thorn"         "ydieresis")
  "A mapping of iso-8859-1 code points to adobe glyph names.")
