(in-package :mcclim-harfbuzz)

(pkg-config-cflags "harfbuzz")
(pkg-config-cflags "freetype2")

(include "hb.h")
(include "hb-ft.h")

(ctype uint32-t "uint32_t")
(ctype hb-codepoint-t "hb_codepoint_t")
(ctype hb-mask-t "hb_mask_t")
(ctype hb-position-t "hb_position_t")
(ctype hb-tag-t "hb_tag_t")

(cenum (hb-direction-t)
       ((:hb-direction-invalid "HB_DIRECTION_INVALID"))
       ((:hb-direction-ltr "HB_DIRECTION_LTR"))
       ((:hb-direction-rtl "HB_DIRECTION_RTL"))
       ((:hb-direction-ttb "HB_DIRECTION_TTB"))
       ((:hb-direction-btt "HB_DIRECTION_BTT")))

(cstruct hb-glyph-info-t "hb_glyph_info_t"
         (codepoint "codepoint" :type hb-codepoint-t)
         (mask "mask" :type hb-mask-t)
         (cluster "cluster" :type uint32-t))

(cstruct hb-glyph-position-t "hb_glyph_position_t"
         (x-advance "x_advance" :type hb-position-t)
         (y-advance "y_advance" :type hb-position-t)
         (x-offset "x_offset" :type hb-position-t)
         (y-offset "y_offset" :type hb-position-t))

(cstruct hb-feature-t "hb_feature_t"
         (tag "tag" :type hb-tag-t)
         (value "value" :type uint32-t)
         (start "start" :type :unsigned-int)
         (end "end" :type :unsigned-int))
