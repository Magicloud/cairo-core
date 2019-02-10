#include "cairo-core.h"
{-|
Description : λ https://www.cairographics.org/manual/cairo-text.html //div[@class="refnamediv"]/table/tr/td/p/text()

λ https://www.cairographics.org/manual/cairo-text.html#cairo-text.description
-}
module Graphics.Cairo.Drawing.Text where

{#import Graphics.Cairo.Types#}
import Foreign
import Graphics.Cairo.Fonts.FontOptions
import Graphics.Cairo.Utilities.ErrorHandling

{#context lib="cairo" prefix="cairo"#}

-- λ https://www.cairographics.org/manual/cairo-text.html#cairo-select-font-face
{#fun select_font_face as ^ { `Context', withUTF8String* `String', `FontSlant', `FontWeight' } -> `()'#}
-- λ https://www.cairographics.org/manual/cairo-text.html#cairo-set-font-size
{#fun set_font_size as ^ { `Context', `Double' } -> `()'#}
-- λ https://www.cairographics.org/manual/cairo-text.html#cairo-set-font-matrix
{#fun set_font_matrix as ^ { `Context', with* `Matrix Double' } -> `()'#}
-- λ https://www.cairographics.org/manual/cairo-text.html#cairo-get-font-matrix
{#fun get_font_matrix as ^ { `Context', alloca- `Matrix Double' peek* } -> `()'#}
-- λ https://www.cairographics.org/manual/cairo-text.html#cairo-set-font-options
{#fun set_font_options as ^ { `Context', `FontOptions' } -> `()'#}
-- λ https://www.cairographics.org/manual/cairo-text.html#cairo-set-font-face
{#fun set_font_face as ^ { `Context', `FontFace' } -> `()'#}
-- λ https://www.cairographics.org/manual/cairo-text.html#cairo-get-font-face
{#fun get_font_face as ^ { `Context' } -> `FontFace' outFontFaceRef*#}
-- λ https://www.cairographics.org/manual/cairo-text.html#cairo-show-text
{#fun show_text as ^ { `Context', withUTF8String* `String' } -> `()'#}
-- λ https://www.cairographics.org/manual/cairo-text.html#cairo-font-extents
{#fun font_extents as ^ { `Context', alloca- `FontExtents' peek* } -> `()'#}
-- λ https://www.cairographics.org/manual/cairo-text.html#cairo-text-extents
{#fun text_extents as ^ { `Context', withUTF8String* `String', alloca- `TextExtents' peek* } -> `()'#}
-- λ https://www.cairographics.org/manual/cairo-text.html#cairo-glyph-extents
{#fun glyph_extents as ^ { `Context', withArrayLen_* `[Glyph]'&, alloca- `TextExtents' peek* } -> `()'#}
-- λ https://www.cairographics.org/manual/cairo-text.html#cairo-show-glyphs
{#fun show_glyphs as ^ { `Context', withArrayLen_* `[Glyph]'& } -> `()'#}

-- λ https://www.cairographics.org/manual/cairo-text.html#cairo-get-font-options
getFontOptions :: Context -> IO FontOptions
getFontOptions c = do
  fo <- (fontOptionsCreate >>= getFontOptions' c)
  fontOptionsStatus fo >>= failStatus
  return fo
  where {#fun get_font_options as getFontOptions' { `Context', `FontOptions' outFontOptions* } -> `()'#}

-- λ https://www.cairographics.org/manual/cairo-text.html#cairo-set-scaled-font
{#fun set_scaled_font as ^ { `Context', `ScaledFont' } -> `()'#} -- λ require CAIRO_CHECK_VERSION(1,2,0)

-- λ https://www.cairographics.org/manual/cairo-text.html#cairo-get-scaled-font
{#fun get_scaled_font as ^ { `Context' } -> `ScaledFont' outScaledFontRef*#} -- λ require CAIRO_CHECK_VERSION(1,4,0)

-- λ https://www.cairographics.org/manual/cairo-text.html#cairo-show-text-glyphs
#if CAIRO_CHECK_VERSION(1,8,0)
showTextGlyphs :: Context
                  -> String -> [Glyph] -> [TextCluster] -> TextClusterFlags -> IO ()
showTextGlyphs context utf8 glyphs textClusters textClusterFlags =
  showTextGlyphs' context utf8 (-1) glyphs textClusters textClusterFlags
  where {#fun show_text_glyphs as showTextGlyphs' { `Context', withUTF8String* `String', `Int', withArrayLen_* `[Glyph]'&,withArrayLen_* `[TextCluster]'&, `TextClusterFlags' } -> `()'#}
#else
{-# WARNING showTextGlyphs "CAIRO_CHECK_VERSION(1,8,0) unmet" #-}
showTextGlyphs= undefined
#endif

-- λ https://www.cairographics.org/manual/cairo-text.html#cairo-text-cluster-free
{#fun text_cluster_free as ^ { `TextClusterPtr' } -> `()'#} -- λ require CAIRO_CHECK_VERSION(1,8,0)
-- λ https://www.cairographics.org/manual/cairo-text.html#cairo-glyph-free
{#fun glyph_free as ^ { `GlyphPtr' } -> `()'#} -- λ require CAIRO_CHECK_VERSION(1,8,0)
-- λ https://www.cairographics.org/manual/cairo-text.html#cairo-toy-font-face-create
{#fun toy_font_face_create as ^ { withUTF8String* `String', `FontSlant', `FontWeight' } -> `FontFace'#} -- λ require CAIRO_CHECK_VERSION(1,8,0)
-- λ https://www.cairographics.org/manual/cairo-text.html#cairo-toy-font-face-get-family
{#fun toy_font_face_get_family as ^ { `FontFace' } -> `String'#} -- λ require CAIRO_CHECK_VERSION(1,8,0)
-- λ https://www.cairographics.org/manual/cairo-text.html#cairo-toy-font-face-get-slant
{#fun toy_font_face_get_slant as ^ { `FontFace' } -> `FontSlant'#} -- λ require CAIRO_CHECK_VERSION(1,8,0)
-- λ https://www.cairographics.org/manual/cairo-text.html#cairo-toy-font-face-get-weight
{#fun toy_font_face_get_weight as ^ { `FontFace' } -> `FontWeight'#} -- λ require CAIRO_CHECK_VERSION(1,8,0)
