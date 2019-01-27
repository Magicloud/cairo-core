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

-- λ https://www.cairographics.org/manual/cairo-text.html#cairo-get-font-options
getFontOptions :: Context -> IO FontOptions
getFontOptions c = do
  fo <- (fontOptionsCreate >>= getFontOptions' c)
  fontOptionsStatus fo >>= failStatus
  return fo
  where {#fun get_font_options as getFontOptions' { `Context', `FontOptions' outFontOptions* } -> `()'#}

-- λ https://www.cairographics.org/manual/cairo-text.html#cairo-glyph-extents
glyphExtents :: Context -> [Glyph] -> IO TextExtents
glyphExtents context glyphs =
  withArrayLen glyphs $ \len ptr -> glyphExtents' context ptr len
  where {#fun glyph_extents as glyphExtents' { `Context', `GlyphPtr', `Int', alloca- `TextExtents' peek* } -> `()'#}

-- λ https://www.cairographics.org/manual/cairo-text.html#cairo-show-glyphs
showGlyphs :: Context -> [Glyph] -> IO ()
showGlyphs context glyphs =
  withArrayLen glyphs $ \len glyphPtr -> showGlyphs' context glyphPtr len
  where {#fun show_glyphs as showGlyphs' { `Context', `GlyphPtr', `Int' } -> `()'#}

#if CAIRO_CHECK_VERSION(1,2,0)
-- λ https://www.cairographics.org/manual/cairo-text.html#cairo-set-scaled-font
{#fun set_scaled_font as ^ { `Context', `ScaledFont' } -> `()'#}
#endif -- CAIRO_CHECK_VERSION(1,2,0)

#if CAIRO_CHECK_VERSION(1,4,0)
-- λ https://www.cairographics.org/manual/cairo-text.html#cairo-get-scaled-font
{#fun get_scaled_font as ^ { `Context' } -> `ScaledFont' outScaledFontRef*#}
#endif -- CAIRO_CHECK_VERSION(1,4,0)

#if CAIRO_CHECK_VERSION(1,8,0)
-- λ https://www.cairographics.org/manual/cairo-text.html#cairo-show-text-glyphs
showTextGlyphs :: Context
                  -> String -> [Glyph] -> [TextCluster] -> TextClusterFlags -> IO ()
showTextGlyphs context utf8 glyphs textClusters textClusterFlags =
  withArrayLen glyphs $ \glyphLen glyphPtr ->
    withArrayLen textClusters $ \tcLen tcPtr ->
        showTextGlyphs' context utf8 (-1) glyphPtr glyphLen tcPtr tcLen textClusterFlags
  where {#fun show_text_glyphs as showTextGlyphs' { `Context', withUTF8String* `String', `Int', `GlyphPtr', `Int', `TextClusterPtr', `Int', `TextClusterFlags' } -> `()'#}

-- λ https://www.cairographics.org/manual/cairo-text.html#cairo-text-cluster-free
{#fun text_cluster_free as ^ { `TextClusterPtr' } -> `()'#}
-- λ https://www.cairographics.org/manual/cairo-text.html#cairo-glyph-free
{#fun glyph_free as ^ { `GlyphPtr' } -> `()'#}
-- λ https://www.cairographics.org/manual/cairo-text.html#cairo-toy-font-face-create
{#fun toy_font_face_create as ^ { withUTF8String* `String', `FontSlant', `FontWeight' } -> `FontFace'#}
-- λ https://www.cairographics.org/manual/cairo-text.html#cairo-toy-font-face-get-family
{#fun toy_font_face_get_family as ^ { `FontFace' } -> `String'#}
-- λ https://www.cairographics.org/manual/cairo-text.html#cairo-toy-font-face-get-slant
{#fun toy_font_face_get_slant as ^ { `FontFace' } -> `FontSlant'#}
-- λ https://www.cairographics.org/manual/cairo-text.html#cairo-toy-font-face-get-weight
{#fun toy_font_face_get_weight as ^ { `FontFace' } -> `FontWeight'#}
#endif -- CAIRO_CHECK_VERSION(1,8,0)
