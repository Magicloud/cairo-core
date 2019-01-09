#include "cairo-core.h"

module Graphics.Cairo.Drawing.Text where

{#import Graphics.Cairo.Types#}
import Foreign
import Graphics.Cairo.Fonts.FontOptions
import Graphics.Cairo.Utilities.ErrorHandling

{#context lib="cairo" prefix="cairo"#}

{#fun select_font_face as ^ { `Context', withUTF8String* `String', `FontSlant', `FontWeight' } -> `()'#}
{#fun set_font_size as ^ { `Context', `Double' } -> `()'#}
{#fun set_font_matrix as ^ { `Context', with* `Matrix Double' } -> `()'#}
{#fun get_font_matrix as ^ { `Context', alloca- `Matrix Double' peek* } -> `()'#}
{#fun set_font_options as ^ { `Context', `FontOptions' } -> `()'#}
{#fun set_font_face as ^ { `Context', `FontFace' } -> `()'#}
{#fun get_font_face as ^ { `Context' } -> `FontFace' outFontFaceRef*#}
{#fun show_text as ^ { `Context', withUTF8String* `String' } -> `()'#}
{#fun font_extents as ^ { `Context', alloca- `FontExtents' peek* } -> `()'#}
{#fun text_extents as ^ { `Context', withUTF8String* `String', alloca- `TextExtents' peek* } -> `()'#}

getFontOptions :: Context -> IO FontOptions
getFontOptions c = do
  fo <- (fontOptionsCreate >>= getFontOptions' c)
  fontOptionsStatus fo >>= failStatus
  return fo
  where {#fun get_font_options as getFontOptions' { `Context', `FontOptions' outFontOptions* } -> `()'#}

glyphExtents :: Context -> [Glyph] -> IO TextExtents
glyphExtents context glyphs =
  withArrayLen glyphs $ \len ptr -> glyphExtents' context ptr len
  where {#fun glyph_extents as glyphExtents' { `Context', `GlyphPtr', `Int', alloca- `TextExtents' peek* } -> `()'#}

showGlyphs :: Context -> [Glyph] -> IO ()
showGlyphs context glyphs =
  withArrayLen glyphs $ \len glyphPtr -> showGlyphs' context glyphPtr len
  where {#fun show_glyphs as showGlyphs' { `Context', `GlyphPtr', `Int' } -> `()'#}

#if CAIRO_CHECK_VERSION(1,2,0)
{#fun set_scaled_font as ^ { `Context', `ScaledFont' } -> `()'#}
#endif -- CAIRO_CHECK_VERSION(1,2,0)

#if CAIRO_CHECK_VERSION(1,4,0)
{#fun get_scaled_font as ^ { `Context' } -> `ScaledFont' outScaledFontRef*#}
#endif -- CAIRO_CHECK_VERSION(1,4,0)

#if CAIRO_CHECK_VERSION(1,8,0)
showTextGlyphs :: Context
                  -> String -> [Glyph] -> [TextCluster] -> TextClusterFlags -> IO ()
showTextGlyphs context utf8 glyphs textClusters textClusterFlags =
  withArrayLen glyphs $ \glyphLen glyphPtr ->
    withArrayLen textClusters $ \tcLen tcPtr ->
        showTextGlyphs' context utf8 (-1) glyphPtr glyphLen tcPtr tcLen textClusterFlags
  where {#fun show_text_glyphs as showTextGlyphs' { `Context', withUTF8String* `String', `Int', `GlyphPtr', `Int', `TextClusterPtr', `Int', `TextClusterFlags' } -> `()'#}

{#fun text_cluster_free as ^ { `TextClusterPtr' } -> `()'#}
{#fun glyph_free as ^ { `GlyphPtr' } -> `()'#}
{#fun toy_font_face_create as ^ { withUTF8String* `String', `FontSlant', `FontWeight' } -> `FontFace'#}
{#fun toy_font_face_get_family as ^ { `FontFace' } -> `String'#}
{#fun toy_font_face_get_slant as ^ { `FontFace' } -> `FontSlant'#}
{#fun toy_font_face_get_weight as ^ { `FontFace' } -> `FontWeight'#}
#endif -- CAIRO_CHECK_VERSION(1,8,0)
