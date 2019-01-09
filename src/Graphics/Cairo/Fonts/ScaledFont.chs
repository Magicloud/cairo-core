#include "cairo-core.h"

module Graphics.Cairo.Fonts.ScaledFont where

{#import Graphics.Cairo.Types#}
import Control.Exception
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import Graphics.Cairo.Drawing.Text
import Graphics.Cairo.Fonts.FontFace
import Graphics.Cairo.Fonts.FontOptions
import Graphics.Cairo.HasStatus
import Graphics.Cairo.Utilities.ErrorHandling

{#context lib="cairo" prefix="cairo"#}

instance HasStatus ScaledFont where
  status = scaledFontStatus

scaledFontGlyphExtents :: ScaledFont -> [Glyph] -> IO TextExtents
scaledFontGlyphExtents scaledFont glyphs =
  withArrayLen glyphs $ \len glyphPtr -> scaledFontGlyphExtents' scaledFont glyphPtr len
  where {#fun scaled_font_glyph_extents as scaledFontGlyphExtents' { `ScaledFont', `GlyphPtr', `Int', alloca- `TextExtents' peek* } -> `()'#}

{#fun scaled_font_status as ^ { `ScaledFont' } -> `Status' #}
{#fun scaled_font_create as ^ { `FontFace', with* `Matrix Double', with* `Matrix Double', `FontOptions' } -> `ScaledFont' outScaledFont*#}
{#fun scaled_font_extents as ^ { `ScaledFont', alloca- `FontExtents' peek* } -> `()'#}

#if CAIRO_CHECK_VERSION(1,2,0)
scaledFontGetFontFace :: ScaledFont -> IO FontFace
scaledFontGetFontFace sf = do
  ff <- scaledFontGetFontFace' sf
  fontFaceStatus ff >>= failStatus
  return ff
  where {#fun scaled_font_get_font_face as scaledFontGetFontFace' { `ScaledFont' } -> `FontFace' outFontFaceRef*#}

scaledFontGetFontOptions :: ScaledFont -> IO FontOptions
scaledFontGetFontOptions sf = do
  fo <- (fontOptionsCreate >>= scaledFontGetFontOptions' sf)
  fontOptionsStatus fo >>= failStatus
  return fo
  where {#fun scaled_font_get_font_options as scaledFontGetFontOptions' { `ScaledFont', `FontOptions' outFontOptions* } -> `()'#}

{#fun scaled_font_text_extents as ^ { `ScaledFont', withUTF8String* `String', alloca- `TextExtents' peek* } -> `()'#}
{#fun scaled_font_get_font_matrix as ^ { `ScaledFont', alloca- `Matrix Double' peek* } -> `()'#}
{#fun scaled_font_get_ctm as ^ { `ScaledFont', alloca- `Matrix Double' peek* } -> `()'#}
{#fun scaled_font_get_type as ^ { `ScaledFont' } -> `FontType'#}
#endif

#if CAIRO_CHECK_VERSION(1,4,0)
{#fun scaled_font_get_reference_count as ^ { `ScaledFont' } -> `Int'#}
#endif

#if CAIRO_CHECK_VERSION(1,8,0)
scaledFontTextToGlyphs :: ScaledFont
                          -> X
                          -> Y
                          -> String
                          -> IO ([Glyph], [TextCluster], TextClusterFlags)
scaledFontTextToGlyphs scaledFont x y utf8 =
  with nullPtr $ \gPtrPtr ->
    with nullPtr $ \tcPtrPtr ->
      bracket (do
        (s, numG, numTC, tcf) <- scaledFontTextToGlyphs' scaledFont x y utf8 (-1) gPtrPtr tcPtrPtr
        gPtr <- peek gPtrPtr
        tcPtr <- peek tcPtrPtr
        return (s, numG, numTC, tcf, gPtr, tcPtr))
              (\(s, _, _, _, gPtr, tcPtr) -> do
        glyphFree gPtr
        textClusterFree tcPtr
        failStatus s) (\(_, numG, numTC, tcf, gPtr, tcPtr) -> do
        glyphs <- peekArray numG gPtr
        tcs <- peekArray numTC tcPtr
        return (glyphs, tcs, tcf))
  where
    {#fun scaled_font_text_to_glyphs as scaledFontTextToGlyphs' { `ScaledFont', CDouble `X', CDouble `Y', withUTF8String* `String', `Int', id `Ptr GlyphPtr', alloca- `Int' peekInt*, id `Ptr TextClusterPtr', alloca- `Int' peekInt*, alloca- `TextClusterFlags' peekEnum* } -> `Status'#}

{#fun scaled_font_get_scale_matrix as ^ { `ScaledFont', alloca- `Matrix Double' peek* } -> `()'#}
#endif
