#include "cairo-core.h"
{-|
Description : λ https://www.cairographics.org/manual/cairo-cairo-scaled-font-t.html //div[@class="refnamediv"]/table/tr/td/p/text()

λ https://www.cairographics.org/manual/cairo-cairo-scaled-font-t.html#cairo-cairo-scaled-font-t.description
-}
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

-- λ https://www.cairographics.org/manual/cairo-cairo-scaled-font-t.html#cairo-scaled-font-glyph-extents
{#fun scaled_font_glyph_extents as ^ { `ScaledFont', withArrayLen_* `[Glyph]'&, alloca- `TextExtents' peek* } -> `()'#}
-- λ https://www.cairographics.org/manual/cairo-cairo-scaled-font-t.html#cairo-scaled-font-create
{#fun scaled_font_create as ^ { `FontFace', with* `Matrix Double', with* `Matrix Double', `FontOptions' } -> `ScaledFont' outScaledFont*#}
-- λ https://www.cairographics.org/manual/cairo-cairo-scaled-font-t.html#cairo-scaled-font-status
{#fun scaled_font_status as ^ { `ScaledFont' } -> `Status' #}
-- λ https://www.cairographics.org/manual/cairo-cairo-scaled-font-t.html#cairo-scaled-font-extents
{#fun scaled_font_extents as ^ { `ScaledFont', alloca- `FontExtents' peek* } -> `()'#}

-- λ https://www.cairographics.org/manual/cairo-cairo-scaled-font-t.html#cairo-scaled-font-get-font-face
#if CAIRO_CHECK_VERSION(1,2,0)
scaledFontGetFontFace :: ScaledFont -> IO FontFace
scaledFontGetFontFace sf = do
  ff <- scaledFontGetFontFace' sf
  fontFaceStatus ff >>= failStatus
  return ff
  where {#fun scaled_font_get_font_face as scaledFontGetFontFace' { `ScaledFont' } -> `FontFace' outFontFaceRef*#}
#else
{-# WARNING scaledFontGetFontFace "CAIRO_CHECK_VERSION(1,2,0) unmet" #-}
scaledFontGetFontFace = undefined
#endif
-- λ https://www.cairographics.org/manual/cairo-cairo-scaled-font-t.html#cairo-scaled-font-get-font-options
#if CAIRO_CHECK_VERSION(1,2,0)
scaledFontGetFontOptions :: ScaledFont -> IO FontOptions
scaledFontGetFontOptions sf = do
  fo <- (fontOptionsCreate >>= scaledFontGetFontOptions' sf)
  fontOptionsStatus fo >>= failStatus
  return fo
  where {#fun scaled_font_get_font_options as scaledFontGetFontOptions' { `ScaledFont', `FontOptions' outFontOptions* } -> `()'#}
#else
{-# WARNING scaledFontGetFontOptions "CAIRO_CHECK_VERSION(1,2,0) unmet" #-}
scaledFontGetFontOptions = undefined
#endif

-- λ https://www.cairographics.org/manual/cairo-cairo-scaled-font-t.html#cairo-scaled-font-text-extents
{#fun scaled_font_text_extents as ^ { `ScaledFont', withUTF8String* `String', alloca- `TextExtents' peek* } -> `()'#} -- λ require CAIRO_CHECK_VERSION(1,2,0)
-- λ https://www.cairographics.org/manual/cairo-cairo-scaled-font-t.html#cairo-scaled-font-get-font-matrix
{#fun scaled_font_get_font_matrix as ^ { `ScaledFont', alloca- `Matrix Double' peek* } -> `()'#} -- λ require CAIRO_CHECK_VERSION(1,2,0)
-- λ https://www.cairographics.org/manual/cairo-cairo-scaled-font-t.html#cairo-scaled-font-get-ctm
{#fun scaled_font_get_ctm as ^ { `ScaledFont', alloca- `Matrix Double' peek* } -> `()'#} -- λ require CAIRO_CHECK_VERSION(1,2,0)
-- λ https://www.cairographics.org/manual/cairo-cairo-scaled-font-t.html#cairo-scaled-font-get-type
{#fun scaled_font_get_type as ^ { `ScaledFont' } -> `FontType'#} -- λ require CAIRO_CHECK_VERSION(1,2,0)

-- λ https://www.cairographics.org/manual/cairo-cairo-scaled-font-t.html#cairo-scaled-font-get-reference-count
{#fun scaled_font_get_reference_count as ^ { `ScaledFont' } -> `Int'#} -- λ require CAIRO_CHECK_VERSION(1,4,0)

-- λ https://www.cairographics.org/manual/cairo-cairo-scaled-font-t.html#cairo-scaled-font-text-to-glyphs
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
#else
{-# WARNING scaledFontTextToGlyphs "CAIRO_CHECK_VERSION(1,8,0) unmet" #-}
scaledFontTextToGlyphs = undefined
#endif

-- λ https://www.cairographics.org/manual/cairo-cairo-scaled-font-t.html#cairo-scaled-font-get-scale-matrix
{#fun scaled_font_get_scale_matrix as ^ { `ScaledFont', alloca- `Matrix Double' peek* } -> `()'#} -- λ require CAIRO_CHECK_VERSION(1,8,0)
