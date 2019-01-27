#include "cairo-core.h"
{-|
Description : λ https://www.cairographics.org/manual/cairo-cairo-font-face-t.html //div[@class="refnamediv"]/table/tr/td/p/text()

λ https://www.cairographics.org/manual/cairo-cairo-font-face-t.html#cairo-cairo-font-face-t.description
-}
module Graphics.Cairo.Fonts.FontFace where

{#import Graphics.Cairo.Types#}
import qualified Foreign.Ptr as C2HSImp
import           Graphics.Cairo.HasStatus

{#context lib="cairo" prefix="cairo"#}

instance HasStatus FontFace where
  status = fontFaceStatus

-- λ https://www.cairographics.org/manual/cairo-cairo-font-face-t.html#cairo-font-face-status
{#fun font_face_status as ^ { `FontFace' } -> `Status'#}

#if CAIRO_CHECK_VERSION(1,2,0)
-- λ https://www.cairographics.org/manual/cairo-cairo-font-face-t.html#cairo-font-face-get-type
{#fun font_face_get_type as ^ { `FontFace' } -> `FontType'#}
#endif -- CAIRO_CHECK_VERSION(1,2,0)

#if CAIRO_CHECK_VERSION(1,4,0)
-- λ https://www.cairographics.org/manual/cairo-cairo-font-face-t.html#cairo-font-face-get-reference-count
{#fun font_face_get_reference_count as ^ { `FontFace' } -> `Int'#}
#endif -- CAIRO_CHECK_VERSION(1,4,0)
