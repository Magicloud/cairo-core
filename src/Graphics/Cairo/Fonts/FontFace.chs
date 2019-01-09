#include "cairo-core.h"

module Graphics.Cairo.Fonts.FontFace where

{#import Graphics.Cairo.Types#}
import qualified Foreign.Ptr as C2HSImp
import Graphics.Cairo.HasStatus

{#context lib="cairo" prefix="cairo"#}

instance HasStatus FontFace where
  status = fontFaceStatus

{#fun font_face_status as ^ { `FontFace' } -> `Status'#}

#if CAIRO_CHECK_VERSION(1,2,0)
{#fun font_face_get_type as ^ { `FontFace' } -> `FontType'#}
#endif -- CAIRO_CHECK_VERSION(1,2,0)

#if CAIRO_CHECK_VERSION(1,4,0)
{#fun font_face_get_reference_count as ^ { `FontFace' } -> `Int'#}
#endif -- CAIRO_CHECK_VERSION(1,4,0)
