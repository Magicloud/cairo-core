#include "cairo-core.h"
{-|
Description : λ https://www.cairographics.org/manual/cairo-Paths.html //div[@class="refnamediv"]/table/tr/td/p/text()

λ https://www.cairographics.org/manual/cairo-Paths.html#cairo-Paths.description
-}
module Graphics.Cairo.Drawing.Paths where

{#import Graphics.Cairo.Types#}
import Foreign.C.Types
import Foreign.Marshal
import Foreign.Storable

{#context lib="cairo" prefix="cairo"#}

-- λ https://www.cairographics.org/manual/cairo-Paths.html#cairo-copy-path
{#fun copy_path as ^ { `Context' } -> `Path' peek*#}
-- λ https://www.cairographics.org/manual/cairo-Paths.html#cairo-copy-path-flat
{#fun copy_path_flat as ^ { `Context' } -> `Path' peek*#}
-- λ https://www.cairographics.org/manual/cairo-Paths.html#cairo-append-path
{#fun append_path as ^ { `Context', with* `Path' } -> `()'#}
-- λ https://www.cairographics.org/manual/cairo-Paths.html#cairo-get-current-point
{#fun get_current_point as ^ { `Context', alloca- `X' peekDouble*, alloca- `Y' peekDouble* } -> `()'#}
-- λ https://www.cairographics.org/manual/cairo-Paths.html#cairo-new-path
{#fun new_path as ^ { `Context' } -> `()'#}
-- λ https://www.cairographics.org/manual/cairo-Paths.html#cairo-close-path
{#fun close_path as ^ { `Context' } -> `()'#}
-- λ https://www.cairographics.org/manual/cairo-Paths.html#cairo-arc
{#fun arc as ^ { `Context', CDouble `X', CDouble `Y', CDouble `Radius', CDouble `Angle', CDouble `Angle' } -> `()'#}
-- λ https://www.cairographics.org/manual/cairo-Paths.html#cairo-arc-negative
{#fun arc_negative as ^ { `Context', CDouble `X', CDouble `Y', CDouble `Radius', CDouble `Angle', CDouble `Angle' } -> `()'#}
-- λ https://www.cairographics.org/manual/cairo-Paths.html#cairo-curve-to
{#fun curve_to as ^ { `Context', CDouble `X', CDouble `Y', CDouble `X', CDouble `Y', CDouble `X', CDouble `Y' } -> `()'#}
-- λ https://www.cairographics.org/manual/cairo-Paths.html#cairo-line-to
{#fun line_to as ^ { `Context', CDouble `X', CDouble `Y' } -> `()'#}
-- λ https://www.cairographics.org/manual/cairo-Paths.html#cairo-move-to
{#fun move_to as ^ { `Context', CDouble `X', CDouble `Y' } -> `()'#}
-- λ https://www.cairographics.org/manual/cairo-Paths.html#cairo-rectangle
{#fun rectangle as ^ { `Context', CDouble `X', CDouble `Y', CDouble `Width', CDouble `Height' } -> `()'#}
-- λ https://www.cairographics.org/manual/cairo-Paths.html#cairo-text-path
{#fun text_path as ^ { `Context', withUTF8String* `String' } -> `()'#}
-- λ https://www.cairographics.org/manual/cairo-Paths.html#cairo-rel-curve-to
{#fun rel_curve_to as ^ { `Context', CDouble `X', CDouble `Y', CDouble `X', CDouble `Y', CDouble `X', CDouble `Y' } -> `()'#}
-- λ https://www.cairographics.org/manual/cairo-Paths.html#cairo-rel-line-to
{#fun rel_line_to as ^ { `Context', CDouble `X', CDouble `Y' } -> `()'#}
-- λ https://www.cairographics.org/manual/cairo-Paths.html#cairo-rel-move-to
{#fun rel_move_to as ^ { `Context', CDouble `X', CDouble `Y' } -> `()'#}

-- λ https://www.cairographics.org/manual/cairo-Paths.html#cairo-glyph-path
glyphPath :: Context -> [Glyph] -> IO ()
glyphPath context glyphs =
  withArrayLen glyphs $ \len ptr -> glyphPath' context ptr len
  where {#fun glyph_path as glyphPath' { `Context', `GlyphPtr', `Int' } -> `()'#}

#if CAIRO_CHECK_VERSION(1,2,0)
-- λ https://www.cairographics.org/manual/cairo-Paths.html#cairo-new-sub-path
{#fun new_sub_path as ^ { `Context' } -> `()'#}
#endif -- CAIRO_CHECK_VERSION(1,2,0)

#if CAIRO_CHECK_VERSION(1,6,0)
-- λ https://www.cairographics.org/manual/cairo-Paths.html#cairo-has-current-point
{#fun has_current_point as ^ { `Context' } -> `Bool'#}
-- λ https://www.cairographics.org/manual/cairo-Paths.html#cairo-path-extents
{#fun path_extents as ^ { `Context', alloca- `X' peekDouble*, alloca- `Y' peekDouble*, alloca- `X' peekDouble*, alloca- `Y' peekDouble* } -> `()'#}
#endif -- CAIRO_CHECK_VERSION(1,6,0)
