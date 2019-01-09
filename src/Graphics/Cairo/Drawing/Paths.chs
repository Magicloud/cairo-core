#include "cairo-core.h"

module Graphics.Cairo.Drawing.Paths where

{#import Graphics.Cairo.Types#}
import Foreign.C.Types
import Foreign.Marshal
import Foreign.Storable

{#context lib="cairo" prefix="cairo"#}

{#fun copy_path as ^ { `Context' } -> `Path' peek*#}
{#fun copy_path_flat as ^ { `Context' } -> `Path' peek*#}
{#fun append_path as ^ { `Context', with* `Path' } -> `()'#}
{#fun get_current_point as ^ { `Context', alloca- `X' peekDouble*, alloca- `Y' peekDouble* } -> `()'#}
{#fun new_path as ^ { `Context' } -> `()'#}
{#fun close_path as ^ { `Context' } -> `()'#}
{#fun arc as ^ { `Context', CDouble `X', CDouble `Y', CDouble `Radius', CDouble `Angle', CDouble `Angle' } -> `()'#}
{#fun arc_negative as ^ { `Context', CDouble `X', CDouble `Y', CDouble `Radius', CDouble `Angle', CDouble `Angle' } -> `()'#}
{#fun curve_to as ^ { `Context', CDouble `X', CDouble `Y', CDouble `X', CDouble `Y', CDouble `X', CDouble `Y' } -> `()'#}
{#fun line_to as ^ { `Context', CDouble `X', CDouble `Y' } -> `()'#}
{#fun move_to as ^ { `Context', CDouble `X', CDouble `Y' } -> `()'#}
{#fun rectangle as ^ { `Context', CDouble `X', CDouble `Y', CDouble `Width', CDouble `Height' } -> `()'#}
{#fun text_path as ^ { `Context', withUTF8String* `String' } -> `()'#}
{#fun rel_curve_to as ^ { `Context', CDouble `X', CDouble `Y', CDouble `X', CDouble `Y', CDouble `X', CDouble `Y' } -> `()'#}
{#fun rel_line_to as ^ { `Context', CDouble `X', CDouble `Y' } -> `()'#}
{#fun rel_move_to as ^ { `Context', CDouble `X', CDouble `Y' } -> `()'#}

glyphPath :: Context -> [Glyph] -> IO ()
glyphPath context glyphs =
  withArrayLen glyphs $ \len ptr -> glyphPath' context ptr len
  where {#fun glyph_path as glyphPath' { `Context', `GlyphPtr', `Int' } -> `()'#}

#if CAIRO_CHECK_VERSION(1,2,0)
{#fun new_sub_path as ^ { `Context' } -> `()'#}
#endif -- CAIRO_CHECK_VERSION(1,2,0)

#if CAIRO_CHECK_VERSION(1,6,0)
{#fun has_current_point as ^ { `Context' } -> `Bool'#}
{#fun path_extents as ^ { `Context', alloca- `X' peekDouble*, alloca- `Y' peekDouble*, alloca- `X' peekDouble*, alloca- `Y' peekDouble* } -> `()'#}
#endif -- CAIRO_CHECK_VERSION(1,6,0)
