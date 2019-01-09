#include "cairo-core.h"

module Graphics.Cairo.Drawing.Cairo where

{#import Graphics.Cairo.Types#}
import Foreign.C.Types
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable
import Graphics.Cairo.HasStatus

{#context lib="cairo" prefix="cairo"#}

instance HasStatus Context where
  status = Graphics.Cairo.Drawing.Cairo.status

{#fun create as ^ { `Surface' } -> `Context' outContext*#}
{#fun get_target as ^ { `Context' } -> `Surface' outSurfaceRef*#}
{#fun get_source as ^ { `Context' } -> `Pattern' outPatternRef*#}
{#fun status as ^ { `Context' } -> `Status'#}
{#fun save as ^ { `Context' } -> `()'#}
{#fun restore as ^ { `Context' } -> `()'#}
{#fun set_source_rgb as ^ { `Context', CDouble `Red', CDouble `Green', CDouble `Blue' } -> `()'#}
{#fun set_source_rgba as ^ { `Context', CDouble `Red', CDouble `Green', CDouble `Blue', CDouble `Alpha' } -> `()'#}
{#fun set_source as ^ { `Context', `Pattern' } -> `()'#}
{#fun set_source_surface as ^ { `Context', `Surface', CDouble `X', CDouble `Y' } -> `()'#}
{#fun set_antialias as ^ { `Context', `Antialias' } -> `()'#}
{#fun get_antialias as ^ { `Context' } -> `Antialias'#}
{#fun set_fill_rule as ^ { `Context', `FillRule' } -> `()'#}
{#fun get_fill_rule as ^ { `Context' } -> `FillRule'#}
{#fun set_line_cap as ^ { `Context', `LineCap' } -> `()'#}
{#fun get_line_cap as ^ { `Context' } -> `LineCap'#}
{#fun set_line_join as ^ { `Context', `LineJoin' } -> `()'#}
{#fun get_line_join as ^ { `Context' } -> `LineJoin'#}
{#fun set_line_width as ^ { `Context', `Double' } -> `()'#}
{#fun get_line_width as ^ { `Context' } -> `Double'#}
{#fun set_miter_limit as ^ { `Context', `Double' } -> `()'#}
{#fun get_miter_limit as ^ { `Context' } -> `Double'#}
{#fun set_operator as ^ { `Context', `Operator' } -> `()'#}
{#fun get_operator as ^ { `Context' } -> `Operator'#}
{#fun set_tolerance as ^ { `Context', `Double' } -> `()'#}
{#fun get_tolerance as ^ { `Context' } -> `Double'#}
{#fun clip as ^ { `Context' } -> `()'#}
{#fun clip_preserve as ^ { `Context' } -> `()'#}
{#fun reset_clip as ^ { `Context' } -> `()'#}
{#fun fill as ^ { `Context' } -> `()'#}
{#fun fill_preserve as ^ { `Context' } -> `()'#}
{#fun fill_extents as ^ { `Context', alloca- `X' peekDouble*, alloca- `Y' peekDouble*, alloca- `X' peekDouble*, alloca- `Y' peekDouble* } -> `()'#}
{#fun in_fill as ^ { `Context', CDouble `X', CDouble `Y' } -> `Bool'#}
{#fun mask as ^ { `Context', `Pattern' } -> `()'#}
{#fun mask_surface as ^ { `Context', `Surface', CDouble `X', CDouble `Y' } -> `()'#}
{#fun paint as ^ { `Context' } -> `()'#}
{#fun paint_with_alpha as ^ { `Context', CDouble `Alpha' } -> `()'#}
{#fun stroke as ^ { `Context' } -> `()'#}
{#fun stroke_preserve as ^ { `Context' } -> `()'#}
{#fun stroke_extents as ^ { `Context', alloca- `X' peekDouble*, alloca- `Y' peekDouble*, alloca- `X' peekDouble*, alloca- `Y' peekDouble* } -> `()'#}
{#fun in_stroke as ^ { `Context', CDouble `X', CDouble `Y' } -> `Bool'#}
{#fun copy_page as ^ { `Context' } -> `()'#}
{#fun show_page as ^ { `Context' } -> `()'#}

#if CAIRO_CHECK_VERSION(1,2,0)
{#fun pop_group as ^ { `Context' } -> `Pattern' outPattern*#}
{#fun get_group_target as ^ { `Context' } -> `Surface' outSurfaceRef*#}
{#fun push_group as ^ { `Context' } -> `()'#}
{#fun push_group_with_content as ^ { `Context', `Content' } -> `()'#}
{#fun pop_group_to_source as ^ { `Context' } -> `()'#}
#endif -- CAIRO_CHECK_VERSION(1,2,0)

#if CAIRO_CHECK_VERSION(1,4,0)
setDash :: Context -> [Double] -> Offset -> IO ()
setDash context dashes offset =
  withArrayLen (map (CDouble) dashes) $ \len ptr -> setDash' context ptr len offset
  where {#fun set_dash as setDash' { `Context', id `Ptr CDouble', `Int', CDouble `Offset' } -> `()'#}

getDash :: Context -> IO ([Double], Offset)
getDash context = do
  len <- getDashCount context
  allocaArray len $ \ptr -> do
    offset <- getDash' context ptr
    dashes <- (map (\(CDouble x) -> x)) <$> peekArray len ptr
    return (dashes, offset)
  where {#fun get_dash as getDash' { `Context', id `Ptr CDouble', alloca- `Offset' peekDouble* } -> `()'#}

{#fun get_dash_count as ^ { `Context' } -> `Int'#}
{#fun clip_extents as ^ { `Context', alloca- `X' peekDouble*, alloca- `Y' peekDouble*, alloca- `X' peekDouble*, alloca- `Y' peekDouble* } -> `()'#}
{#fun copy_clip_rectangle_list as ^ { `Context' } -> `RectangleList Double' peek*#}
{#fun get_reference_count as ^ { `Context' } -> `Int'#}
#endif -- CAIRO_CHECK_VERSION(1,4,0)

#if CAIRO_CHECK_VERSION(1,10,0)
{#fun in_clip as ^ { `Context', CDouble `X', CDouble `Y' } -> `Bool'#}
#endif -- CAIRO_CHECK_VERSION(1,10,0)
