#include "cairo-core.h"
{-|
Description : λ https://www.cairographics.org/manual/cairo-cairo-t.html //div[@class="refnamediv"]/table/tr/td/p/text()

λ https://www.cairographics.org/manual/cairo-cairo-t.html#cairo-cairo-t.description
-}
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

-- λ https://www.cairographics.org/manual/cairo-cairo-t.html#cairo-create
{#fun create as ^ { `Surface' } -> `Context' outContext*#}
-- λ https://www.cairographics.org/manual/cairo-cairo-t.html#cairo-get-target
{#fun get_target as ^ { `Context' } -> `Surface' outSurfaceRef*#}
-- λ https://www.cairographics.org/manual/cairo-cairo-t.html#cairo-get-source
{#fun get_source as ^ { `Context' } -> `Pattern' outPatternRef*#}
-- λ https://www.cairographics.org/manual/cairo-cairo-t.html#cairo-status
{#fun status as ^ { `Context' } -> `Status'#}
-- λ https://www.cairographics.org/manual/cairo-cairo-t.html#cairo-save
{#fun save as ^ { `Context' } -> `()'#}
-- λ https://www.cairographics.org/manual/cairo-cairo-t.html#cairo-restore
{#fun restore as ^ { `Context' } -> `()'#}
-- λ https://www.cairographics.org/manual/cairo-cairo-t.html#cairo-set-source-rgb
{#fun set_source_rgb as ^ { `Context', CDouble `Red', CDouble `Green', CDouble `Blue' } -> `()'#}
-- λ https://www.cairographics.org/manual/cairo-cairo-t.html#cairo-set-source-rgba
{#fun set_source_rgba as ^ { `Context', CDouble `Red', CDouble `Green', CDouble `Blue', CDouble `Alpha' } -> `()'#}
-- λ https://www.cairographics.org/manual/cairo-cairo-t.html#cairo-set-source
{#fun set_source as ^ { `Context', `Pattern' } -> `()'#}
-- λ https://www.cairographics.org/manual/cairo-cairo-t.html#cairo-set-source-surface
{#fun set_source_surface as ^ { `Context', `Surface', CDouble `X', CDouble `Y' } -> `()'#}
-- λ https://www.cairographics.org/manual/cairo-cairo-t.html#cairo-set-antialias
{#fun set_antialias as ^ { `Context', `Antialias' } -> `()'#}
-- λ https://www.cairographics.org/manual/cairo-cairo-t.html#cairo-get-antialias
{#fun get_antialias as ^ { `Context' } -> `Antialias'#}
-- λ https://www.cairographics.org/manual/cairo-cairo-t.html#cairo-set-fill-rule
{#fun set_fill_rule as ^ { `Context', `FillRule' } -> `()'#}
-- λ https://www.cairographics.org/manual/cairo-cairo-t.html#cairo-get-fill-rule
{#fun get_fill_rule as ^ { `Context' } -> `FillRule'#}
-- λ https://www.cairographics.org/manual/cairo-cairo-t.html#cairo-set-line-cap
{#fun set_line_cap as ^ { `Context', `LineCap' } -> `()'#}
-- λ https://www.cairographics.org/manual/cairo-cairo-t.html#cairo-get-line-cap
{#fun get_line_cap as ^ { `Context' } -> `LineCap'#}
-- λ https://www.cairographics.org/manual/cairo-cairo-t.html#cairo-set-line-join
{#fun set_line_join as ^ { `Context', `LineJoin' } -> `()'#}
-- λ https://www.cairographics.org/manual/cairo-cairo-t.html#cairo-get-line-join
{#fun get_line_join as ^ { `Context' } -> `LineJoin'#}
-- λ https://www.cairographics.org/manual/cairo-cairo-t.html#cairo-set-line-width
{#fun set_line_width as ^ { `Context', `Double' } -> `()'#}
-- λ https://www.cairographics.org/manual/cairo-cairo-t.html#cairo-get-line-width
{#fun get_line_width as ^ { `Context' } -> `Double'#}
-- λ https://www.cairographics.org/manual/cairo-cairo-t.html#cairo-set-miter-limit
{#fun set_miter_limit as ^ { `Context', `Double' } -> `()'#}
-- λ https://www.cairographics.org/manual/cairo-cairo-t.html#cairo-get-miter-limit
{#fun get_miter_limit as ^ { `Context' } -> `Double'#}
-- λ https://www.cairographics.org/manual/cairo-cairo-t.html#cairo-set-operator
{#fun set_operator as ^ { `Context', `Operator' } -> `()'#}
-- λ https://www.cairographics.org/manual/cairo-cairo-t.html#cairo-get-operator
{#fun get_operator as ^ { `Context' } -> `Operator'#}
-- λ https://www.cairographics.org/manual/cairo-cairo-t.html#cairo-set-tolerance
{#fun set_tolerance as ^ { `Context', `Double' } -> `()'#}
-- λ https://www.cairographics.org/manual/cairo-cairo-t.html#cairo-get-tolerance
{#fun get_tolerance as ^ { `Context' } -> `Double'#}
-- λ https://www.cairographics.org/manual/cairo-cairo-t.html#cairo-clip
{#fun clip as ^ { `Context' } -> `()'#}
-- λ https://www.cairographics.org/manual/cairo-cairo-t.html#cairo-clip-preserve
{#fun clip_preserve as ^ { `Context' } -> `()'#}
-- λ https://www.cairographics.org/manual/cairo-cairo-t.html#cairo-reset-clip
{#fun reset_clip as ^ { `Context' } -> `()'#}
-- λ https://www.cairographics.org/manual/cairo-cairo-t.html#cairo-fill
{#fun fill as ^ { `Context' } -> `()'#}
-- λ https://www.cairographics.org/manual/cairo-cairo-t.html#cairo-fill-preserve
{#fun fill_preserve as ^ { `Context' } -> `()'#}
-- λ https://www.cairographics.org/manual/cairo-cairo-t.html#cairo-fill-extents
{#fun fill_extents as ^ { `Context', alloca- `X' peekDouble*, alloca- `Y' peekDouble*, alloca- `X' peekDouble*, alloca- `Y' peekDouble* } -> `()'#}
-- λ https://www.cairographics.org/manual/cairo-cairo-t.html#cairo-in-fill
{#fun in_fill as ^ { `Context', CDouble `X', CDouble `Y' } -> `Bool'#}
-- λ https://www.cairographics.org/manual/cairo-cairo-t.html#cairo-mask
{#fun mask as ^ { `Context', `Pattern' } -> `()'#}
-- λ https://www.cairographics.org/manual/cairo-cairo-t.html#cairo-mask-surface
{#fun mask_surface as ^ { `Context', `Surface', CDouble `X', CDouble `Y' } -> `()'#}
-- λ https://www.cairographics.org/manual/cairo-cairo-t.html#cairo-paint
{#fun paint as ^ { `Context' } -> `()'#}
-- λ https://www.cairographics.org/manual/cairo-cairo-t.html#cairo-paint-with-alpha
{#fun paint_with_alpha as ^ { `Context', CDouble `Alpha' } -> `()'#}
-- λ https://www.cairographics.org/manual/cairo-cairo-t.html#cairo-stroke
{#fun stroke as ^ { `Context' } -> `()'#}
-- λ https://www.cairographics.org/manual/cairo-cairo-t.html#cairo-stroke-preserve
{#fun stroke_preserve as ^ { `Context' } -> `()'#}
-- λ https://www.cairographics.org/manual/cairo-cairo-t.html#cairo-stroke-extents
{#fun stroke_extents as ^ { `Context', alloca- `X' peekDouble*, alloca- `Y' peekDouble*, alloca- `X' peekDouble*, alloca- `Y' peekDouble* } -> `()'#}
-- λ https://www.cairographics.org/manual/cairo-cairo-t.html#cairo-in-stroke
{#fun in_stroke as ^ { `Context', CDouble `X', CDouble `Y' } -> `Bool'#}
-- λ https://www.cairographics.org/manual/cairo-cairo-t.html#cairo-copy-page
{#fun copy_page as ^ { `Context' } -> `()'#}
-- λ https://www.cairographics.org/manual/cairo-cairo-t.html#cairo-show-page
{#fun show_page as ^ { `Context' } -> `()'#}

-- λ https://www.cairographics.org/manual/cairo-cairo-t.html#cairo-push-group
{#fun push_group as ^ { `Context' } -> `()'#} -- λ require CAIRO_CHECK_VERSION(1,2,0)
-- λ https://www.cairographics.org/manual/cairo-cairo-t.html#cairo-push-group-with-content
{#fun push_group_with_content as ^ { `Context', `Content' } -> `()'#} -- λ require CAIRO_CHECK_VERSION(1,2,0)
-- λ https://www.cairographics.org/manual/cairo-cairo-t.html#cairo-pop-group
{#fun pop_group as ^ { `Context' } -> `Pattern' outPattern*#} -- λ require CAIRO_CHECK_VERSION(1,2,0)
-- λ https://www.cairographics.org/manual/cairo-cairo-t.html#cairo-pop-group-to-source
{#fun pop_group_to_source as ^ { `Context' } -> `()'#} -- λ require CAIRO_CHECK_VERSION(1,2,0)
-- λ https://www.cairographics.org/manual/cairo-cairo-t.html#cairo-get-group-target
{#fun get_group_target as ^ { `Context' } -> `Surface' outSurfaceRef*#} -- λ require CAIRO_CHECK_VERSION(1,2,0)

-- λ https://www.cairographics.org/manual/cairo-cairo-t.html#cairo-set-dash
#if CAIRO_CHECK_VERSION(1,4,0)
setDash :: Context -> [Double] -> Offset -> IO ()
setDash context dashes offset =
  setDash' context (map CDouble dashes) offset
  where {#fun set_dash as setDash' { `Context', withArrayLen_* `[CDouble]'&, CDouble `Offset' } -> `()'#}
#else
{-# WARNING setDash "CAIRO_CHECK_VERSION(1,4,0) unmet" #-}
setDash = undefined
#endif

-- λ https://www.cairographics.org/manual/cairo-cairo-t.html#cairo-get-dash
#if CAIRO_CHECK_VERSION(1,4,0)
getDash :: Context -> IO ([Double], Offset)
getDash context = do
  len <- getDashCount context
  allocaArray len $ \ptr -> do
    offset <- getDash' context ptr
    dashes <- (map (\(CDouble x) -> x)) <$> peekArray len ptr
    return (dashes, offset)
  where {#fun get_dash as getDash' { `Context', id `Ptr CDouble', alloca- `Offset' peekDouble* } -> `()'#}
#else
{-# WARNING getDash "CAIRO_CHECK_VERSION(1,4,0) unmet" #-}
getDash = undefined
#endif

-- λ https://www.cairographics.org/manual/cairo-cairo-t.html#cairo-get-dash-count
{#fun get_dash_count as ^ { `Context' } -> `Int'#} -- λ require CAIRO_CHECK_VERSION(1,4,0)
-- λ https://www.cairographics.org/manual/cairo-cairo-t.html#cairo-clip-extents
{#fun clip_extents as ^ { `Context', alloca- `X' peekDouble*, alloca- `Y' peekDouble*, alloca- `X' peekDouble*, alloca- `Y' peekDouble* } -> `()'#} -- λ require CAIRO_CHECK_VERSION(1,4,0)
-- λ https://www.cairographics.org/manual/cairo-cairo-t.html#cairo-copy-clip-rectangle-list
{#fun copy_clip_rectangle_list as ^ { `Context' } -> `RectangleList Double' peek*#} -- λ require CAIRO_CHECK_VERSION(1,4,0)
-- λ https://www.cairographics.org/manual/cairo-cairo-t.html#cairo-get-reference-count
{#fun get_reference_count as ^ { `Context' } -> `Int'#} -- λ require CAIRO_CHECK_VERSION(1,4,0)

-- λ https://www.cairographics.org/manual/cairo-cairo-t.html#cairo-in-clip
{#fun in_clip as ^ { `Context', CDouble `X', CDouble `Y' } -> `Bool'#} -- λ require CAIRO_CHECK_VERSION(1,10,0)
