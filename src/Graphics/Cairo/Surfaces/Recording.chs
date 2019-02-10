#include "cairo-core.h"
#include <cairo-features.h>
{-|
Description : λ https://www.cairographics.org/manual/cairo-Recording-Surfaces.html //div[@class="refnamediv"]/table/tr/td/p/text()

λ https://www.cairographics.org/manual/cairo-Recording-Surfaces.html#cairo-Recording-Surfaces.description
-}
module Graphics.Cairo.Surfaces.Recording where

{#import Graphics.Cairo.Types#}
import Foreign.Marshal
import Foreign.Storable

{#context lib="cairo" prefix="cairo"#}

-- λ https://www.cairographics.org/manual/cairo-Recording-Surfaces.html#cairo-recording-surface-create
{#fun recording_surface_create as ^ { `Content', with* `Rectangle Double' } -> `RecordingSurface' outSurface*#} -- λ require CAIRO_HAS_RECORDING_SURFACE CAIRO_CHECK_VERSION(1,10,0)
-- λ https://www.cairographics.org/manual/cairo-Recording-Surfaces.html#cairo-recording-surface-ink-extents
{#fun recording_surface_ink_extents as ^ { withSurface* `RecordingSurface', alloca- `X' peekDouble*, alloca- `Y' peekDouble*, alloca- `Width' peekDouble*, alloca- `Height' peekDouble* } -> `()'#} -- λ require CAIRO_HAS_RECORDING_SURFACE CAIRO_CHECK_VERSION(1,10,0)
-- λ https://www.cairographics.org/manual/cairo-Recording-Surfaces.html#cairo-recording-surface-get-extents
{#fun recording_surface_get_extents as ^ { withSurface* `RecordingSurface', alloca- `Rectangle Double' peek* } -> `Bool'#} -- λ require CAIRO_HAS_RECORDING_SURFACE CAIRO_CHECK_VERSION(1,12,0)
