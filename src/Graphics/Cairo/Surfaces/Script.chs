#include "cairo-core.h"
#include <cairo-script.h>
{-|
Description : λ https://www.cairographics.org/manual/cairo-Script-Surfaces.html //div[@class="refnamediv"]/table/tr/td/p/text()

λ https://www.cairographics.org/manual/cairo-Script-Surfaces.html#cairo-Script-Surfaces.description
-}
module Graphics.Cairo.Surfaces.Script where

{#import Graphics.Cairo.Types#}
import Foreign.C

{#context lib="cairo" prefix="cairo"#}

-- λ https://www.cairographics.org/manual/cairo-Script-Surfaces.html#cairo-script-create
{#fun script_create as ^ { withCString* `FilePath' } -> `ScriptDevice' outDevice*#} -- λ require CAIRO_HAS_SCRIPT_SURFACE CAIRO_CHECK_VERSION(1,12,0)
-- λ https://www.cairographics.org/manual/cairo-Script-Surfaces.html#cairo-script-from-recording-surface
{#fun script_from_recording_surface as ^ { withDevice* `ScriptDevice', withSurface* `RecordingSurface' } -> `Status'#} -- λ require CAIRO_HAS_SCRIPT_SURFACE CAIRO_CHECK_VERSION(1,12,0)
-- λ https://www.cairographics.org/manual/cairo-Script-Surfaces.html#cairo-script-get-mode
{#fun script_get_mode as ^ { withDevice* `ScriptDevice' } -> `ScriptMode'#} -- λ require CAIRO_HAS_SCRIPT_SURFACE CAIRO_CHECK_VERSION(1,12,0)
-- λ https://www.cairographics.org/manual/cairo-Script-Surfaces.html#cairo-script-set-mode
{#fun script_set_mode as ^ { withDevice* `ScriptDevice', `ScriptMode' } -> `()'#} -- λ require CAIRO_HAS_SCRIPT_SURFACE CAIRO_CHECK_VERSION(1,12,0)
-- λ https://www.cairographics.org/manual/cairo-Script-Surfaces.html#cairo-script-surface-create
{#fun script_surface_create as ^ { withDevice* `ScriptDevice', `Content', CDouble `Width', CDouble `Height' } -> `ScriptSurface' outSurface*#} -- λ require CAIRO_HAS_SCRIPT_SURFACE CAIRO_CHECK_VERSION(1,12,0)
-- λ https://www.cairographics.org/manual/cairo-Script-Surfaces.html#cairo-script-surface-create-for-target
{#fun script_surface_create_for_target as ^ { withDevice* `ScriptDevice', `Surface' } -> `ScriptSurface' outSurface*#} -- λ require CAIRO_HAS_SCRIPT_SURFACE CAIRO_CHECK_VERSION(1,12,0)
-- λ https://www.cairographics.org/manual/cairo-Script-Surfaces.html#cairo-script-write-comment
{#fun script_write_comment as ^ { withDevice* `ScriptDevice', `String'& } -> `()' #} -- λ require CAIRO_HAS_SCRIPT_SURFACE CAIRO_CHECK_VERSION(1,12,0)
