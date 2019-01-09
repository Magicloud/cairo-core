#include "cairo-core.h"

module Graphics.Cairo.Surfaces.Surface where

{#import Graphics.Cairo.Types#}
import Control.Exception
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Graphics.Cairo.HasStatus
import Graphics.Cairo.Utilities.ErrorHandling

{#context lib="cairo" prefix="cairo"#}

instance HasStatus Surface where
  status = surfaceStatus

{#fun surface_create_similar as ^ { `Surface', `Content', fromIntegral `WidthInt', fromIntegral `HeightInt' } -> `Surface' outSurface*#}
{#fun surface_status as ^ { `Surface' } -> `Status'#}
{#fun surface_finish as ^ { `Surface' } -> `()'#}
{#fun surface_flush as ^ { `Surface' } -> `()'#}
{#fun surface_get_font_options as ^ { `Surface', `FontOptions'} -> `()'#}
{#fun surface_mark_dirty as ^ { `Surface' } -> `()'#}
{#fun surface_mark_dirty_rectangle as ^ { `Surface', fromIntegral `XInt', fromIntegral `YInt', fromIntegral `WidthInt', fromIntegral `HeightInt' } -> `()'#}
{#fun surface_set_device_offset as ^ { `Surface', CDouble `XOffset', CDouble `YOffset' } -> `()'#}

#if CAIRO_CHECK_VERSION(1,2,0)
{#fun surface_get_content as ^ { `Surface' } -> `Content'#}
{#fun surface_get_device_offset as ^ { `Surface', alloca- `XOffset' peekDouble*, alloca- `YOffset' peekDouble* } -> `()'#}
{#fun surface_set_fallback_resolution as ^ { `Surface', CDouble `XResolution', CDouble `YResolution' } -> `()'#}
{#fun surface_get_type as ^ { `Surface' } -> `SurfaceType'#}
#endif -- CAIRO_CHECK_VERSION(1,2,0)

#if CAIRO_CHECK_VERSION(1,4,0)
{#fun surface_get_reference_count as ^ { `Surface' } -> `Int'#}
#endif -- CAIRO_CHECK_VERSION(1,4,0)

#if CAIRO_CHECK_VERSION(1,6,0)
{#fun surface_copy_page as ^ { `Surface' } -> `()'#}
{#fun surface_show_page as ^ { `Surface' } -> `()'#}
#endif -- CAIRO_CHECK_VERSION(1,6,0)

#if CAIRO_CHECK_VERSION(1,8,0)
{#fun surface_has_show_text_glyphs as ^ { `Surface' } -> `Bool'#}
{#fun surface_get_fallback_resolution as ^ { `Surface', alloca- `XResolution' peekDouble*, alloca- `YResolution' peekDouble* } -> `()'#}
#endif -- CAIRO_CHECK_VERSION(1,8,0)

#if CAIRO_CHECK_VERSION(1,10,0)
{#fun surface_create_for_rectangle as ^ { `Surface', CDouble `X', CDouble `Y', CDouble `Width', CDouble `Height'} -> `Surface' outSurface*#}
{#fun surface_get_device as ^ { `Surface' } -> `Maybe Device' outMaybeDevice*#}
#endif -- CAIRO_CHECK_VERSION(1,10,0)

#if CAIRO_CHECK_VERSION(1,12,0)
{#fun surface_create_similar_image as ^ { `Surface', `Format', fromIntegral `WidthInt', fromIntegral `HeightInt' } -> `Surface' outSurface*#}

useSurfaceMapImage :: Surface
                      -> Maybe (Rectangle Int) -> (Surface -> IO c) -> IO c
useSurfaceMapImage s mr f =
  bracket (surfaceMapToImage s mr) (\ns -> do
    st <- surfaceStatus ns
    surfaceUnmapImage s ns
    failStatus st) f
  where
    {#fun surface_map_to_image as ^ { `Surface', withMaybe* `Maybe (Rectangle Int)' } -> `Surface' outSurface*#}
    {#fun surface_unmap_image as ^ { `Surface', `Surface' } -> `()'#}

{#fun surface_supports_mime_type as ^ { `Surface', `String' } -> `Bool'#}
#endif -- CAIRO_CHECK_VERSION(1,12,0)

#if CAIRO_CHECK_VERSION(1,14,0)
{#fun surface_get_device_scale as ^ { `Surface', alloca- `XScale' peekDouble*, alloca- `YScale' peekDouble* } -> `()'#}
{#fun surface_set_device_scale as ^ { `Surface', CDouble `XScale', CDouble `YScale' } -> `()'#}
#endif -- CAIRO_CHECK_VERSION(1,14,0)
