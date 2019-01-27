#include "cairo-core.h"
{-|
Description : λ https://www.cairographics.org/manual/cairo-cairo-surface-t.html //div[@class="refnamediv"]/table/tr/td/p/text()

λ https://www.cairographics.org/manual/cairo-cairo-surface-t.html#cairo-cairo-surface-t.description
-}
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

-- λ https://www.cairographics.org/manual/cairo-cairo-surface-t.html#cairo-surface-create-similar
{#fun surface_create_similar as ^ { `Surface', `Content', fromIntegral `WidthInt', fromIntegral `HeightInt' } -> `Surface' outSurface*#}
-- λ https://www.cairographics.org/manual/cairo-cairo-surface-t.html#cairo-surface-status
{#fun surface_status as ^ { `Surface' } -> `Status'#}
-- λ https://www.cairographics.org/manual/cairo-cairo-surface-t.html#cairo-surface-finish
{#fun surface_finish as ^ { `Surface' } -> `()'#}
-- λ https://www.cairographics.org/manual/cairo-cairo-surface-t.html#cairo-surface-flush
{#fun surface_flush as ^ { `Surface' } -> `()'#}
-- λ https://www.cairographics.org/manual/cairo-cairo-surface-t.html#cairo-surface-get-font-options
{#fun surface_get_font_options as ^ { `Surface', `FontOptions'} -> `()'#}
-- λ https://www.cairographics.org/manual/cairo-cairo-surface-t.html#cairo-surface-mark-dirty
{#fun surface_mark_dirty as ^ { `Surface' } -> `()'#}
-- λ https://www.cairographics.org/manual/cairo-cairo-surface-t.html#cairo-surface-mark-dirty-rectangle
{#fun surface_mark_dirty_rectangle as ^ { `Surface', fromIntegral `XInt', fromIntegral `YInt', fromIntegral `WidthInt', fromIntegral `HeightInt' } -> `()'#}
-- λ https://www.cairographics.org/manual/cairo-cairo-surface-t.html#cairo-surface-set-device-offset
{#fun surface_set_device_offset as ^ { `Surface', CDouble `XOffset', CDouble `YOffset' } -> `()'#}

#if CAIRO_CHECK_VERSION(1,2,0)
-- λ https://www.cairographics.org/manual/cairo-cairo-surface-t.html#cairo-surface-get-content
{#fun surface_get_content as ^ { `Surface' } -> `Content'#}
-- λ https://www.cairographics.org/manual/cairo-cairo-surface-t.html#cairo-surface-get-device-offset
{#fun surface_get_device_offset as ^ { `Surface', alloca- `XOffset' peekDouble*, alloca- `YOffset' peekDouble* } -> `()'#}
-- λ https://www.cairographics.org/manual/cairo-cairo-surface-t.html#cairo-surface-set-fallback-resolution
{#fun surface_set_fallback_resolution as ^ { `Surface', CDouble `XResolution', CDouble `YResolution' } -> `()'#}
-- λ https://www.cairographics.org/manual/cairo-cairo-surface-t.html#cairo-surface-get-type
{#fun surface_get_type as ^ { `Surface' } -> `SurfaceType'#}
#endif -- CAIRO_CHECK_VERSION(1,2,0)

#if CAIRO_CHECK_VERSION(1,4,0)
-- λ https://www.cairographics.org/manual/cairo-cairo-surface-t.html#cairo-surface-get-reference-count
{#fun surface_get_reference_count as ^ { `Surface' } -> `Int'#}
#endif -- CAIRO_CHECK_VERSION(1,4,0)

#if CAIRO_CHECK_VERSION(1,6,0)
-- λ https://www.cairographics.org/manual/cairo-cairo-surface-t.html#cairo-surface-copy-page
{#fun surface_copy_page as ^ { `Surface' } -> `()'#}
-- λ https://www.cairographics.org/manual/cairo-cairo-surface-t.html#cairo-surface-show-page
{#fun surface_show_page as ^ { `Surface' } -> `()'#}
#endif -- CAIRO_CHECK_VERSION(1,6,0)

#if CAIRO_CHECK_VERSION(1,8,0)
-- λ https://www.cairographics.org/manual/cairo-cairo-surface-t.html#cairo-surface-has-show-text-glyphs
{#fun surface_has_show_text_glyphs as ^ { `Surface' } -> `Bool'#}
-- λ https://www.cairographics.org/manual/cairo-cairo-surface-t.html#cairo-surface-get-fallback-resolution
{#fun surface_get_fallback_resolution as ^ { `Surface', alloca- `XResolution' peekDouble*, alloca- `YResolution' peekDouble* } -> `()'#}
#endif -- CAIRO_CHECK_VERSION(1,8,0)

#if CAIRO_CHECK_VERSION(1,10,0)
-- λ https://www.cairographics.org/manual/cairo-cairo-surface-t.html#cairo-surface-create-for-rectangle
{#fun surface_create_for_rectangle as ^ { `Surface', CDouble `X', CDouble `Y', CDouble `Width', CDouble `Height'} -> `Surface' outSurface*#}
-- λ https://www.cairographics.org/manual/cairo-cairo-surface-t.html#cairo-surface-get-device
{#fun surface_get_device as ^ { `Surface' } -> `Maybe Device' outMaybeDevice*#}
#endif -- CAIRO_CHECK_VERSION(1,10,0)

#if CAIRO_CHECK_VERSION(1,12,0)
-- λ https://www.cairographics.org/manual/cairo-cairo-surface-t.html#cairo-surface-create-similar-image
{#fun surface_create_similar_image as ^ { `Surface', `Format', fromIntegral `WidthInt', fromIntegral `HeightInt' } -> `Surface' outSurface*#}

{-|
λ https://www.cairographics.org/manual/cairo-cairo-surface-t.html#cairo-surface-map-to-image
λ https://www.cairographics.org/manual/cairo-cairo-surface-t.html#cairo-surface-unmap-image
-}
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

-- λ https://www.cairographics.org/manual/cairo-cairo-surface-t.html#cairo-surface-supports-mime-type
{#fun surface_supports_mime_type as ^ { `Surface', `String' } -> `Bool'#}
#endif -- CAIRO_CHECK_VERSION(1,12,0)

#if CAIRO_CHECK_VERSION(1,14,0)
-- λ https://www.cairographics.org/manual/cairo-cairo-surface-t.html#cairo-surface-get-device-scale
{#fun surface_get_device_scale as ^ { `Surface', alloca- `XScale' peekDouble*, alloca- `YScale' peekDouble* } -> `()'#}
-- λ https://www.cairographics.org/manual/cairo-cairo-surface-t.html#cairo-surface-set-device-scale
{#fun surface_set_device_scale as ^ { `Surface', CDouble `XScale', CDouble `YScale' } -> `()'#}
#endif -- CAIRO_CHECK_VERSION(1,14,0)
