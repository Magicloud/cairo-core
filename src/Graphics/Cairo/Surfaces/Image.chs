#include "cairo-core.h"
{-|
Description : λ https://www.cairographics.org/manual/cairo-Image-Surfaces.html //div[@class="refnamediv"]/table/tr/td/p/text()

λ https://www.cairographics.org/manual/cairo-Image-Surfaces.html#cairo-Image-Surfaces.description
-}
module Graphics.Cairo.Surfaces.Image where

{#import Graphics.Cairo.Types#}
import Foreign.Ptr as C2HSImp

{#context lib="cairo" prefix="cairo"#}

-- λ https://www.cairographics.org/manual/cairo-Image-Surfaces.html#cairo-format-stride-for-width
{#fun format_stride_for_width as ^ { `Format', fromIntegral `WidthInt' } -> `Stride' fromIntegral#} -- λ require CAIRO_CHECK_VERSION(1,6,0)
-- λ https://www.cairographics.org/manual/cairo-Image-Surfaces.html#cairo-image-surface-create
{#fun image_surface_create as ^ { `Format', fromIntegral `WidthInt', fromIntegral `HeightInt' } -> `ImageSurface' outSurface*#}
-- λ https://www.cairographics.org/manual/cairo-Image-Surfaces.html#cairo-image-surface-get-format
{#fun image_surface_get_format as ^ { withSurface* `ImageSurface' } -> `Format'#} -- λ require CAIRO_CHECK_VERSION(1,2,0)
-- λ https://www.cairographics.org/manual/cairo-Image-Surfaces.html#cairo-image-surface-get-width
{#fun image_surface_get_width as ^ { withSurface* `ImageSurface' } -> `WidthInt' fromIntegral#}
-- λ https://www.cairographics.org/manual/cairo-Image-Surfaces.html#cairo-image-surface-get-height
{#fun image_surface_get_height as ^ { withSurface* `ImageSurface' } -> `HeightInt' fromIntegral#}
-- λ https://www.cairographics.org/manual/cairo-Image-Surfaces.html#cairo-image-surface-get-stride
{#fun image_surface_get_stride as ^ { withSurface* `ImageSurface' } -> `Stride' fromIntegral#} -- λ require CAIRO_CHECK_VERSION(1,2,0)
