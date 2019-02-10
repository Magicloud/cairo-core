#include "cairo-core.h"
{-|
Description : λ https://www.cairographics.org/manual/cairo-PNG-Support.html //div[@class="refnamediv"]/table/tr/td/p/text()

λ https://www.cairographics.org/manual/cairo-PNG-Support.html#cairo-PNG-Support.description
-}
module Graphics.Cairo.Surfaces.PNG where

{#import Graphics.Cairo.Types#}
import Foreign.C.String

{#context lib="cairo" prefix="cairo"#}

-- λ https://www.cairographics.org/manual/cairo-PNG-Support.html#cairo-image-surface-create-from-png
{#fun image_surface_create_from_png as ^ { withCString* `FilePath' } -> `PngSurface' outSurface*#} -- λ require CAIRO_HAS_PNG_FUNCTIONS
-- λ https://www.cairographics.org/manual/cairo-PNG-Support.html#cairo-surface-write-to-png
{#fun surface_write_to_png as ^ { withSurface* `PngSurface', withCString* `FilePath' } -> `Status'#} -- λ require CAIRO_HAS_PNG_FUNCTIONS
