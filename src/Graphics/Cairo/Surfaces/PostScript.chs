#include "cairo-core.h"
#include <cairo-ps.h>
{-|
Description : λ https://www.cairographics.org/manual/cairo-PostScript-Surfaces.html //div[@class="refnamediv"]/table/tr/td/p/text()

λ https://www.cairographics.org/manual/cairo-PostScript-Surfaces.html#cairo-PostScript-Surfaces.description
-}
module Graphics.Cairo.Surfaces.PostScript where

{#import Graphics.Cairo.Types#}
import Foreign.C

{#context lib="cairo" prefix="cairo"#}

-- λ https://www.cairographics.org/manual/cairo-PostScript-Surfaces.html#cairo-ps-surface-create
{#fun ps_surface_create as ^ { withCString* `FilePath', CDouble `WidthPoints', CDouble `HeightPoints' } -> `PsSurface' outSurface*#} -- λ require CAIRO_HAS_PS_SURFACE CAIRO_CHECK_VERSION(1,2,0)
-- λ https://www.cairographics.org/manual/cairo-PostScript-Surfaces.html#cairo-ps-surface-set-size
{#fun ps_surface_set_size as ^ { withSurface* `PsSurface', CDouble `WidthPoints', CDouble `HeightPoints' } -> `()'#} -- λ require CAIRO_HAS_PS_SURFACE CAIRO_CHECK_VERSION(1,2,0)
-- λ https://www.cairographics.org/manual/cairo-PostScript-Surfaces.html#cairo-ps-surface-dsc-begin-setup
{#fun ps_surface_dsc_begin_setup as ^ { withSurface* `PsSurface' } -> `()'#} -- λ require CAIRO_HAS_PS_SURFACE CAIRO_CHECK_VERSION(1,2,0)
-- λ https://www.cairographics.org/manual/cairo-PostScript-Surfaces.html#cairo-ps-surface-dsc-begin-page-setup
{#fun ps_surface_dsc_begin_page_setup as ^ { withSurface* `PsSurface' } -> `()'#} -- λ require CAIRO_HAS_PS_SURFACE CAIRO_CHECK_VERSION(1,2,0)
-- λ https://www.cairographics.org/manual/cairo-PostScript-Surfaces.html#cairo-ps-surface-dsc-comment
{#fun ps_surface_dsc_comment as ^ { withSurface* `PsSurface', `String' } -> `()'#} -- λ require CAIRO_HAS_PS_SURFACE CAIRO_CHECK_VERSION(1,2,0)
-- λ https://www.cairographics.org/manual/cairo-PostScript-Surfaces.html#cairo-ps-surface-restrict-to-level
{#fun ps_surface_restrict_to_level as ^ { withSurface* `PsSurface', `PsLevel' } -> `()'#} -- λ require CAIRO_HAS_PS_SURFACE CAIRO_CHECK_VERSION(1,6,0)
-- λ https://www.cairographics.org/manual/cairo-PostScript-Surfaces.html#cairo-ps-level-to-string
{#fun pure ps_level_to_string as ^ { `PsLevel' } -> `String'#} -- λ require CAIRO_HAS_PS_SURFACE CAIRO_CHECK_VERSION(1,6,0)
-- λ https://www.cairographics.org/manual/cairo-PostScript-Surfaces.html#cairo-ps-surface-set-eps
{#fun ps_surface_set_eps as ^ { withSurface* `PsSurface', `Bool' } -> `()'#} -- λ require CAIRO_HAS_PS_SURFACE CAIRO_CHECK_VERSION(1,6,0)
-- λ https://www.cairographics.org/manual/cairo-PostScript-Surfaces.html#cairo-ps-surface-get-eps
{#fun ps_surface_get_eps as ^ { withSurface* `PsSurface' } -> `Bool'#} -- λ require CAIRO_HAS_PS_SURFACE CAIRO_CHECK_VERSION(1,6,0)

-- λ https://www.cairographics.org/manual/cairo-PostScript-Surfaces.html#cairo-ps-get-levels
#if CAIRO_CHECK_VERSION(1,6,0)
#if CAIRO_HAS_PS_SURFACE
psGetLevels :: [PsLevel]
psGetLevels = [minBound..maxBound]
#else
{-# WARNING psGetLevels "CAIRO_HAS_PS_SURFACE unmet" #-}
psGetLevels = undefined
#endif
#else
{-# WARNING psGetLevels "CAIRO_CHECK_VERSION(1,6,0) unmet" #-}
psGetLevels = undefined
#endif
