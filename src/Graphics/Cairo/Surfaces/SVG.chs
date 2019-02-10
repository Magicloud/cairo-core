#include "cairo-core.h"
#include <cairo-svg.h>
{-|
Description : λ https://www.cairographics.org/manual/cairo-SVG-Surfaces.html //div[@class="refnamediv"]/table/tr/td/p/text()

λ https://www.cairographics.org/manual/cairo-SVG-Surfaces.html#cairo-SVG-Surfaces.description
-}
module Graphics.Cairo.Surfaces.SVG where

{#import Graphics.Cairo.Types#}
import Foreign.C

{#context lib="cairo" prefix="cairo"#}

-- λ https://www.cairographics.org/manual/cairo-SVG-Surfaces.html#cairo-svg-surface-create
{#fun svg_surface_create as ^ { withCString* `FilePath', CDouble `WidthPoints', CDouble `HeightPoints' } -> `SvgSurface' outSurface* #} -- λ require CAIRO_HAS_SVG_SURFACE CAIRO_CHECK_VERSION(1,2,0)
-- λ https://www.cairographics.org/manual/cairo-SVG-Surfaces.html#cairo-svg-surface-restrict-to-version
{#fun svg_surface_restrict_to_version as ^ { withSurface* `SvgSurface', `SvgVersion' } -> `()'#} -- λ require CAIRO_HAS_SVG_SURFACE CAIRO_CHECK_VERSION(1,2,0)
-- λ https://www.cairographics.org/manual/cairo-SVG-Surfaces.html#cairo-svg-version-to-string
{#fun pure svg_version_to_string as ^ { `SvgVersion' } -> `String'#} -- λ require CAIRO_HAS_SVG_SURFACE CAIRO_CHECK_VERSION(1,2,0)

-- λ https://www.cairographics.org/manual/cairo-SVG-Surfaces.html#cairo-svg-get-versions
#if CAIRO_CHECK_VERSION(1,2,0)
#if CAIRO_HAS_SVG_SURFACE
svgGetVersions :: [SvgVersion]
svgGetVersions = [minBound..maxBound]
#else
{-# WARNING svgGetVersions "CAIRO_HAS_PDF_SURFACE unmet" #-}
svgGetVersions = undefined
#endif
#else
{-# WARNING svgGetVersions "CAIRO_CHECK_VERSION(1,2,0) unmet" #-}
svgGetVersions = undefined
#endif

-- λ https://www.cairographics.org/manual/cairo-SVG-Surfaces.html#cairo-svg-surface-get-document-unit
{#fun svg_surface_get_document_unit as ^ { withSurface* `SvgSurface' } -> `SvgUnit'#} -- λ require CAIRO_HAS_SVG_SURFACE CAIRO_CHECK_VERSION(1,16,0)
-- λ https://www.cairographics.org/manual/cairo-SVG-Surfaces.html#cairo-svg-surface-set-document-unit
{#fun svg_surface_set_document_unit as ^ { withSurface* `SvgSurface', `SvgUnit' } -> `()'#} -- λ require CAIRO_HAS_SVG_SURFACE CAIRO_CHECK_VERSION(1,16,0)
