#include "cairo-core.h"
#include <cairo-pdf.h>
{-|
Description : λ https://www.cairographics.org/manual/cairo-PDF-Surfaces.html //div[@class="refnamediv"]/table/tr/td/p/text()

λ https://www.cairographics.org/manual/cairo-PDF-Surfaces.html#cairo-PDF-Surfaces.description
-}
module Graphics.Cairo.Surfaces.PDF where

{#import Graphics.Cairo.Types#}
import Foreign.C

{#context lib="cairo" prefix="cairo"#}

-- λ https://www.cairographics.org/manual/cairo-PDF-Surfaces.html#cairo-pdf-surface-create
{#fun pdf_surface_create as ^ { withCString* `FilePath', CDouble `WidthPoints', CDouble `HeightPoints' } -> `PdfSurface' outSurface* #} -- λ require CAIRO_HAS_PDF_SURFACE CAIRO_CHECK_VERSION(1,2,0)
-- λ https://www.cairographics.org/manual/cairo-PDF-Surfaces.html#cairo-pdf-surface-set-size
{#fun pdf_surface_set_size as ^ { withSurface* `PdfSurface', CDouble `WidthPoints', CDouble `HeightPoints' } -> `()'#} -- λ require CAIRO_HAS_PDF_SURFACE CAIRO_CHECK_VERSION(1,2,0)
-- λ https://www.cairographics.org/manual/cairo-PDF-Surfaces.html#cairo-pdf-surface-restrict-to-version
{#fun pdf_surface_restrict_to_version as ^ { withSurface* `PdfSurface', `PdfVersion' } -> `()' #} -- λ require CAIRO_HAS_PDF_SURFACE CAIRO_CHECK_VERSION(1,10,0)
-- λ https://www.cairographics.org/manual/cairo-PDF-Surfaces.html#cairo-pdf-version-to-string
{#fun pure pdf_version_to_string as ^ { `PdfVersion' } -> `String'#} -- λ require CAIRO_HAS_PDF_SURFACE CAIRO_CHECK_VERSION(1,10,0)

-- λ https://www.cairographics.org/manual/cairo-PDF-Surfaces.html#cairo-pdf-get-versions
#if CAIRO_CHECK_VERSION(1,10,0)
#if CAIRO_HAS_PDF_SURFACE
pdfGetVersions :: [PdfVersion]
pdfGetVersions = [minBound..maxBound]
#else
{-# WARNING pdfGetVersions "CAIRO_HAS_PDF_SURFACE unmet" #-}
pdfGetVersions = undefined
#endif
#else
{-# WARNING pdfGetVersions "CAIRO_CHECK_VERSION(1,10,0) unmet" #-}
pdfGetVersions = undefined
#endif

-- λ https://www.cairographics.org/manual/cairo-PDF-Surfaces.html#cairo-pdf-surface-add-outline
{#fun pdf_surface_add_outline as ^ { withSurface* `PdfSurface', fromIntegral `OutlineId', withUTF8String* `String', `String', `PdfOutlineFlags'} -> `OutlineId' fromIntegral#} -- λ require CAIRO_HAS_PDF_SURFACE CAIRO_CHECK_VERSION(1,16,0)
-- λ https://www.cairographics.org/manual/cairo-PDF-Surfaces.html#cairo-pdf-surface-set-metadata
{#fun pdf_surface_set_metadata as ^ { withSurface* `PdfSurface', `PdfMetadata', withUTF8String* `String'} -> `()'#} -- λ require CAIRO_HAS_PDF_SURFACE CAIRO_CHECK_VERSION(1,16,0)
-- λ https://www.cairographics.org/manual/cairo-PDF-Surfaces.html#cairo-pdf-surface-set-page-label
{#fun pdf_surface_set_page_label as ^ { withSurface* `PdfSurface', withUTF8String* `String' } -> `()'#} -- λ require CAIRO_HAS_PDF_SURFACE CAIRO_CHECK_VERSION(1,16,0)
-- λ https://www.cairographics.org/manual/cairo-PDF-Surfaces.html#cairo-pdf-surface-set-thumbnail-size
{#fun pdf_surface_set_thumbnail_size as ^ { withSurface* `PdfSurface', fromIntegral `WidthInt', fromIntegral `HeightInt' } -> `()'#} -- λ require CAIRO_HAS_PDF_SURFACE CAIRO_CHECK_VERSION(1,16,0)
