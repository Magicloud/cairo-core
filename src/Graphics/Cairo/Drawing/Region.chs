#include "cairo-core.h"
{-|
Description : λ https://www.cairographics.org/manual/cairo-Regions.html //div[@class="refnamediv"]/table/tr/td/p/text()

λ https://www.cairographics.org/manual/cairo-Regions.html#cairo-Regions.description
-}
module Graphics.Cairo.Drawing.Region where

{#import Graphics.Cairo.Types#}
import           Foreign.Marshal
import qualified Foreign.Ptr as C2HSImp
import           Foreign.Storable
import           Graphics.Cairo.HasStatus
import           Graphics.Cairo.Utilities.ErrorHandling

{#context lib="cairo" prefix="cairo"#}

#if CAIRO_CHECK_VERSION(1,10,0)
instance HasStatus Region where
  status = regionStatus
#endif

-- λ https://www.cairographics.org/manual/cairo-Regions.html#cairo-region-create
{#fun region_create as ^ {} -> `Region' outRegion*#} -- λ require CAIRO_CHECK_VERSION(1,10,0)
-- λ https://www.cairographics.org/manual/cairo-Regions.html#cairo-region-create-rectangle
{#fun region_create_rectangle as ^ { with* `Rectangle Int' } -> `Region' outRegion*#} -- λ require CAIRO_CHECK_VERSION(1,10,0)
-- λ https://www.cairographics.org/manual/cairo-Regions.html#cairo-region-create-rectangles
{#fun region_create_rectangles as ^ {`RectangleIntPtr', `Int' } -> `Region' outRegion*#} -- λ require CAIRO_CHECK_VERSION(1,10,0)
-- λ https://www.cairographics.org/manual/cairo-Regions.html#cairo-region-copy
{#fun region_copy as ^ { `Region' } -> `Region' outRegion*#} -- λ require CAIRO_CHECK_VERSION(1,10,0)
-- λ https://www.cairographics.org/manual/cairo-Regions.html#cairo-region-status
{#fun region_status as ^ { `Region' } -> `Status'#} -- λ require CAIRO_CHECK_VERSION(1,10,0)
-- λ https://www.cairographics.org/manual/cairo-Regions.html#cairo-region-get-extents
{#fun region_get_extents as ^ { `Region', alloca- `Rectangle Int' peek* } -> `()'#} -- λ require CAIRO_CHECK_VERSION(1,10,0)
-- λ https://www.cairographics.org/manual/cairo-Regions.html#cairo-region-num-rectangles
{#fun region_num_rectangles as ^ { `Region' } -> `Int'#} -- λ require CAIRO_CHECK_VERSION(1,10,0)
-- λ https://www.cairographics.org/manual/cairo-Regions.html#cairo-region-get-rectangle
{#fun region_get_rectangle as ^ { `Region', fromIntegral `Index', alloca- `Rectangle Int' peek* } -> `()'#} -- λ require CAIRO_CHECK_VERSION(1,10,0)
-- λ https://www.cairographics.org/manual/cairo-Regions.html#cairo-region-is-empty
{#fun region_is_empty as ^ { `Region' } -> `Bool'#} -- λ require CAIRO_CHECK_VERSION(1,10,0)
-- λ https://www.cairographics.org/manual/cairo-Regions.html#cairo-region-contains-point
{#fun region_contains_point as ^ { `Region', fromIntegral `XInt', fromIntegral `YInt' } -> `Bool'#} -- λ require CAIRO_CHECK_VERSION(1,10,0)
-- λ https://www.cairographics.org/manual/cairo-Regions.html#cairo-region-contains-rectangle
{#fun region_contains_rectangle as ^ { `Region', with* `Rectangle Int' } -> `RegionOverlap'#} -- λ require CAIRO_CHECK_VERSION(1,10,0)
-- λ https://www.cairographics.org/manual/cairo-Regions.html#cairo-region-equal
{#fun region_equal as ^ { `Region', `Region' } -> `Bool'#} -- λ require CAIRO_CHECK_VERSION(1,10,0)
-- λ https://www.cairographics.org/manual/cairo-Regions.html#cairo-region-translate
{#fun region_translate as ^ { `Region', fromIntegral `XInt', fromIntegral `YInt' } -> `()'#} -- λ require CAIRO_CHECK_VERSION(1,10,0)
-- λ https://www.cairographics.org/manual/cairo-Regions.html#cairo-region-intersect
{#fun region_intersect as ^ { `Region', `Region' } -> `()' failStatusRaw*#} -- λ require CAIRO_CHECK_VERSION(1,10,0)
-- λ https://www.cairographics.org/manual/cairo-Regions.html#cairo-region-intersect-rectangle
{#fun region_intersect_rectangle as ^ { `Region', with* `Rectangle Int' } -> `()' failStatusRaw*#} -- λ require CAIRO_CHECK_VERSION(1,10,0)
-- λ https://www.cairographics.org/manual/cairo-Regions.html#cairo-region-subtract
{#fun region_subtract as ^ { `Region', `Region' } -> `()' failStatusRaw*#} -- λ require CAIRO_CHECK_VERSION(1,10,0)
-- λ https://www.cairographics.org/manual/cairo-Regions.html#cairo-region-subtract-rectangle
{#fun region_subtract_rectangle as ^ { `Region', with* `Rectangle Int' } -> `()' failStatusRaw*#} -- λ require CAIRO_CHECK_VERSION(1,10,0)
-- λ https://www.cairographics.org/manual/cairo-Regions.html#cairo-region-union
{#fun region_union as ^ { `Region', `Region' } -> `()' failStatusRaw*#} -- λ require CAIRO_CHECK_VERSION(1,10,0)
-- λ https://www.cairographics.org/manual/cairo-Regions.html#cairo-region-union-rectangle
{#fun region_union_rectangle as ^ { `Region', with* `Rectangle Int' } -> `()' failStatusRaw*#} -- λ require CAIRO_CHECK_VERSION(1,10,0)
-- λ https://www.cairographics.org/manual/cairo-Regions.html#cairo-region-xor
{#fun region_xor as ^ { `Region', `Region' } -> `()' failStatusRaw*#} -- λ require CAIRO_CHECK_VERSION(1,10,0)
-- λ https://www.cairographics.org/manual/cairo-Regions.html#cairo-region-xor-rectangle
{#fun region_xor_rectangle as ^ { `Region', with* `Rectangle Int' } -> `()' failStatusRaw*#} -- λ require CAIRO_CHECK_VERSION(1,10,0)
