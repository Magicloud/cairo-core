#include "cairo-core.h"

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

{#fun region_create as ^ {} -> `Region' outRegion*#}
{#fun region_create_rectangles as ^ {`RectangleIntPtr', `Int' } -> `Region' outRegion*#}
{#fun region_create_rectangle as ^ { with* `Rectangle Int' } -> `Region' outRegion*#}
{#fun region_copy as ^ { `Region' } -> `Region' outRegion*#}
{#fun region_status as ^ { `Region' } -> `Status'#}
{#fun region_get_extents as ^ { `Region', alloca- `Rectangle Int' peek* } -> `()'#}
{#fun region_num_rectangles as ^ { `Region' } -> `Int'#}
{#fun region_get_rectangle as ^ { `Region', fromIntegral `Index', alloca- `Rectangle Int' peek* } -> `()'#}
{#fun region_is_empty as ^ { `Region' } -> `Bool'#}
{#fun region_contains_point as ^ { `Region', fromIntegral `XInt', fromIntegral `YInt' } -> `Bool'#}
{#fun region_contains_rectangle as ^ { `Region', with* `Rectangle Int' } -> `RegionOverlap'#}
{#fun region_equal as ^ { `Region', `Region' } -> `Bool'#}
{#fun region_translate as ^ { `Region', fromIntegral `XInt', fromIntegral `YInt' } -> `()'#}
{#fun region_intersect as ^ { `Region', `Region' } -> `()' failStatusRaw*#}
{#fun region_intersect_rectangle as ^ { `Region', with* `Rectangle Int' } -> `()' failStatusRaw*#}
{#fun region_subtract as ^ { `Region', `Region' } -> `()' failStatusRaw*#}
{#fun region_subtract_rectangle as ^ { `Region', with* `Rectangle Int' } -> `()' failStatusRaw*#}
{#fun region_union as ^ { `Region', `Region' } -> `()' failStatusRaw*#}
{#fun region_union_rectangle as ^ { `Region', with* `Rectangle Int' } -> `()' failStatusRaw*#}
{#fun region_xor as ^ { `Region', `Region' } -> `()' failStatusRaw*#}
{#fun region_xor_rectangle as ^ { `Region', with* `Rectangle Int' } -> `()' failStatusRaw*#}
#endif -- CAIRO_CHECK_VERSION(1,10,0)
