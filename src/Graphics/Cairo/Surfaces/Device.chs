#include "cairo-core.h"
{-|
Description : λ https://www.cairographics.org/manual/cairo-cairo-device-t.html //div[@class="refnamediv"]/table/tr/td/p/text()

λ https://www.cairographics.org/manual/cairo-cairo-device-t.html#cairo-cairo-device-t.description
-}
module Graphics.Cairo.Surfaces.Device where

{#import Graphics.Cairo.Types#}
import           Control.Exception
import           Control.Monad
import qualified Foreign.Ptr as C2HSImp
import           Graphics.Cairo.HasStatus
import           Graphics.Cairo.Utilities.ErrorHandling

{#context lib="cairo" prefix="cairo"#}

#if CAIRO_CHECK_VERSION(1,10,0)
instance HasStatus Device where
  status = deviceStatus

-- λ https://www.cairographics.org/manual/cairo-cairo-device-t.html#cairo-device-status
{#fun device_status as ^ { `Device' } -> `Status'#}
-- λ https://www.cairographics.org/manual/cairo-cairo-device-t.html#cairo-device-finish
{#fun device_finish as ^ { `Device' } -> `()'#}
-- λ https://www.cairographics.org/manual/cairo-cairo-device-t.html#cairo-device-flush
{#fun device_flush as ^ { `Device' } -> `()'#}
-- λ https://www.cairographics.org/manual/cairo-cairo-device-t.html#cairo-device-get-type
{#fun device_get_type as ^ { `Device' } -> `DeviceType'#}
-- λ https://www.cairographics.org/manual/cairo-cairo-device-t.html#cairo-device-get-reference-count
{#fun device_get_reference_count as ^ { `Device' } -> `Int'#}

{-|
λ https://www.cairographics.org/manual/cairo-cairo-device-t.html#cairo-device-acquire
λ https://www.cairographics.org/manual/cairo-cairo-device-t.html#cairo-device-release
-}
acquireDevice :: Device -> (Device -> IO c) -> IO c
acquireDevice d f =
  bracket (deviceAcquire d) (const (deviceRelease d) <=< failStatus) (const $ f d)
  where
    {#fun device_acquire as ^ { `Device' } -> `Status'#}
    {#fun device_release as ^ { `Device' } -> `()'#}

-- λ https://www.cairographics.org/manual/cairo-cairo-device-t.html#cairo-device-observer-elapsed
{#fun device_observer_elapsed as ^ { `Device' } -> `Double'#}
-- λ https://www.cairographics.org/manual/cairo-cairo-device-t.html#cairo-device-observer-fill-elapsed
{#fun device_observer_fill_elapsed as ^ { `Device' } -> `Double'#}
-- λ https://www.cairographics.org/manual/cairo-cairo-device-t.html#cairo-device-observer-glyphs-elapsed
{#fun device_observer_glyphs_elapsed as ^ { `Device' } -> `Double'#}
-- λ https://www.cairographics.org/manual/cairo-cairo-device-t.html#cairo-device-observer-mask-elapsed
{#fun device_observer_mask_elapsed as ^ { `Device' } -> `Double'#}
-- λ https://www.cairographics.org/manual/cairo-cairo-device-t.html#cairo-device-observer-paint-elapsed
{#fun device_observer_paint_elapsed as ^ { `Device' } -> `Double'#}
-- λ https://www.cairographics.org/manual/cairo-cairo-device-t.html#cairo-device-observer-stroke-elapsed
{#fun device_observer_stroke_elapsed as ^ { `Device' } -> `Double'#}
#endif -- CAIRO_CHECK_VERSION(1,10,0)
