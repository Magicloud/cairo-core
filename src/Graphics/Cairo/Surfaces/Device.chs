#include "cairo-core.h"

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

{#fun device_status as ^ { `Device' } -> `Status'#}
{#fun device_finish as ^ { `Device' } -> `()'#}
{#fun device_flush as ^ { `Device' } -> `()'#}
{#fun device_get_type as ^ { `Device' } -> `DeviceType'#}
{#fun device_get_reference_count as ^ { `Device' } -> `Int'#}

acquireDevice :: Device -> (Device -> IO c) -> IO c
acquireDevice d f =
  bracket (deviceAcquire d) (const (deviceRelease d) <=< failStatus) (const $ f d)
  where
    {#fun device_acquire as ^ { `Device' } -> `Status'#}
    {#fun device_release as ^ { `Device' } -> `()'#}

{#fun device_observer_elapsed as ^ { `Device' } -> `Double'#}
{#fun device_observer_fill_elapsed as ^ { `Device' } -> `Double'#}
{#fun device_observer_glyphs_elapsed as ^ { `Device' } -> `Double'#}
{#fun device_observer_mask_elapsed as ^ { `Device' } -> `Double'#}
{#fun device_observer_paint_elapsed as ^ { `Device' } -> `Double'#}
{#fun device_observer_stroke_elapsed as ^ { `Device' } -> `Double'#}
#endif -- CAIRO_CHECK_VERSION(1,10,0)
