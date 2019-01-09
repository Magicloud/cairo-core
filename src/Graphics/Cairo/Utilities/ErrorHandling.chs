#include "cairo-core.h"

module Graphics.Cairo.Utilities.ErrorHandling where

{#import Graphics.Cairo.Types#}
import Control.Monad

{#context lib="cairo" prefix="cairo"#}

{#fun status_to_string as ^ { `Status' } -> `String'#}
{#fun debug_reset_static_data as ^ {} -> `()'#}

failStatus :: Status -> IO ()
failStatus s = when (s /= StatusSuccess) $ statusToString s >>= fail
failStatusRaw :: (Integral a) => a -> IO ()
failStatusRaw = failStatus . toEnum . fromIntegral
