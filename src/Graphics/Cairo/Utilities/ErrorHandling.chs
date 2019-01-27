#include "cairo-core.h"
{-|
Description : λ https://www.cairographics.org/manual/cairo-Error-handling.html //div[@class="refnamediv"]/table/tr/td/p/text()

λ https://www.cairographics.org/manual/cairo-Error-handling.html#cairo-Error-handling.description
-}
module Graphics.Cairo.Utilities.ErrorHandling where

{#import Graphics.Cairo.Types#}
import Control.Monad

{#context lib="cairo" prefix="cairo"#}

-- λ https://www.cairographics.org/manual/cairo-Error-handling.html#cairo-status-to-string
{#fun status_to_string as ^ { `Status' } -> `String'#}
-- λ https://www.cairographics.org/manual/cairo-Error-handling.html#cairo-debug-reset-static-data
{#fun debug_reset_static_data as ^ {} -> `()'#}

failStatus :: Status -> IO ()
failStatus s = when (s /= StatusSuccess) $ statusToString s >>= fail
failStatusRaw :: (Integral a) => a -> IO ()
failStatusRaw = failStatus . toEnum . fromIntegral
