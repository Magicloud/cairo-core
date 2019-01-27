#include "cairo-core.h"
{-|
Description : λ https://www.cairographics.org/manual/cairo-Transformations.html //div[@class="refnamediv"]/table/tr/td/p/text()

λ https://www.cairographics.org/manual/cairo-Transformations.html#cairo-Transformations.functions_details
-}
module Graphics.Cairo.Drawing.Transformations where

{#import Graphics.Cairo.Types#}
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Storable

{#context lib="cairo" prefix="cairo"#}

-- λ https://www.cairographics.org/manual/cairo-Transformations.html#cairo-translate
{#fun translate as ^ { `Context', CDouble `X0', CDouble `Y0' } -> `()'#}
-- λ https://www.cairographics.org/manual/cairo-Transformations.html#cairo-scale
{#fun scale as ^ { `Context', CDouble `XX', CDouble `YY' } -> `()'#}
-- λ https://www.cairographics.org/manual/cairo-Transformations.html#cairo-rotate
{#fun rotate as ^ { `Context', CDouble `Radius' } -> `()'#}
-- λ https://www.cairographics.org/manual/cairo-Transformations.html#cairo-transform
{#fun transform as ^ { `Context', with* `Matrix Double' } -> `()'#}
-- λ https://www.cairographics.org/manual/cairo-Transformations.html#cairo-set-matrix
{#fun set_matrix as ^ { `Context', with* `Matrix Double' } -> `()'#}
-- λ https://www.cairographics.org/manual/cairo-Transformations.html#cairo-get-matrix
{#fun get_matrix as ^ { `Context', alloca- `Matrix Double' peek*} -> `()'#}
-- λ https://www.cairographics.org/manual/cairo-Transformations.html#cairo-identity-matrix
{#fun identity_matrix as ^ { `Context' } -> `()'#}
-- λ https://www.cairographics.org/manual/cairo-Transformations.html#cairo-user-to-device
{#fun user_to_device as ^ { `Context', withDouble* `X' peekDouble*, withDouble* `Y' peekDouble* } -> `()'#}
-- λ https://www.cairographics.org/manual/cairo-Transformations.html#cairo-user-to-device-distance
{#fun user_to_device_distance as ^ { `Context', withDouble* `X' peekDouble*, withDouble* `Y' peekDouble* } -> `()'#}
-- λ https://www.cairographics.org/manual/cairo-Transformations.html#cairo-device-to-user
{#fun device_to_user as ^ { `Context', withDouble* `X' peekDouble*, withDouble* `Y' peekDouble* } -> `()'#}
-- λ https://www.cairographics.org/manual/cairo-Transformations.html#cairo-device-to-user-distance
{#fun device_to_user_distance as ^ { `Context', withDouble* `X' peekDouble*, withDouble* `Y' peekDouble* } -> `()'#}
