#include "cairo-core.h"

module Graphics.Cairo.Drawing.Transformations where

{#import Graphics.Cairo.Types#}
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Storable

{#context lib="cairo" prefix="cairo"#}

{#fun translate as ^ { `Context', CDouble `X0', CDouble `Y0' } -> `()'#}
{#fun scale as ^ { `Context', CDouble `XX', CDouble `YY' } -> `()'#}
{#fun rotate as ^ { `Context', CDouble `Radius' } -> `()'#}
{#fun transform as ^ { `Context', with* `Matrix Double' } -> `()'#}
{#fun set_matrix as ^ { `Context', with* `Matrix Double' } -> `()'#}
{#fun get_matrix as ^ { `Context', alloca- `Matrix Double' peek*} -> `()'#}
{#fun identity_matrix as ^ { `Context' } -> `()'#}
{#fun user_to_device as ^ { `Context', withDouble* `X' peekDouble*, withDouble* `Y' peekDouble* } -> `()'#}
{#fun user_to_device_distance as ^ { `Context', withDouble* `X' peekDouble*, withDouble* `Y' peekDouble* } -> `()'#}
{#fun device_to_user as ^ { `Context', withDouble* `X' peekDouble*, withDouble* `Y' peekDouble* } -> `()'#}
{#fun device_to_user_distance as ^ { `Context', withDouble* `X' peekDouble*, withDouble* `Y' peekDouble* } -> `()'#}
