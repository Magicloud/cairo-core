#include "cairo-core.h"

module Graphics.Cairo.Drawing.TagsLinks where

{#import Graphics.Cairo.Types#}

{#context lib="cairo" prefix="cairo"#}

#if CAIRO_CHECK_VERSION(1,16,0)
{#fun tag_begin as ^ { `Context', `String', `String' } -> `()'#}
{#fun tag_end as ^ { `Context', `String' } -> `()'#}
#endif -- CAIRO_CHECK_VERSION(1,16,0)
