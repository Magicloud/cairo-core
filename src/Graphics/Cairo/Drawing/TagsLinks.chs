#include "cairo-core.h"
{-|
Description : λ https://www.cairographics.org/manual/cairo-Tags-and-Links.html //div[@class="refnamediv"]/table/tr/td/p/text()

λ https://www.cairographics.org/manual/cairo-Tags-and-Links.html#cairo-Tags-and-Links.description
-}
module Graphics.Cairo.Drawing.TagsLinks where

{#import Graphics.Cairo.Types#}

{#context lib="cairo" prefix="cairo"#}

#if CAIRO_CHECK_VERSION(1,16,0)
-- λ https://www.cairographics.org/manual/cairo-Tags-and-Links.html#cairo-tag-begin
{#fun tag_begin as ^ { `Context', `String', `String' } -> `()'#}
-- λ https://www.cairographics.org/manual/cairo-Tags-and-Links.html#cairo-tag-end
{#fun tag_end as ^ { `Context', `String' } -> `()'#}
#endif -- CAIRO_CHECK_VERSION(1,16,0)
