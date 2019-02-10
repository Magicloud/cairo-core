#include "cairo-core.h"
{-|
Description : λ https://www.cairographics.org/manual/cairo-cairo-font-options-t.html //div[@class="refnamediv"]/table/tr/td/p/text()

λ https://www.cairographics.org/manual/cairo-cairo-font-options-t.html#cairo-cairo-font-options-t.description
-}
module Graphics.Cairo.Fonts.FontOptions where

{#import Graphics.Cairo.Types#}
import Graphics.Cairo.HasStatus

{#context lib="cairo" prefix="cairo"#}

instance HasStatus FontOptions where
  status = fontOptionsStatus

-- λ https://www.cairographics.org/manual/cairo-cairo-font-options-t.html#cairo-font-options-create
{#fun font_options_create as ^ {} -> `FontOptions'#}
-- λ https://www.cairographics.org/manual/cairo-cairo-font-options-t.html#cairo-font-options-copy
{#fun font_options_copy as ^ { `FontOptions' } -> `FontOptions'#}
-- λ https://www.cairographics.org/manual/cairo-cairo-font-options-t.html#cairo-font-options-status
{#fun font_options_status as ^ { `FontOptions' } -> `Status'#}
-- λ https://www.cairographics.org/manual/cairo-cairo-font-options-t.html#cairo-font-options-merge
{#fun font_options_merge as ^ { `FontOptions', `FontOptions' } -> `()'#}
-- λ https://www.cairographics.org/manual/cairo-cairo-font-options-t.html#cairo-font-options-hash
{#fun font_options_hash as ^ { `FontOptions' } -> `Int'#}
-- λ https://www.cairographics.org/manual/cairo-cairo-font-options-t.html#cairo-font-options-equal
{#fun font_options_equal as ^ { `FontOptions', `FontOptions' } -> `Bool'#}
-- λ https://www.cairographics.org/manual/cairo-cairo-font-options-t.html#cairo-font-options-set-antialias
{#fun font_options_set_antialias as ^ { `FontOptions', `Antialias' } -> `()'#}
-- λ https://www.cairographics.org/manual/cairo-cairo-font-options-t.html#cairo-font-options-get-antialias
{#fun font_options_get_antialias as ^ { `FontOptions' } -> `Antialias'#}
-- λ https://www.cairographics.org/manual/cairo-cairo-font-options-t.html#cairo-font-options-set-subpixel-order
{#fun font_options_set_subpixel_order as ^ { `FontOptions', `SubpixelOrder' } -> `()'#}
-- λ https://www.cairographics.org/manual/cairo-cairo-font-options-t.html#cairo-font-options-get-subpixel-order
{#fun font_options_get_subpixel_order as ^ { `FontOptions' } -> `SubpixelOrder'#}
-- λ https://www.cairographics.org/manual/cairo-cairo-font-options-t.html#cairo-font-options-set-hint-style
{#fun font_options_set_hint_style as ^ { `FontOptions', `HintStyle' } -> `()'#}
-- λ https://www.cairographics.org/manual/cairo-cairo-font-options-t.html#cairo-font-options-get-hint-style
{#fun font_options_get_hint_style as ^ { `FontOptions' } -> `HintStyle'#}
-- λ https://www.cairographics.org/manual/cairo-cairo-font-options-t.html#cairo-font-options-set-hint-metrics
{#fun font_options_set_hint_metrics as ^ { `FontOptions', `HintMetrics' } -> `()'#}
-- λ https://www.cairographics.org/manual/cairo-cairo-font-options-t.html#cairo-font-options-get-hint-metrics
{#fun font_options_get_hint_metrics as ^ { `FontOptions' } -> `HintMetrics'#}

-- λ https://www.cairographics.org/manual/cairo-cairo-font-options-t.html#cairo-font-options-get-variations
{#fun font_options_get_variations as ^ { `FontOptions' } -> `String'#} -- λ require CAIRO_CHECK_VERSION(1,16,0)
-- λ https://www.cairographics.org/manual/cairo-cairo-font-options-t.html#cairo-font-options-set-variations
{#fun font_options_set_variations as ^ { `FontOptions', `String' } -> `()'#} -- λ require CAIRO_CHECK_VERSION(1,16,0)
