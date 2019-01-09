{-# LANGUAGE LambdaCase #-}
#include "cairo-core.h"

module Graphics.Cairo.Fonts.FontOptions where

{#import Graphics.Cairo.Types#}
import Graphics.Cairo.HasStatus

{#context lib="cairo" prefix="cairo"#}

instance HasStatus FontOptions where
  status = fontOptionsStatus

{#fun font_options_create as ^ {} -> `FontOptions'#}
{#fun font_options_copy as ^ { `FontOptions' } -> `FontOptions'#}
{#fun font_options_status as ^ { `FontOptions' } -> `Status'#}
{#fun font_options_merge as ^ { `FontOptions', `FontOptions' } -> `()'#}
{#fun font_options_hash as ^ { `FontOptions' } -> `Int'#}
{#fun font_options_equal as ^ { `FontOptions', `FontOptions' } -> `Bool'#}
{#fun font_options_set_antialias as ^ { `FontOptions', `Antialias' } -> `()'#}
{#fun font_options_get_antialias as ^ { `FontOptions' } -> `Antialias'#}
{#fun font_options_set_subpixel_order as ^ { `FontOptions', `SubpixelOrder' } -> `()'#}
{#fun font_options_get_subpixel_order as ^ { `FontOptions' } -> `SubpixelOrder'#}
{#fun font_options_set_hint_style as ^ { `FontOptions', `HintStyle' } -> `()'#}
{#fun font_options_get_hint_style as ^ { `FontOptions' } -> `HintStyle'#}
{#fun font_options_set_hint_metrics as ^ { `FontOptions', `HintMetrics' } -> `()'#}
{#fun font_options_get_hint_metrics as ^ { `FontOptions' } -> `HintMetrics'#}

#if CAIRO_CHECK_VERSION(1,16,0)
{#fun font_options_get_variations as ^ { `FontOptions' } -> `String'#}
{#fun font_options_set_variations as ^ { `FontOptions', `String' } -> `()'#}
#endif -- CAIRO_CHECK_VERSION(1,16,0)
