#include "cairo-core.h"
{-|
Description : λ https://www.cairographics.org/manual/cairo-Version-Information.html //div[@class="refnamediv"]/table/tr/td/p/text()

λ https://www.cairographics.org/manual/cairo-Version-Information.html#cairo-Version-Information.description
-}
module Graphics.Cairo.Utilities.VersionInformation where

import Graphics.Cairo.Types

{#context lib="cairo" prefix="cairo"#}

-- λ https://www.cairographics.org/manual/cairo-Version-Information.html#cairo-version
{#fun pure version as versionNumber {} -> `Int'#}
-- λ https://www.cairographics.org/manual/cairo-Version-Information.html#cairo-version-string
{#fun pure version_string as ^ {} -> `String'#}

instance Show Version where
  show _ = versionString

version :: Version
version = Version {#const CAIRO_VERSION_MAJOR#} {#const CAIRO_VERSION_MINOR#} {#const CAIRO_VERSION_MICRO#}
