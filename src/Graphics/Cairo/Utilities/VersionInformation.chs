#include "cairo-core.h"

module Graphics.Cairo.Utilities.VersionInformation where

import Graphics.Cairo.Types

{#context lib="cairo" prefix="cairo"#}

{#fun pure version as versionNumber {} -> `Int'#}
{#fun pure version_string as ^ {} -> `String'#}

instance Show Version where
  show _ = versionString

version :: Version
version = Version {#const CAIRO_VERSION_MAJOR#} {#const CAIRO_VERSION_MINOR#} {#const CAIRO_VERSION_MICRO#}
