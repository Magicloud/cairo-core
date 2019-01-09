#include "cairo-core.h"

module Graphics.Cairo.Drawing.Patterns where

{#import Graphics.Cairo.Types#}
import Control.Exception
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Storable
import Graphics.Cairo.HasStatus
import Graphics.Cairo.Utilities.ErrorHandling

{#context lib="cairo" prefix="cairo"#}

instance HasStatus Pattern where
  status = patternStatus

{#fun pattern_create_rgb as ^ { CDouble `Red', CDouble `Green', CDouble `Blue' } -> `SolidPattern' outPattern*#}
{#fun pattern_create_rgba as ^ { CDouble `Red', CDouble `Green', CDouble `Blue', CDouble `Alpha' } -> `SolidPattern' outPattern*#}
{#fun pattern_create_for_surface as ^ { `Surface' } -> `SurfacePattern' outPattern*#}
{#fun pattern_create_linear as ^ { CDouble `X', CDouble `Y', CDouble `X', CDouble `Y' } -> `LinearGradientPattern' outPattern*#}
{#fun pattern_create_radial as ^ { CDouble `X', CDouble `Y', CDouble `Radius', CDouble `X', CDouble `Y', CDouble `Radius' } -> `RadialGradientPattern' outPattern*#}
{#fun pattern_add_color_stop_rgb as ^ { withPattern* `GradientPattern', CDouble `Offset', CDouble `Red', CDouble `Green', CDouble `Blue' } -> `()'#}
{#fun pattern_add_color_stop_rgba as ^ { withPattern* `GradientPattern', CDouble `Offset', CDouble `Red', CDouble `Green', CDouble `Blue', CDouble `Alpha' } -> `()'#}
{#fun pattern_status as ^ { `Pattern' } -> `Status'#}
{#fun pattern_set_extend as ^ { withPattern* `SurfacePattern', `Extend' } -> `()'#}
{#fun pattern_get_extend as ^ { withPattern* `SurfacePattern' } -> `Extend'#}
{#fun pattern_set_filter as ^ { withPattern* `SurfacePattern', `Filter' } -> `()'#}
{#fun pattern_get_filter as ^ { withPattern* `SurfacePattern' } -> `Filter'#}
{#fun pattern_set_matrix as ^ { `Pattern', with* `Matrix Double' } -> `()'#}
{#fun pattern_get_matrix as ^ { `Pattern', alloca- `Matrix Double' peek*} -> `()'#}

#if CAIRO_CHECK_VERSION(1,2,0)
{#fun pattern_get_type as ^ { `Pattern' } -> `PatternType'#}
#endif -- CAIRO_CHECK_VERSION(1,2,0)

#if CAIRO_CHECK_VERSION(1,4,0)
{#fun pattern_get_color_stop_count as ^ { withPattern* `GradientPattern', alloca- `Int' peekInt* } -> `()' failStatusRaw*#}
{#fun pattern_get_color_stop_rgba as ^ { withPattern* `GradientPattern', fromIntegral `Index', alloca- `Double' peekDouble*, alloca- `Red' peekDouble*, alloca- `Green' peekDouble*, alloca- `Blue' peekDouble*, alloca- `Alpha' peekDouble* } -> `()' failStatusRaw*#}
{#fun pattern_get_rgba as ^ { withPattern* `SolidPattern', alloca- `Red' peekDouble*, alloca- `Green' peekDouble*, alloca- `Blue' peekDouble*, alloca- `Alpha' peekDouble* } -> `()' failStatusRaw*#}
{#fun pattern_get_surface as ^ { withPattern* `SurfacePattern', alloca- `Surface' outSurfacePtrRef* } -> `()' failStatusRaw*#}
{#fun pattern_get_linear_points as ^ { withPattern* `LinearGradientPattern', alloca- `X' peekDouble*, alloca- `Y' peekDouble*, alloca- `X' peekDouble*, alloca- `Y' peekDouble* } -> `()' failStatusRaw*#}
{#fun pattern_get_radial_circles as ^ { withPattern* `RadialGradientPattern', alloca- `X' peekDouble*, alloca- `Y' peekDouble*, alloca- `Radius' peekDouble*, alloca- `X' peekDouble*, alloca- `Y' peekDouble*, alloca- `Radius' peekDouble* } -> `()' failStatusRaw*#}
{#fun pattern_get_reference_count as ^ { `Pattern' } -> `Int'#}
#endif -- CAIRO_CHECK_VERSION(1,4,0)

#if CAIRO_CHECK_VERSION(1,12,0)
meshPatternPatch :: Mesh -> (Mesh -> IO c) -> IO c
meshPatternPatch mesh f =
  bracket (meshPatternBeginPatch mesh) (const $ do
    meshPatternEndPatch mesh
    patternStatus mesh >>= failStatus) (const $ f mesh)
  where
    {#fun mesh_pattern_begin_patch as ^ { withPattern* `Mesh' } -> `()'#}
    {#fun mesh_pattern_end_patch as ^ { withPattern* `Mesh' } -> `()'#}

{#fun pattern_create_mesh as ^ {} -> `Mesh' outPattern*#}
{#fun mesh_pattern_move_to as ^ { withPattern* `Mesh', CDouble `X', CDouble `Y' } -> `()'#}
{#fun mesh_pattern_line_to as ^ { withPattern* `Mesh', CDouble `X', CDouble `Y' } -> `()'#}
{#fun mesh_pattern_curve_to as ^ { withPattern* `Mesh', CDouble `X', CDouble `Y', CDouble `X', CDouble `Y', CDouble `X', CDouble `Y' } -> `()'#}
{#fun mesh_pattern_set_control_point as ^ { withPattern* `Mesh', cFromEnum `MeshPointNum', CDouble `X', CDouble `Y' } -> `()'#}
{#fun mesh_pattern_set_corner_color_rgb as ^ { withPattern* `Mesh', cFromEnum `MeshCornerNum', CDouble `Red', CDouble `Green', CDouble `Blue' } -> `()'#}
{#fun mesh_pattern_set_corner_color_rgba as ^ { withPattern* `Mesh', cFromEnum `MeshCornerNum', CDouble `Red', CDouble `Green', CDouble `Blue', CDouble `Alpha' } -> `()'#}
{#fun mesh_pattern_get_patch_count as ^ { withPattern* `Mesh', alloca- `Int' peekInt*} -> `()' failStatusRaw*#}
{#fun mesh_pattern_get_path as ^ { withPattern* `Mesh', `Int' } -> `Path' peek*#}
{#fun mesh_pattern_get_control_point as ^ { withPattern* `Mesh', `Int', cFromEnum `MeshPointNum', alloca- `X' peekDouble*, alloca- `Y' peekDouble* } -> `()' failStatusRaw*#}
{#fun mesh_pattern_get_corner_color_rgba as ^ { withPattern* `Mesh', `Int', cFromEnum `MeshCornerNum', alloca- `Red' peekDouble*, alloca- `Green' peekDouble*, alloca- `Blue' peekDouble*, alloca- `Alpha' peekDouble* } -> `()' failStatusRaw*#}
#endif -- CAIRO_CHECK_VERSION(1,12,0)
