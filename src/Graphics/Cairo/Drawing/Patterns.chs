#include "cairo-core.h"
{-|
Description : λ https://www.cairographics.org/manual/cairo-cairo-pattern-t.html //div[@class="refnamediv"]/table/tr/td/p/text()

λ https://www.cairographics.org/manual/cairo-cairo-pattern-t.html#cairo-cairo-pattern-t.description
-}
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

-- λ https://www.cairographics.org/manual/cairo-cairo-pattern-t.html#cairo-pattern-add-color-stop-rgb
{#fun pattern_add_color_stop_rgb as ^ { withPattern* `GradientPattern', CDouble `Offset', CDouble `Red', CDouble `Green', CDouble `Blue' } -> `()'#}
-- λ https://www.cairographics.org/manual/cairo-cairo-pattern-t.html#cairo-pattern-add-color-stop-rgba
{#fun pattern_add_color_stop_rgba as ^ { withPattern* `GradientPattern', CDouble `Offset', CDouble `Red', CDouble `Green', CDouble `Blue', CDouble `Alpha' } -> `()'#}
-- λ https://www.cairographics.org/manual/cairo-cairo-pattern-t.html#cairo-pattern-create-rgb
{#fun pattern_create_rgb as ^ { CDouble `Red', CDouble `Green', CDouble `Blue' } -> `SolidPattern' outPattern*#}
-- λ https://www.cairographics.org/manual/cairo-cairo-pattern-t.html#cairo-pattern-create-rgba
{#fun pattern_create_rgba as ^ { CDouble `Red', CDouble `Green', CDouble `Blue', CDouble `Alpha' } -> `SolidPattern' outPattern*#}
-- λ https://www.cairographics.org/manual/cairo-cairo-pattern-t.html#cairo-pattern-create-for-surface
{#fun pattern_create_for_surface as ^ { `Surface' } -> `SurfacePattern' outPattern*#}
-- λ https://www.cairographics.org/manual/cairo-cairo-pattern-t.html#cairo-pattern-create-linear
{#fun pattern_create_linear as ^ { CDouble `X', CDouble `Y', CDouble `X', CDouble `Y' } -> `LinearGradientPattern' outPattern*#}
-- λ https://www.cairographics.org/manual/cairo-cairo-pattern-t.html#cairo-pattern-create-radial
{#fun pattern_create_radial as ^ { CDouble `X', CDouble `Y', CDouble `Radius', CDouble `X', CDouble `Y', CDouble `Radius' } -> `RadialGradientPattern' outPattern*#}
-- λ https://www.cairographics.org/manual/cairo-cairo-pattern-t.html#cairo-pattern-status
{#fun pattern_status as ^ { `Pattern' } -> `Status'#}
-- λ https://www.cairographics.org/manual/cairo-cairo-pattern-t.html#cairo-pattern-set-extend
{#fun pattern_set_extend as ^ { withPattern* `SurfacePattern', `Extend' } -> `()'#}
-- λ https://www.cairographics.org/manual/cairo-cairo-pattern-t.html#cairo-pattern-get-extend
{#fun pattern_get_extend as ^ { withPattern* `SurfacePattern' } -> `Extend'#}
-- λ https://www.cairographics.org/manual/cairo-cairo-pattern-t.html#cairo-pattern-set-filter
{#fun pattern_set_filter as ^ { withPattern* `SurfacePattern', `Filter' } -> `()'#}
-- λ https://www.cairographics.org/manual/cairo-cairo-pattern-t.html#cairo-pattern-get-filter
{#fun pattern_get_filter as ^ { withPattern* `SurfacePattern' } -> `Filter'#}
-- λ https://www.cairographics.org/manual/cairo-cairo-pattern-t.html#cairo-pattern-set-matrix
{#fun pattern_set_matrix as ^ { `Pattern', with* `Matrix Double' } -> `()'#}
-- λ https://www.cairographics.org/manual/cairo-cairo-pattern-t.html#cairo-pattern-get-matrix
{#fun pattern_get_matrix as ^ { `Pattern', alloca- `Matrix Double' peek*} -> `()'#}

-- λ https://www.cairographics.org/manual/cairo-cairo-pattern-t.html#cairo-pattern-get-type
{#fun pattern_get_type as ^ { `Pattern' } -> `PatternType'#} -- λ require CAIRO_CHECK_VERSION(1,2,0)

-- λ https://www.cairographics.org/manual/cairo-cairo-pattern-t.html#cairo-pattern-get-color-stop-count
{#fun pattern_get_color_stop_count as ^ { withPattern* `GradientPattern', alloca- `Int' peekInt* } -> `()' failStatusRaw*#} -- λ require CAIRO_CHECK_VERSION(1,4,0)
-- λ https://www.cairographics.org/manual/cairo-cairo-pattern-t.html#cairo-pattern-get-color-stop-rgba
{#fun pattern_get_color_stop_rgba as ^ { withPattern* `GradientPattern', fromIntegral `Index', alloca- `Double' peekDouble*, alloca- `Red' peekDouble*, alloca- `Green' peekDouble*, alloca- `Blue' peekDouble*, alloca- `Alpha' peekDouble* } -> `()' failStatusRaw*#} -- λ require CAIRO_CHECK_VERSION(1,4,0)
-- λ https://www.cairographics.org/manual/cairo-cairo-pattern-t.html#cairo-pattern-get-rgba
{#fun pattern_get_rgba as ^ { withPattern* `SolidPattern', alloca- `Red' peekDouble*, alloca- `Green' peekDouble*, alloca- `Blue' peekDouble*, alloca- `Alpha' peekDouble* } -> `()' failStatusRaw*#} -- λ require CAIRO_CHECK_VERSION(1,4,0)
-- λ https://www.cairographics.org/manual/cairo-cairo-pattern-t.html#cairo-pattern-get-surface
{#fun pattern_get_surface as ^ { withPattern* `SurfacePattern', alloca- `Surface' outSurfacePtrRef* } -> `()' failStatusRaw*#} -- λ require CAIRO_CHECK_VERSION(1,4,0)
-- λ https://www.cairographics.org/manual/cairo-cairo-pattern-t.html#cairo-pattern-get-linear-points
{#fun pattern_get_linear_points as ^ { withPattern* `LinearGradientPattern', alloca- `X' peekDouble*, alloca- `Y' peekDouble*, alloca- `X' peekDouble*, alloca- `Y' peekDouble* } -> `()' failStatusRaw*#} -- λ require CAIRO_CHECK_VERSION(1,4,0)
-- λ https://www.cairographics.org/manual/cairo-cairo-pattern-t.html#cairo-pattern-get-radial-circles
{#fun pattern_get_radial_circles as ^ { withPattern* `RadialGradientPattern', alloca- `X' peekDouble*, alloca- `Y' peekDouble*, alloca- `Radius' peekDouble*, alloca- `X' peekDouble*, alloca- `Y' peekDouble*, alloca- `Radius' peekDouble* } -> `()' failStatusRaw*#} -- λ require CAIRO_CHECK_VERSION(1,4,0)
-- λ https://www.cairographics.org/manual/cairo-cairo-pattern-t.html#cairo-pattern-get-reference-count
{#fun pattern_get_reference_count as ^ { `Pattern' } -> `Int'#} -- λ require CAIRO_CHECK_VERSION(1,4,0)

{- |
λ https://www.cairographics.org/manual/cairo-cairo-pattern-t.html#cairo-mesh-pattern-begin-patch
λ https://www.cairographics.org/manual/cairo-cairo-pattern-t.html#cairo-mesh-pattern-end-patch
-}
#if CAIRO_CHECK_VERSION(1,12,0)
meshPatternPatch :: Mesh -> (Mesh -> IO c) -> IO c
meshPatternPatch mesh f =
  bracket (meshPatternBeginPatch mesh) (const $ do
    meshPatternEndPatch mesh
    patternStatus mesh >>= failStatus) (const $ f mesh)
  where
    {#fun mesh_pattern_begin_patch as ^ { withPattern* `Mesh' } -> `()'#}
    {#fun mesh_pattern_end_patch as ^ { withPattern* `Mesh' } -> `()'#}
#else
{-# WARNING meshPatternPatch "CAIRO_CHECK_VERSION(1,12,0) unmet" #-}
meshPatternPatch = undefined
#endif

-- λ https://www.cairographics.org/manual/cairo-cairo-pattern-t.html#cairo-pattern-create-mesh
{#fun pattern_create_mesh as ^ {} -> `Mesh' outPattern*#} -- λ require CAIRO_CHECK_VERSION(1,12,0)
-- λ https://www.cairographics.org/manual/cairo-cairo-pattern-t.html#cairo-mesh-pattern-move-to
{#fun mesh_pattern_move_to as ^ { withPattern* `Mesh', CDouble `X', CDouble `Y' } -> `()'#} -- λ require CAIRO_CHECK_VERSION(1,12,0)
-- λ https://www.cairographics.org/manual/cairo-cairo-pattern-t.html#cairo-mesh-pattern-line-to
{#fun mesh_pattern_line_to as ^ { withPattern* `Mesh', CDouble `X', CDouble `Y' } -> `()'#} -- λ require CAIRO_CHECK_VERSION(1,12,0)
-- λ https://www.cairographics.org/manual/cairo-cairo-pattern-t.html#cairo-mesh-pattern-curve-to
{#fun mesh_pattern_curve_to as ^ { withPattern* `Mesh', CDouble `X', CDouble `Y', CDouble `X', CDouble `Y', CDouble `X', CDouble `Y' } -> `()'#} -- λ require CAIRO_CHECK_VERSION(1,12,0)
-- λ https://www.cairographics.org/manual/cairo-cairo-pattern-t.html#cairo-mesh-pattern-set-control-point
{#fun mesh_pattern_set_control_point as ^ { withPattern* `Mesh', cFromEnum `MeshPointNum', CDouble `X', CDouble `Y' } -> `()'#} -- λ require CAIRO_CHECK_VERSION(1,12,0)
-- λ https://www.cairographics.org/manual/cairo-cairo-pattern-t.html#cairo-mesh-pattern-set-corner-color-rgb
{#fun mesh_pattern_set_corner_color_rgb as ^ { withPattern* `Mesh', cFromEnum `MeshCornerNum', CDouble `Red', CDouble `Green', CDouble `Blue' } -> `()'#} -- λ require CAIRO_CHECK_VERSION(1,12,0)
-- λ https://www.cairographics.org/manual/cairo-cairo-pattern-t.html#cairo-mesh-pattern-set-corner-color-rgba
{#fun mesh_pattern_set_corner_color_rgba as ^ { withPattern* `Mesh', cFromEnum `MeshCornerNum', CDouble `Red', CDouble `Green', CDouble `Blue', CDouble `Alpha' } -> `()'#} -- λ require CAIRO_CHECK_VERSION(1,12,0)
-- λ https://www.cairographics.org/manual/cairo-cairo-pattern-t.html#cairo-mesh-pattern-get-patch-count
{#fun mesh_pattern_get_patch_count as ^ { withPattern* `Mesh', alloca- `Int' peekInt*} -> `()' failStatusRaw*#} -- λ require CAIRO_CHECK_VERSION(1,12,0)
-- λ https://www.cairographics.org/manual/cairo-cairo-pattern-t.html#cairo-mesh-pattern-get-path
{#fun mesh_pattern_get_path as ^ { withPattern* `Mesh', `Int' } -> `Path' peek*#} -- λ require CAIRO_CHECK_VERSION(1,12,0)
-- λ https://www.cairographics.org/manual/cairo-cairo-pattern-t.html#cairo-mesh-pattern-get-control-point
{#fun mesh_pattern_get_control_point as ^ { withPattern* `Mesh', `Int', cFromEnum `MeshPointNum', alloca- `X' peekDouble*, alloca- `Y' peekDouble* } -> `()' failStatusRaw*#} -- λ require CAIRO_CHECK_VERSION(1,12,0)
-- λ https://www.cairographics.org/manual/cairo-cairo-pattern-t.html#cairo-mesh-pattern-get-corner-color-rgba
{#fun mesh_pattern_get_corner_color_rgba as ^ { withPattern* `Mesh', `Int', cFromEnum `MeshCornerNum', alloca- `Red' peekDouble*, alloca- `Green' peekDouble*, alloca- `Blue' peekDouble*, alloca- `Alpha' peekDouble* } -> `()' failStatusRaw*#} -- λ require CAIRO_CHECK_VERSION(1,12,0)
