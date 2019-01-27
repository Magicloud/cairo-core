{-# LANGUAGE FlexibleInstances #-}
{-|
Description : λ https://www.cairographics.org/manual/cairo-cairo-matrix-t.html //div[@class="refnamediv"]/table/tr/td/p/text()

λ https://www.cairographics.org/manual/cairo-cairo-matrix-t.html#cairo-cairo-matrix-t.description
-}
module Graphics.Cairo.Utilities.Matrix where

import Graphics.Cairo.Types

-- λ https://www.cairographics.org/manual/cairo-cairo-matrix-t.html#cairo-matrix-init
matrixInit :: a -> a -> a -> a -> a -> a -> Matrix a
matrixInit = Matrix

-- λ https://www.cairographics.org/manual/cairo-cairo-matrix-t.html#cairo-matrix-init-identity
matrixInitIdentity :: Matrix Double
matrixInitIdentity = Matrix 1 0 0 1 0 0

-- λ https://www.cairographics.org/manual/cairo-cairo-matrix-t.html#cairo-matrix-init-translate
matrixInitTranslate :: X0 -> Y0 -> Matrix Double
matrixInitTranslate = Matrix 1 0 0 1

-- λ https://www.cairographics.org/manual/cairo-cairo-matrix-t.html#cairo-matrix-init-scale
matrixInitScale :: XX -> YY -> Matrix Double
matrixInitScale sx sy = Matrix sx 0 0 sy 0 0

-- λ https://www.cairographics.org/manual/cairo-cairo-matrix-t.html#cairo-matrix-init-rotate
matrixInitRotate :: Radius -> Matrix Double
matrixInitRotate radians = Matrix c s (-s) c 0 0
  where s = sin radians
        c = cos radians

-- λ https://www.cairographics.org/manual/cairo-cairo-matrix-t.html#cairo-matrix-translate
matrixTranslate :: X0 -> Y0 -> Matrix Double -> Matrix Double
matrixTranslate tx ty m = m * matrixInitTranslate tx ty

-- λ https://www.cairographics.org/manual/cairo-cairo-matrix-t.html#cairo-matrix-scale
matrixScale :: XX -> YY -> Matrix Double -> Matrix Double
matrixScale sx sy m = m * matrixInitScale sx sy

-- λ https://www.cairographics.org/manual/cairo-cairo-matrix-t.html#cairo-matrix-rotate
matrixRotate :: Radius -> Matrix Double -> Matrix Double
matrixRotate radians m = m * matrixInitRotate radians

-- λ https://www.cairographics.org/manual/cairo-cairo-matrix-t.html#cairo-matrix-invert
matrixInvert :: Matrix Double -> Maybe (Matrix Double)
matrixInvert (Matrix 0 0 0 _ _ _) = Nothing
matrixInvert (Matrix _ 0 0 0 _ _) = Nothing
matrixInvert (Matrix xx 0 0 yy x0 y0) =
  let (xx', x0') = if xx /= 1
                     then (recip xx, -x0 * recip xx)
                     else (xx, -x0)
      (yy', y0') = if yy /= 1
                     then (recip yy, -y0 * recip yy)
                     else (yy, -y0)
  in Just $ Matrix xx' 0 0 yy' x0' y0'
matrixInvert matrix
  | determinant matrix == 0 = Nothing
  | otherwise = Just $ scalarMultiply (adjoint matrix) (recip $ determinant matrix)
  where
    determinant (Matrix xx' yx' xy' yy' _ _) = xx' * yy' - yx' * xy'
    adjoint (Matrix xx' yx' xy' yy' x0' y0') = Matrix yy' (-yx') (-xy') xx' (xy' * y0' - yy' * x0') (yx' * x0' - xx' * y0')
    scalarMultiply matrix' scalar = fmap (scalar *) matrix'

-- λ https://www.cairographics.org/manual/cairo-cairo-matrix-t.html#cairo-matrix-multiply
matrixMultiply :: Matrix Double -> Matrix Double -> Matrix Double
matrixMultiply = (*)

-- λ https://www.cairographics.org/manual/cairo-cairo-matrix-t.html#cairo-matrix-transform-distance
matrixTransformDistance :: Matrix Double -> (X, Y) -> (X, Y)
matrixTransformDistance (Matrix xx yx xy yy _ _) (x, y) =
  (xx * x + xy * y, yx * x + yy * y)

-- λ https://www.cairographics.org/manual/cairo-cairo-matrix-t.html#cairo-matrix-transform-point
matrixTransformPoint :: Matrix Double -> (X, Y) -> (X, Y)
matrixTransformPoint matrix@(Matrix _ _ _ _ x0 y0) pd =
  let (x, y) = matrixTransformDistance matrix pd
  in (x + x0, y + y0)
