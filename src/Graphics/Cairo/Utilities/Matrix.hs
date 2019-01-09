{-# LANGUAGE FlexibleInstances #-}
module Graphics.Cairo.Utilities.Matrix where

import Graphics.Cairo.Types

initIdentity :: Matrix Double
initIdentity = Matrix 1 0 0 1 0 0

initTranslate :: X0 -> Y0 -> Matrix Double
initTranslate = Matrix 1 0 0 1

initScale :: XX -> YY -> Matrix Double
initScale sx sy = Matrix sx 0 0 sy 0 0

initRotate :: Radius -> Matrix Double
initRotate radians = Matrix c s (-s) c 0 0
  where s = sin radians
        c = cos radians

translate :: X0 -> Y0 -> Matrix Double -> Matrix Double
translate tx ty m = m * initTranslate tx ty

scale :: XX -> YY -> Matrix Double -> Matrix Double
scale sx sy m = m * initScale sx sy

rotate :: Radius -> Matrix Double -> Matrix Double
rotate radians m = m * initRotate radians

invert :: Matrix Double -> Maybe (Matrix Double)
invert (Matrix 0 0 0 _ _ _) = Nothing
invert (Matrix _ 0 0 0 _ _) = Nothing
invert (Matrix xx 0 0 yy x0 y0) =
  let (xx', x0') = if xx /= 1
                     then (recip xx, -x0 * recip xx)
                     else (xx, -x0)
      (yy', y0') = if yy /= 1
                     then (recip yy, -y0 * recip yy)
                     else (yy, -y0)
  in Just $ Matrix xx' 0 0 yy' x0' y0'
invert matrix
  | determinant matrix == 0 = Nothing
  | otherwise = Just $ scalarMultiply (adjoint matrix) (recip $ determinant matrix)
  where
    determinant (Matrix xx' yx' xy' yy' _ _) = xx' * yy' - yx' * xy'
    adjoint (Matrix xx' yx' xy' yy' x0' y0') = Matrix yy' (-yx') (-xy') xx' (xy' * y0' - yy' * x0') (yx' * x0' - xx' * y0')
    scalarMultiply matrix' scalar = fmap (scalar *) matrix'

transformDistance :: Matrix Double -> (X, Y) -> (X, Y)
transformDistance (Matrix xx yx xy yy _ _) (x, y) =
  (xx * x + xy * y, yx * x + yy * y)

transformPoint :: Matrix Double -> (X, Y) -> (X, Y)
transformPoint matrix@(Matrix _ _ _ _ x0 y0) pd =
  let (x, y) = transformDistance matrix pd
  in (x + x0, y + y0)
