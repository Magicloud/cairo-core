-- λ SKIP MODULE
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
#include "cairo-core.h"

module Graphics.Cairo.Types where

import           Control.Monad
import           Control.Monad.Extra
import           Data.Foldable
import           Data.Maybe
import           Foreign
import           Foreign.C
import qualified GHC.Foreign     as GF (withCString)
import qualified GHC.IO.Encoding as GF (utf8)

{#context lib="cairo" prefix="cairo"#}

type Index = Int
type WidthInt = Int
type HeightInt = Int
type XInt = Int
type YInt = Int
type X = Double
type Y = Double
type Width = Double
type Height = Double
type Radius = Double
type Offset = Double
type XOffset = Double
type YOffset = Double
type XResolution = Double
type YResolution = Double
type XScale = Double
type YScale = Double
type Red = Double
type Green = Double
type Blue = Double
type Alpha = Double
type Angle = Double
type XX = Double
type YY = Double
type XY = Double
type YX = Double
type X0 = Double
type Y0 = Double
data Rectangle a = Rectangle { rectX :: a, rectY :: a
                             , rectWidth :: a
                             , rectHeight :: a }
                 deriving (Show, Eq)
data Circle a = Circle { circleCenter :: Point a
                       , circleRadius :: a }
              deriving (Show, Eq)
data Point a = Point { pointX :: a
                     , pointY :: a }
             deriving (Show, Eq)
data Color a = ColorRGB { colorRed :: a
                        , colorGreen :: a
                        , colorBlue :: a }
             | ColorRGBA { colorARed :: a
                         , colorAGreen :: a
                         , colorABlue :: a
                         , colorAlpha :: a }
             deriving (Show, Eq)
data Matrix a = Matrix { matrixXX :: a, matrixYX :: a
                       , matrixXY :: a, matrixYY :: a
                       , matrixX0 :: a, matrixY0 :: a }
              deriving (Show, Eq)
data Version = Version { verMajor :: Int
                       , verMinor :: Int
                       , verMicro :: Int }
             deriving (Eq)
instance Ord Version where
  compare (Version a1 a2 a3) (Version b1 b2 b3) =
    case compare a1 b1 of
      GT -> GT
      LT -> LT
      EQ -> case compare a2 b2 of
        GT -> GT
        LT -> LT
        EQ -> compare a3 b3

type SolidPattern = Pattern
type SurfacePattern = Pattern
type GradientPattern = Pattern
type LinearGradientPattern = GradientPattern
type RadialGradientPattern = GradientPattern
type Mesh = Pattern

-- | https://www.cairographics.org/manual/cairo-cairo-t.html#cairo-t
{#pointer *t as Context foreign finalizer destroy newtype#}
outContext :: Ptr Context -> IO Context
outContext = fmap Context . newForeignPtr cairo_destroy

-- | https://www.cairographics.org/manual/cairo-cairo-font-face-t.html#cairo-font-face-t
{#pointer *font_face_t as FontFace foreign finalizer font_face_destroy newtype#}
outFontFace :: Ptr FontFace -> IO FontFace
outFontFace = fmap FontFace . newForeignPtr cairo_font_face_destroy
outFontFaceRef :: Ptr FontFace -> IO FontFace
outFontFaceRef = fmap FontFace . (newForeignPtr cairo_font_face_destroy <=< fontFaceReference)

-- | https://www.cairographics.org/manual/cairo-cairo-font-options-t.html#cairo-font-options-t
{#pointer *font_options_t as FontOptions foreign finalizer font_options_destroy newtype#}
outFontOptions :: Ptr FontOptions -> IO FontOptions
outFontOptions = fmap FontOptions . newForeignPtr cairo_font_options_destroy

-- | https://www.cairographics.org/manual/cairo-cairo-pattern-t.html#cairo-pattern-t
{#pointer *pattern_t as Pattern foreign finalizer pattern_destroy newtype#}
outPattern :: Ptr Pattern -> IO Pattern
outPattern = fmap Pattern . newForeignPtr cairo_pattern_destroy
outPatternRef :: Ptr Pattern -> IO Pattern
outPatternRef = fmap Pattern . (newForeignPtr cairo_pattern_destroy <=< patternReference)

-- | https://www.cairographics.org/manual/cairo-cairo-scaled-font-t.html#cairo-scaled-font-t
{#pointer *scaled_font_t as ScaledFont foreign finalizer scaled_font_destroy newtype#}
outScaledFont :: Ptr ScaledFont -> IO ScaledFont
outScaledFont = fmap ScaledFont . newForeignPtr cairo_scaled_font_destroy
outScaledFontRef :: Ptr ScaledFont -> IO ScaledFont
outScaledFontRef = fmap ScaledFont . (newForeignPtr cairo_scaled_font_destroy <=< scaledFontReference)

-- | https://www.cairographics.org/manual/cairo-cairo-surface-t.html#cairo-surface-t
{#pointer *surface_t as Surface foreign finalizer surface_destroy newtype#}
outSurface :: Ptr Surface -> IO Surface
outSurface = fmap Surface . newForeignPtr cairo_surface_destroy
outSurfaceRef :: Ptr Surface -> IO Surface
outSurfaceRef = fmap Surface . (newForeignPtr cairo_surface_destroy <=< surfaceReference)
outSurfacePtrRef :: Ptr (Ptr Surface) -> IO Surface
outSurfacePtrRef pp = peek pp >>= outSurfaceRef

type UnionNum = Int
sizeOfDouble :: Int
sizeOfDouble = 8
sizeOfUnion :: Int
sizeOfUnion = 2 * sizeOfDouble
pathDataSizes :: [(PathDataType, (UnionNum, Int))]
pathDataSizes = [ (PathMoveTo, (2, 2 * sizeOfUnion))
                , (PathLineTo, (2, 2 * sizeOfUnion))
                , (PathCurveTo, (4, 4 * sizeOfUnion))
                , (PathClosePath, (1, sizeOfUnion)) ]
us :: PathDataType -> UnionNum
us t = fst $ fromJust $ lookup t pathDataSizes
-- | https://www.cairographics.org/manual/cairo-Paths.html#cairo-path-data-t
{#pointer *path_data_t as PathDataPtr -> PathData#}
data PathData = PDMoveTo (Point Double)
              | PDLineTo (Point Double)
              | PDCurveTo (Point Double, Point Double, Point Double)
              | PDClosePath
instance Storable PathData where
  sizeOf PDClosePath = snd $ fromJust $ lookup PathClosePath pathDataSizes
  sizeOf (PDCurveTo _) = snd $ fromJust $ lookup PathCurveTo pathDataSizes
  sizeOf _ = snd $ fromJust $ lookup PathMoveTo pathDataSizes
  alignment _ = sizeOfUnion
  peek p = do
    pdType <- (toEnum . fromIntegral) <$> {#get path_data_t->header.type#} p
    let p' = plusPtr p sizeOfUnion
    case pdType of
      PathMoveTo -> do
        CDouble x <- {#get path_data_t->point.x#} p'
        CDouble y <- {#get path_data_t->point.y#} p'
        return $ PDMoveTo $ Point x y
      PathLineTo -> do
        CDouble x <- {#get path_data_t->point.x#} p'
        CDouble y <- {#get path_data_t->point.y#} p'
        return $ PDLineTo $ Point x y
      PathCurveTo -> do
        CDouble x0 <- {#get path_data_t->point.x#} p'
        CDouble y0 <- {#get path_data_t->point.y#} p'
        let p'' = plusPtr p' sizeOfUnion
        CDouble x1 <- {#get path_data_t->point.x#} p''
        CDouble y1 <- {#get path_data_t->point.y#} p''
        let p''' = plusPtr p'' sizeOfUnion
        CDouble x2 <- {#get path_data_t->point.x#} p'''
        CDouble y2 <- {#get path_data_t->point.y#} p'''
        return $ PDCurveTo (Point x0 y0, Point x1 y1, Point x2 y2)
      PathClosePath -> return PDClosePath
  poke p (PDMoveTo (Point x y)) = do
    {#set path_data_t->header.type#} p $ fromIntegral $ fromEnum PathMoveTo
    {#set path_data_t->header.length#} p $ fromIntegral $ us PathMoveTo
    let p' = plusPtr p sizeOfUnion
    {#set path_data_t->point.x#} p' $ CDouble x
    {#set path_data_t->point.y#} p' $ CDouble y
  poke p (PDLineTo (Point x y)) = do
    {#set path_data_t->header.type#} p $ fromIntegral $ fromEnum PathLineTo
    {#set path_data_t->header.length#} p $ fromIntegral $ us PathLineTo
    let p' = plusPtr p sizeOfUnion
    {#set path_data_t->point.x#} p' $ CDouble x
    {#set path_data_t->point.y#} p' $ CDouble y
  poke p (PDCurveTo (Point x0 y0, Point x1 y1, Point x2 y2)) = do
    {#set path_data_t->header.type#} p $ fromIntegral $ fromEnum PathCurveTo
    {#set path_data_t->header.length#} p $ fromIntegral $ us PathCurveTo
    let p' = plusPtr p sizeOfUnion
    {#set path_data_t->point.x#} p' $ CDouble x0
    {#set path_data_t->point.y#} p' $ CDouble y0
    let p'' = plusPtr p' sizeOfUnion
    {#set path_data_t->point.x#} p'' $ CDouble x1
    {#set path_data_t->point.y#} p'' $ CDouble y1
    let p''' = plusPtr p'' sizeOfUnion
    {#set path_data_t->point.x#} p''' $ CDouble x2
    {#set path_data_t->point.y#} p''' $ CDouble y2
  poke p PDClosePath = do
    {#set path_data_t->header.type#} p $ fromIntegral $ fromEnum PathClosePath
    {#set path_data_t->header.length#} p $ fromIntegral $ us PathClosePath

-- | https://www.cairographics.org/manual/cairo-Paths.html#cairo-path-t
{#pointer *path_t as PathPtr -> Path#}
-- outPath :: Ptr Path -> IO Path
-- outPath = fmap Path . newForeignPtr cairo_path_destroy
data Path = Path { pathStatus :: Status
                 , pathData :: [PathData] }
instance Storable Path where
  sizeOf _ = {#sizeof path_t#}
  alignment _ = {#alignof path_t#}
  peek p = do
    s <- (toEnum . fromIntegral) <$> {#get path_t->status#} p
    p' <- {#get path_t->data#} p
    c <- fromIntegral <$> {#get path_t->num_data#} p
    (Path s) <$> unfoldM (\(pdPtr, cnt) -> do
      if (cnt >= c)
        then return Nothing
        else do
          pd <- peek pdPtr
          case pd of
            PDMoveTo _ -> return $ Just (pd, (plusPtr pdPtr $ sizeOf pd, cnt + (us PathMoveTo)))
            PDLineTo _ -> return $ Just (pd, (plusPtr pdPtr $ sizeOf pd, cnt + (us PathLineTo)))
            PDCurveTo _ -> return $ Just (pd, (plusPtr pdPtr $ sizeOf pd, cnt + (us PathCurveTo)))
            PDClosePath -> return $ Just (pd, (plusPtr pdPtr $ sizeOf pd, cnt + (us PathClosePath)))) (p', 0)
  poke p (Path s pds)= do
    {#set path_t->status#} p $ fromIntegral $ fromEnum s
    let len = sum $ map sizeOf pds
    pdPtr <- mallocArray len
    _ <- foldlM (\p' pd -> do
      poke p' pd
      return $ plusPtr p' $ sizeOf pd) pdPtr pds
    {#set path_t->data#} p pdPtr
    {#set path_t->num_data#} p $ fromIntegral len

-- | https://www.cairographics.org/manual/cairo-cairo-scaled-font-t.html#cairo-font-extents-t
{#pointer *font_extents_t as FontExtentsPtr -> FontExtents#}
data FontExtents = FontExtents { feAscent      :: Double
                               , feDescent     :: Double
                               , feHeight      :: Height
                               , feMaxXadvance :: Double
                               , feMaxYadvance :: Double }
instance Storable FontExtents where
  sizeOf _ = {#sizeof font_extents_t#}
  alignment _ = {#alignof font_extents_t#}
  peek p = do
    CDouble ascent  <- {#get font_extents_t->ascent#}        p
    CDouble descent <- {#get font_extents_t->descent#}       p
    CDouble height  <- {#get font_extents_t->height#}        p
    CDouble maxXa   <- {#get font_extents_t->max_x_advance#} p
    CDouble maxYa   <- {#get font_extents_t->max_y_advance#} p
    return $ FontExtents ascent descent height maxXa maxYa
  poke p (FontExtents ascent descent height maxXa maxYa) = do
    {#set font_extents_t->ascent#}        p (CDouble ascent)
    {#set font_extents_t->descent#}       p (CDouble descent)
    {#set font_extents_t->height#}        p (CDouble height)
    {#set font_extents_t->max_x_advance#} p (CDouble maxXa)
    {#set font_extents_t->max_y_advance#} p (CDouble maxYa)

-- | https://www.cairographics.org/manual/cairo-text.html#cairo-glyph-t
{#pointer *glyph_t as GlyphPtr -> Glyph#}
data Glyph = Glyph { glyphIndex :: Index
                   , glyphX :: X
                   , glyphY :: Y }
instance Storable Glyph where
  sizeOf _ = {#sizeof glyph_t#}
  alignment _ = {#alignof glyph_t#}
  peek p = do
    i         <- {#get glyph_t->index#} p
    CDouble x <- {#get glyph_t->x#} p
    CDouble y <- {#get glyph_t->y#} p
    return $ Glyph (fromIntegral i) x y
  poke p (Glyph i x y) = do
    {#set glyph_t->index#} p (fromIntegral i)
    {#set glyph_t->x#}     p (CDouble x)
    {#set glyph_t->y#}     p (CDouble y)

-- | https://www.cairographics.org/manual/cairo-cairo-matrix-t.html#cairo-matrix-t
{#pointer *matrix_t as MatrixPtr -> Matrix Double#}
instance Storable (Matrix Double) where
  sizeOf _ = {#sizeof matrix_t#}
  alignment _ = {#alignof matrix_t#}
  peek p = do
    CDouble xx <- {#get matrix_t->xx#} p
    CDouble yx <- {#get matrix_t->yx#} p
    CDouble xy <- {#get matrix_t->xy#} p
    CDouble yy <- {#get matrix_t->yy#} p
    CDouble x0 <- {#get matrix_t->x0#} p
    CDouble y0 <- {#get matrix_t->y0#} p
    return $ Matrix xx yx xy yy x0 y0
  poke p (Matrix xx yx xy yy x0 y0)= do
    {#set matrix_t->xx#} p (CDouble xx)
    {#set matrix_t->yx#} p (CDouble yx)
    {#set matrix_t->xy#} p (CDouble xy)
    {#set matrix_t->yy#} p (CDouble yy)
    {#set matrix_t->x0#} p (CDouble x0)
    {#set matrix_t->y0#} p (CDouble y0)
instance Functor Matrix where
  fmap f (Matrix xx yx xy yy x0 y0) = Matrix (f xx) (f yx) (f xy) (f yy) (f x0) (f y0)
instance Num (Matrix Double) where
  (*) (Matrix xx yx xy yy x0 y0) (Matrix xx' yx' xy' yy' x0' y0') =
    Matrix (xx * xx' + yx * xy')
           (xx * yx' + yx * yy')
           (xy * xx' + yy * xy')
           (xy * yx' + yy * yy')
           (x0 * xx' + y0 * xy' + x0')
           (x0 * yx' + y0 * yy' + y0')
  (+) (Matrix xx yx xy yy x0 y0) (Matrix xx' yx' xy' yy' x0' y0') =
    Matrix (xx + xx') (yx + yx') (xy + xy') (yy + yy') (x0 + x0') (y0 + y0')
  (-) (Matrix xx yx xy yy x0 y0) (Matrix xx' yx' xy' yy' x0' y0') =
    Matrix (xx - xx') (yx - yx') (xy - xy') (yy - yy') (x0 - x0') (y0 - y0')
  abs = fmap abs
  signum = fmap signum
  fromInteger n = Matrix (fromInteger n) 0 0 (fromInteger n) 0 0

-- | https://www.cairographics.org/manual/cairo-cairo-t.html#cairo-rectangle-t
{#pointer *rectangle_t as RectanglePtr -> Rectangle Double#}
instance Storable (Rectangle Double) where
  sizeOf _ = {#sizeof rectangle_t#}
  alignment _ = {#alignof rectangle_t#}
  peek p = do
    CDouble x      <- {#get rectangle_t->x#}      p
    CDouble y      <- {#get rectangle_t->y#}      p
    CDouble width  <- {#get rectangle_t->width#}  p
    CDouble height <- {#get rectangle_t->height#} p
    return $ Rectangle x y width height
  poke p (Rectangle x y w h) = do
    {#set rectangle_t->x#}      p (CDouble x)
    {#set rectangle_t->y#}      p (CDouble y)
    {#set rectangle_t->width#}  p (CDouble w)
    {#set rectangle_t->height#} p (CDouble h)

-- | https://www.cairographics.org/manual/cairo-cairo-scaled-font-t.html#cairo-text-extents-t
{#pointer *text_extents_t as TextExtentsPtr -> TextExtents#}
data TextExtents = TextExtents { teXbearing :: Double
                               , teYbearing :: Double
                               , teWidth    :: Width
                               , teHeight   :: Height
                               , teXadvance :: Double
                               , teYadvance :: Double }
instance Storable TextExtents where
  sizeOf _ = {#sizeof text_extents_t#}
  alignment _ = {#alignof text_extents_t#}
  peek p = do
    CDouble xb <- {#get text_extents_t->x_bearing#} p
    CDouble yb <- {#get text_extents_t->y_bearing#} p
    CDouble w  <- {#get text_extents_t->width#}     p
    CDouble h  <- {#get text_extents_t->height#}    p
    CDouble xa <- {#get text_extents_t->x_advance#} p
    CDouble ya <- {#get text_extents_t->y_advance#} p
    return $ TextExtents xb yb w h xa ya
  poke p (TextExtents xb yb w h xa ya) = do
    {#set text_extents_t->x_bearing#} p (CDouble xb)
    {#set text_extents_t->y_bearing#} p (CDouble yb)
    {#set text_extents_t->width#}     p (CDouble w)
    {#set text_extents_t->height#}    p (CDouble h)
    {#set text_extents_t->x_advance#} p (CDouble xa)
    {#set text_extents_t->y_advance#} p (CDouble ya)

#if CAIRO_CHECK_VERSION(1,4,0)
-- | https://www.cairographics.org/manual/cairo-cairo-t.html#cairo-rectangle-list-t
data RectangleList a = RectangleList { rlStatus     :: Status
                                     , rlRectangles :: [Rectangle a] }
                     deriving (Show, Eq)
{#pointer *rectangle_list_t as RectangleListPtr -> RectangleList Double#}
instance Storable (RectangleList Double) where
  sizeOf _ = {#sizeof rectangle_list_t#}
  alignment _ = {#alignof rectangle_list_t#}
  peek p = do
    s <- {#get rectangle_list_t->status#}         p
    r <- {#get rectangle_list_t->rectangles#}     p
    n <- {#get rectangle_list_t->num_rectangles#} p
    peekArray (fromIntegral n) r >>=
      return . RectangleList (toEnum $ fromIntegral s)
  poke p (RectangleList s rs)=
    withArrayLen rs $ \len ptr' -> do
      {#set rectangle_list_t->status#}         p (fromIntegral $ fromEnum s)
      {#set rectangle_list_t->rectangles#}     p ptr'
      {#set rectangle_list_t->num_rectangles#} p (fromIntegral len)
#endif -- CAIRO_CHECK_VERSION(1,4,0)

#if CAIRO_CHECK_VERSION(1,8,0)
-- | https://www.cairographics.org/manual/cairo-text.html#cairo-text-cluster-t
data TextCluster = TextCluster { textClusterNumBytes  :: Int
                               , textClusterNumGlyphs :: Int }
                 deriving (Show, Eq)
{#pointer *text_cluster_t as TextClusterPtr -> TextCluster#}
instance Storable TextCluster where
  sizeOf _ = {#sizeof text_cluster_t#}
  alignment _ = {#alignof text_cluster_t#}
  peek p = do
    b <- {#get text_cluster_t->num_bytes#}  p
    g <- {#get text_cluster_t->num_glyphs#} p
    return $ TextCluster (fromIntegral b) (fromIntegral g)
  poke p (TextCluster b g)= do
    {#set text_cluster_t->num_bytes#}  p (fromIntegral b)
    {#set text_cluster_t->num_glyphs#} p (fromIntegral g)
#endif -- CAIRO_CHECK_VERSION(1,8,0)

#if CAIRO_CHECK_VERSION(1,10,0)
-- | https://www.cairographics.org/manual/cairo-cairo-device-t.html#cairo-device-t
{#pointer *device_t as Device foreign finalizer device_destroy newtype#}
outDevice :: Ptr Device -> IO Device
outDevice = fmap Device . newForeignPtr cairo_device_destroy
outMaybeDevice :: Ptr Device -> IO (Maybe Device)
outMaybeDevice p = if p == nullPtr
  then return Nothing
  else Just <$> outDevice p

-- | https://www.cairographics.org/manual/cairo-Regions.html#cairo-region-t
{#pointer *region_t as Region foreign finalizer region_destroy newtype#}
outRegion :: Ptr Region -> IO Region
outRegion = fmap Region . newForeignPtr cairo_region_destroy

-- | https://www.cairographics.org/manual/cairo-Types.html#cairo-rectangle-int-t
{#pointer *rectangle_int_t as RectangleIntPtr -> Rectangle Int#}
instance Storable (Rectangle Int) where
  sizeOf _ = {#sizeof rectangle_int_t#}
  alignment _ = {#alignof rectangle_int_t#}
  peek p = do
    x      <- {#get rectangle_int_t->x#}      p
    y      <- {#get rectangle_int_t->y#}      p
    width  <- {#get rectangle_int_t->width#}  p
    height <- {#get rectangle_int_t->height#} p
    return $ Rectangle (fromIntegral x)
                       (fromIntegral y)
                       (fromIntegral width)
                       (fromIntegral height)
  poke p (Rectangle x y w h) = do
    {#set rectangle_int_t->x#}      p (fromIntegral x)
    {#set rectangle_int_t->y#}      p (fromIntegral y)
    {#set rectangle_int_t->width#}  p (fromIntegral w)
    {#set rectangle_int_t->height#} p (fromIntegral h)
#endif -- CAIRO_CHECK_VERSION(1,10,0)

-- | https://www.cairographics.org/manual/cairo-cairo-t.html#cairo-antialias-t
{#enum antialias_t as Antialias {underscoreToCase} deriving(Eq, Show)#}
-- | https://www.cairographics.org/manual/cairo-cairo-surface-t.html#cairo-content-t
{#enum content_t as Content {underscoreToCase} deriving(Eq, Show)#}
-- | https://www.cairographics.org/manual/cairo-cairo-pattern-t.html#cairo-extend-t
{#enum extend_t as Extend {underscoreToCase} deriving(Eq, Show)#}
-- | https://www.cairographics.org/manual/cairo-cairo-t.html#cairo-fill-rule-t
{#enum fill_rule_t as FillRule {underscoreToCase} deriving(Eq, Show)#}
-- | https://www.cairographics.org/manual/cairo-cairo-pattern-t.html#cairo-filter-t
{#enum filter_t as Filter {underscoreToCase} deriving(Eq, Show)#}
-- | https://www.cairographics.org/manual/cairo-text.html#cairo-font-slant-t
{#enum font_slant_t as FontSlant {underscoreToCase} deriving(Eq, Show)#}
-- | https://www.cairographics.org/manual/cairo-text.html#cairo-font-weight-t
{#enum font_weight_t as FontWeight {underscoreToCase} deriving(Eq, Show)#}
-- | https://www.cairographics.org/manual/cairo-Image-Surfaces.html#cairo-format-t
{#enum format_t as Format {underscoreToCase} deriving(Eq, Show)#}
-- | https://www.cairographics.org/manual/cairo-cairo-font-options-t.html#cairo-hint-metrics-t
{#enum hint_metrics_t as HintMetrics {underscoreToCase} deriving(Eq, Show)#}
-- | https://www.cairographics.org/manual/cairo-cairo-font-options-t.html#cairo-hint-style-t
{#enum hint_style_t as HintStyle {underscoreToCase}#}
-- | https://www.cairographics.org/manual/cairo-cairo-t.html#cairo-line-cap-t
{#enum line_cap_t as LineCap {underscoreToCase} deriving(Eq, Show)#}
-- | https://www.cairographics.org/manual/cairo-cairo-t.html#cairo-line-join-t
{#enum line_join_t as LineJoin {underscoreToCase} deriving(Eq, Show)#}
-- | https://www.cairographics.org/manual/cairo-cairo-t.html#cairo-operator-t
{#enum operator_t as Operator {underscoreToCase} deriving(Eq, Show)#}
-- | https://www.cairographics.org/manual/cairo-Paths.html#cairo-path-data-type-t
{#enum path_data_type_t as PathDataType {underscoreToCase} deriving(Eq, Show)#}
-- | https://www.cairographics.org/manual/cairo-cairo-font-options-t.html#cairo-subpixel-order-t
{#enum subpixel_order_t as SubpixelOrder {underscoreToCase} deriving(Eq, Show)#}
-- | https://www.cairographics.org/manual/cairo-Error-handling.html#cairo-status-t
{#enum status_t as Status {underscoreToCase} deriving(Eq, Show)#}

#if CAIRO_CHECK_VERSION(1,2,0)
-- | https://www.cairographics.org/manual/cairo-cairo-font-face-t.html#cairo-font-type-t
{#enum font_type_t as FontType {underscoreToCase} deriving(Eq, Show)#}
-- | https://www.cairographics.org/manual/cairo-cairo-pattern-t.html#cairo-pattern-type-t
{#enum pattern_type_t as PatternType {underscoreToCase} deriving(Eq, Show)#}
-- | https://cairographics.org/manual/cairo-cairo-surface-t.html#cairo-surface-type-t
{#enum surface_type_t as SurfaceType {underscoreToCase} deriving(Eq, Show)#}
#endif -- CAIRO_CHECK_VERSION(1,2,0)

#if CAIRO_CHECK_VERSION(1,8,0)
-- | https://cairographics.org/manual/cairo-text.html#cairo-text-cluster-flags-t
{#enum text_cluster_flags_t as TextClusterFlags {underscoreToCase} deriving(Eq, Show)#}
#endif -- CAIRO_CHECK_VERSION(1,8,0)

#if CAIRO_CHECK_VERSION(1,10,0)
-- | https://www.cairographics.org/manual/cairo-cairo-device-t.html#cairo-device-type-t
{#enum device_type_t as DeviceType {underscoreToCase} deriving(Eq, Show)#}
-- | https://www.cairographics.org/manual/cairo-Regions.html#cairo-region-overlap-t
{#enum region_overlap_t as RegionOverlap {underscoreToCase} deriving(Eq, Show)#}
#endif -- CAIRO_CHECK_VERSION(1,10,0)

#if CAIRO_CHECK_VERSION(1,12,0)
data MeshPointNum = MP0
                  | MP1
                  | MP2
                  | MP3
                  deriving (Show, Eq, Enum)
data MeshCornerNum = MC0
                   | MC1
                   | MC2
                   | MC3
                   deriving (Show, Eq, Enum)
#endif -- CAIRO_CHECK_VERSION(1,12,0)

-- Marshallers

cFromEnum :: (Enum a, Integral b) => a -> b
cFromEnum = fromIntegral . fromEnum

peekEnum :: (Storable a, Integral a, Enum b) => Ptr a -> IO b
peekEnum = fmap toEnum . peekInt

peekInt :: (Storable a, Integral a) => Ptr a -> IO Int
peekInt = fmap fromIntegral . peek

peekDouble :: Ptr CDouble -> IO Double
peekDouble = fmap (\(CDouble x) -> x) . peek

withDouble :: Double -> (Ptr CDouble -> IO b) -> IO b
withDouble = with . CDouble

withUTF8String :: String -> (CString -> IO a) -> IO a
withUTF8String = GF.withCString GF.utf8

withMaybe :: Storable a => Maybe a -> (Ptr a -> IO b) -> IO b
withMaybe m f = case m of
  Nothing -> f nullPtr
  Just p -> with p f

-- Referencer

{#fun reference as ^ { id `Ptr Context' } -> `Ptr Context' return*#}
{#fun pattern_reference as ^ { id `Ptr Pattern' } -> `Ptr Pattern' return*#}
{#fun region_reference as ^ { id `Ptr Region' } -> `Ptr Region' return*#}
{#fun font_face_reference as ^ { id `Ptr FontFace' } -> `Ptr FontFace' return*#}
{#fun scaled_font_reference as ^ { id `Ptr ScaledFont' } -> `Ptr ScaledFont' return*#}
{#fun device_reference as ^ { id `Ptr Device' } -> `Ptr Device' return*#}
{#fun surface_reference as ^ { id `Ptr Surface' } -> `Ptr Surface' return*#}
