{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import           Codec.Picture.Repa
import qualified Data.Array.Repa.Stencil as R
import           Data.Array.Repa.Stencil.Dim2
import qualified Data.Array.Repa as R
import           Data.Array.Repa (Z(..), Array, (:.)(..), traverse, DIM1, DIM2, DIM3, D, delay, computeS)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as V hiding (drop,zip)
import           Control.Monad (foldM)
import           Data.Word
import           System.Environment (getArgs)
import           System.FilePath ((<.>))
import qualified Data.ByteString.Lazy as B
import           Control.Monad.ST
import           Control.Monad

import Debug.Trace

main :: IO ()
main = getArgs >>= run . head

run :: FilePath -> IO ()
run fp = do
    eimg <- readImage fp
    img  <- either (error "Unable to load the image") return eimg
    let resultImages = map imgToImage (morph img)
    zipWithM_ (\n i -> B.writeFile (fp <.> ("out" ++ show n) <.> "png" ) i) [1..] (map imageToPng resultImages)

red,green,blue,alpha :: Int
red   = 0
green = 1
blue  = 2
alpha = 3

type Morph dim = Array D dim Word8 -> Array D dim Word8
type MorphU dim = Array R.U dim Word8 -> Array R.U dim Word8
type Convert dimA dimB = Array D dimA Word8 -> Array D dimB Word8

imgArray :: Img RGBA -> Array D DIM3 Word8
imgArray = delay . imgData

morph :: Img RGBA -> [Img RGBA]
morph orig =
    let img = delay (imgData orig)
        force = delay . R.computeUnboxedS
        g = force $ grayscale img
        o = force $ otsu g
        -- l = force $ maskBy3 (\lk _ idx -> letterColor lk idx) 0 img o
        t = topHat o
        s = force $ scw defaultScwMean (18, 6) (48, 16) g
        d = dilation7_7 (force $ otsu (invert s))
        a = force $ imgAnd t d
        finish = Img . R.computeS . grayscaleToRGBA
    in map finish [g, o, t, s, d, a] -- R.computeS (grayscaleToRGBA a)

imgAnd :: Array D DIM2 Word8 -> Array D DIM2 Word8 -> Array D DIM2 Word8
imgAnd x y = R.traverse2 x y const (\l0 l1 idx -> if l0 idx /= 0 && l1 idx /= 0 then 255 else 0)

subGreen :: (DIM3 -> Word8) -> (DIM2 -> Word8) -> DIM2 -> Word8
subGreen lk0 lk1 sh@(Z :. _y :. _x)
   | lk0 (sh :. green) > lk0 (sh :. blue) = 0
   | otherwise                            = lk1 sh

maxGreen :: (DIM3 -> Word8) -> (DIM2 -> Word8) -> DIM2 -> Word8
maxGreen lk0 lk1 sh@(Z :. _y :. _x)
   | lk0 (sh :. green) > lk0 (sh :. blue) = 255
   | otherwise                            = lk1 sh

subRed :: (DIM3 -> Word8) -> (DIM2 -> Word8) -> DIM2 -> Word8
subRed lk0 lk1 sh@(Z :. _y :. _x)
   | lk0 (sh :. red) > lk0 (sh :. blue) = 0
   | otherwise                            = lk1 sh

maxRed :: (DIM3 -> Word8) -> (DIM2 -> Word8) -> DIM2 -> Word8
maxRed lk0 lk1 sh@(Z :. _y :. _x)
   | lk0 (sh :. red) > lk0 (sh :. blue) = 255
   | otherwise                          = lk1 sh

letterColor :: (DIM3 -> Word8) -> DIM2 -> Bool
letterColor lk0 sh =
   (g <= 0x40 &&
     r <= 0x28 &&
     b >= 0x32) ||
   (g <= 0x75 &&
     r <= 0x59 &&
     b >= 0x51)
     where
       g = lk0 (sh :. green)
       r = lk0 (sh :. red)
       b = lk0 (sh :. blue)
{-# INLINE letterColor #-}

maskBy :: ((DIM3 -> Word8)->  (DIM2 -> Word8) -> DIM2 -> Bool) -> Word8 -> Array D DIM3 Word8 -> Morph DIM2
maskBy f def orig x = R.traverse2 orig x (\_ s -> s) (\l0 l1 idx -> if f l0 l1 idx then l1 idx else def)

maskBy3 :: R.Shape dim => ((DIM3 -> Word8)-> (dim -> Word8) -> dim -> Bool) -> Word8 -> Array D DIM3 Word8 -> Morph dim
maskBy3 f def orig x = R.traverse2 orig x (\_ s -> s) (\l0 l1 idx -> if f l0 l1 idx then l1 idx else def)
{-# INLINE maskBy3 #-}

invert :: Morph DIM2
invert = R.map (\x -> if x == 0 then 255 else 0)

defaultScwMean :: Double
defaultScwMean = 0.95

scw :: Double -> (Int,Int) -> (Int,Int) -> Morph DIM2
scw scwMean small large orig =
    let mkBox sz = R.computeUnboxedS $ (uncurry scwGeneric) sz orig
        boxSmall = mkBox small
        boxLarge = mkBox large
        lg = fromIntegral $ fst large * snd large
        sm = fromIntegral $ fst small * snd small
    in R.traverse3 orig boxSmall boxLarge
                   (\s _ _ -> s)
                   (\l0 l1 l2 idx -> let r = (l2 idx / lg) / (l1 idx / sm) in if r < scwMean then 0 else l0 idx)

scwGeneric :: Int -> Int -> Array D DIM2 Word8 -> Array D DIM2 Double
scwGeneric xDelta yDelta = \orig ->
    let img = R.computeUnboxedS (R.map fromIntegral orig :: Array R.D DIM2 Double)
        Z :. width :. height = R.extent orig
        (xh,yh) = (xDelta `div` 2, yDelta `div` 2)
    in R.traverse img id (\l0 idx@(Z :. x :. y) -> if x >= xh && (width - x) > xh &&
                                                      y >= yh && (height - y) > yh
                                                       then sum [ l0 (Z :. i :. j) | i <- [x - xh .. x + xh]
                                                                                   , j <- [y - yh .. y + yh] ]
                                                       else l0 idx)
{-# INLINE scwGeneric #-}

-- Minimum value from a neighborhood
--
-- NOTE: i must be less than 16!
erosion3_3 :: Morph DIM2
erosion3_3 a =
    let s = [stencil2| 1 1 1
                       1 1 1
                       1 1 1 |] -- R.makeStencil (Z :. i :. i) (Just . const 1) -- Max value == 255 * i * i == 231
        f v = if v /= 3*3*255 then 0 else 255
    in R.map f (R.computeUnboxedS $ mapStencil2 (R.BoundConst 0) s a)

erosion5_5 a =
    let s = [stencil2| 1 1 1 1 1
                       1 1 1 1 1
                       1 1 1 1 1
                       1 1 1 1 1
                       1 1 1 1 1|] -- R.makeStencil (Z :. i :. i) (Just . const 1) -- Max value == 255 * i * i == 231
        f v = if v /= 5*5*255 then 0 else 255
    in R.map f (R.computeUnboxedS $ mapStencil2 (R.BoundConst 0) s a)

erosion7_7 :: Morph DIM2
erosion7_7 a =
    let s = [stencil2| 1 1 1 1 1 1 1
                       1 1 1 1 1 1 1
                       1 1 1 1 1 1 1
                       1 1 1 1 1 1 1
                       1 1 1 1 1 1 1
                       1 1 1 1 1 1 1
                       1 1 1 1 1 1 1|] -- R.makeStencil (Z :. i :. i) (Just . const 1) -- Max value == 255 * i * i == 231
        f v = if v /= 7*7*255 then 0 else 255
    in R.map f (R.computeUnboxedS $ mapStencil2 (R.BoundConst 0) s a)

-- Maximum value from a neighborhood
dilation3_3 :: Morph DIM2
dilation3_3 a =
    let s = [stencil2| 1 1 1
                       1 1 1
                       1 1 1 |]
        f v = if v /= 0 then 255 else 0
    in R.map f (R.computeUnboxedS $ mapStencil2 (R.BoundConst 0) s a) -- (const $ R.extent a) f

dilation5_3 :: Morph DIM2
dilation5_3 a =
    let s = [stencil2| 1 1 1 1 1
                       1 1 1 1 1
                       1 1 1 1 1|]
        f v = if v /= 0 then 255 else 0
    in R.map f (R.computeUnboxedS $ mapStencil2 (R.BoundConst 0) s a) -- (const $ R.extent a) f

dilation5_5 :: Morph DIM2
dilation5_5 a =
    let s = [stencil2| 1 1 1 1 1
                       1 1 1 1 1
                       1 1 1 1 1
                       1 1 1 1 1
                       1 1 1 1 1|]
        f v = if v /= 0 then 255 else 0
    in R.map f (R.computeUnboxedS $ mapStencil2 (R.BoundConst 0) s a) -- (const $ R.extent a) f

dilation7_7 :: Morph DIM2
dilation7_7 a =
    let s = [stencil2| 1 1 1 1 1 1 1
                       1 1 1 1 1 1 1
                       1 1 1 1 1 1 1
                       1 1 1 1 1 1 1
                       1 1 1 1 1 1 1
                       1 1 1 1 1 1 1
                       1 1 1 1 1 1 1|]
        f v = if v /= 0 then 255 else 0
    in R.map f (R.computeUnboxedS $ mapStencil2 (R.BoundConst 0) s a) -- (const $ R.extent a) f

openning :: Morph DIM2
openning = dilation7_7 . dilation3_3 . erosion3_3 . erosion7_7

-- smallOpenning = smallDilation . smallErosion

smallErosion a =
    let s = [stencil2| 1 1
                       1 1 |] -- R.makeStencil (Z :. i :. i) (Just . const 1) -- Max value == 255 * i * i == 231
        f lk idx = if lk idx /= 9*255 then 0 else 255
    in R.traverse (mapStencil2 (R.BoundConst 0) s a) (const $ R.extent a) f

-- Maximum value from a neighborhood
-- dilation :: Int -> Morph DIM2
smallDilation a =
    let s = [stencil2| 1 1
                       1 1 |] -- makeStencil2 3 3 (Just . const 1)
        f lk idx = if lk idx /= 0 then 255 else 0
    in R.traverse (mapStencil2 (R.BoundConst 0) s a) (const $ R.extent a) f

-- closing :: Int -> Morph DIM2
-- closing = erosion . dilation

-- smallClosing = smallErosion . smallDilation

topHat :: Morph DIM2
topHat a = a R.-^ openning a

-- bottomHat :: Int -> Morph DIM2
-- bottomHat i a
--     | i == (2::Int) = smallClosing a R.-^ a
--     | otherwise     = closing a R.-^ a

otsuLevel :: Array D DIM2 Word8 -> Word8
otsuLevel arr =
    let hist = histogram arr
        histV = R.toUnboxed $ R.computeUnboxedS hist
        tot  = let (Z :. y :. x) = R.extent arr in y * x
        runningMul = V.zipWith (\v i -> fromIntegral v * i :: Int) histV (V.fromList [0..255])
        sm   = fromIntegral (V.sum $ V.drop 1 runningMul) :: Double
        wB   = V.scanl1 (+) histV
        wF   = V.map (\x -> tot - x) wB
        sumB = V.scanl1 (+) runningMul
        mB   = V.zipWith (\n d -> if d == 0 then 1 else fromIntegral n / fromIntegral d :: Double) sumB wB
        mF   = V.zipWith (\b f -> if f == 0 then 1 else (sm - fromIntegral b) / fromIntegral f) sumB wF
        between = V.zipWith4 (\x y b f -> fromIntegral x * fromIntegral y * (b-f)^2) wB wF mB mF
    in snd $ V.maximum (V.zip between (V.fromList [0..255]))

otsu :: Morph DIM2
otsu arr =
    let level = otsuLevel arr
    in trace (show level) $ threshold level arr

grayscaleToRGBA :: Convert DIM2 DIM3
grayscaleToRGBA a = R.traverse a (\(Z :. y :. x) -> Z :. y :. x :. four) expandChan
  where
  four = 4
  expandChan lk (Z :. y :. x :. chan)
    | chan == 0 = 255
    | otherwise = lk (Z :. y :. x)

grayscale :: Convert DIM3 DIM2
grayscale a = traverse a (\(Z :. y :. x :. _) -> Z :. y :. x) f
  where
  f lk sh =
        let r = fromIntegral $ lk (sh :. red) :: Double
            b = fromIntegral $ lk (sh :. blue)
            g = fromIntegral $ lk (sh :. green)
        in floor (0.2126 * r + 0.7152 * g + 0.0722 * b)

threshold :: Word8 -> Morph DIM2
threshold w a = R.traverse a id f
 where
 f lk (Z :. y :. x) =
        let v = lk (Z :. y :. x)
        in if v <= w then 255 else 0

equalizeImage :: Morph DIM2
equalizeImage img = R.map f img
  where hist = equalizedHistogram img
        f w  = hist `R.index` (Z :. fromIntegral w)

-- Compute The equalized histogram of an image
equalizedHistogram :: Convert DIM2 DIM1
equalizedHistogram img = R.delay $ R.fromUnboxed (Z :. 256) $ V.generate 256 f
  where
  (Z :. y :. x) = R.extent img
  mn      = V.minimum (R.toUnboxed $ R.computeUnboxedS cumu)
  cumu    = cumulative (histogram img)
  f i     = floor $ 255 * ((fromIntegral $ (cumu `R.index` (Z :. i)) - mn :: Double) / (fromIntegral $ y*x - mn))

-- Cumulative distribution function (given a histogram, compute the
-- percentage of each grayscale value)
cdf :: Array D DIM1 Int -> Array D DIM1 Double
cdf hist =
   let histV          = R.toUnboxed (R.computeUnboxedS hist)
       histCumulative = V.scanl1 (+) histV
       histTotal      = fromIntegral (V.sum histV) :: Double
       histDistrib    = V.map ((/ histTotal) . fromIntegral) histCumulative
   in delay $ R.fromUnboxed (R.extent hist) histDistrib

-- Take a histogram and return the cumulative values (scanl)
cumulative :: Array D DIM1 Int -> Array D DIM1 Int
cumulative hist =
    let histV = R.toUnboxed (R.computeUnboxedS hist)
    in delay $ R.fromUnboxed (R.extent hist) $ V.scanl1 (+) histV

histogram :: Array D DIM2 Word8 -> Array D DIM1 Int
histogram r =
    let monochrome = r -- traverse r (\(Z :. y :. x) -> Z :. y :. x) f :: Array D (Z :. Int :. Int) Word8
        len = 256
    in delay $ R.fromUnboxed (Z :. len) $ runST $
        do hist <- V.new 256
           V.set hist 0
           let (Z :. height :. width) = R.extent monochrome
               incIdx h (x,y)         =
                   let val = fromIntegral $ R.index monochrome (Z :. y :. x) :: Int
                   in V.write hist val . (+1) =<< V.read hist val -- XXX unsafeRead, unsafeWrite
           mapM (incIdx hist) [(x,y) | x <- [0..width-1], y <- [0..height-1]]
           V.unsafeFreeze hist

-- deriviative :: Image a -> Image a
-- derivative = onImg $ \arr ->
    

channel :: Int -> Convert DIM3 DIM2
channel c i =
    let f lk idx = lk (idx :. c)
    in R.traverse i (\(Z :. x :. y :. _) -> Z :. x :. y) f
