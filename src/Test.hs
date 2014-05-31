{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnboxedTuples #-}
module Main where

import           Codec.Picture.Repa
import qualified Data.Array.Repa.Stencil as R
import qualified Data.Array.Repa.Stencil.Dim2 as R
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

import Debug.Trace

main :: IO ()
main = getArgs >>= run . head

run :: FilePath -> IO ()
run fp = do
    eimg <- readImage fp
    img  <- either (error "Unable to load the image") return eimg
    let resultImage = imgToImage (morph img)
    B.writeFile (fp <.> "out" <.> "png" ) (imageToPng resultImage)

red,green,blue,alpha :: Int
red   = 3
blue  = 2
green = 1
alpha = 0

type Morph dim = Array D dim Word8 -> Array D dim Word8
type Convert dimA dimB = Array D dimA Word8 -> Array D dimB Word8

imgArray :: Img RGBA -> Array D DIM3 Word8
imgArray = delay . imgData

morph :: Img RGBA -> Img RGBA
morph orig =
    (computeS . grayscaleToRGBA . {- topHat 5 . threshold 100 . -} filterBy subRed (imgArray orig) . filterBy subGreen (imgArray orig) . otsu . equalizeImage . grayscale . delay) `onImg` orig

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
   | lk0 (sh :. alpha) > lk0 (sh :. blue) = 0
   | otherwise                          = lk1 sh

maxRed :: (DIM3 -> Word8) -> (DIM2 -> Word8) -> DIM2 -> Word8
maxRed lk0 lk1 sh@(Z :. _y :. _x)
   | lk0 (sh :. alpha) > lk0 (sh :. blue) = 255
   | otherwise                          = lk1 sh

filterBy :: ((DIM3 -> Word8)->  (DIM2 -> Word8) -> DIM2 -> Word8) -> Array D DIM3 Word8 -> Morph DIM2
filterBy f orig x = R.traverse2 orig x (\_ s -> s) f

-- Minimum value from a neighborhood
--
-- NOTE: i must be less than 16!
erosion :: Int -> Morph DIM2
erosion i a =
    let s = R.makeStencil2 i i (Just . const 1) -- Max value == 255 * i * i == 231
        f lk idx = if lk idx == 0 then 0 else 255
    in R.traverse (R.mapStencil2 R.BoundClamp s a) id f

-- Maximum value from a neighborhood
dilation :: Int -> Morph DIM2
dilation i a =
    let s = R.makeStencil2 i i (Just . const 1)
        f lk idx = if lk idx /= 0 then 255 else 0
    in R.traverse (R.mapStencil2 R.BoundClamp s a) id f

openning :: Int -> Morph DIM2
openning i = dilation i . erosion i

closing :: Int -> Morph DIM2
closing i = erosion i . dilation i

topHat :: Int -> Morph DIM2
topHat i a = a R.-^ openning i a

bottomHat :: Int -> Morph DIM2
bottomHat i a = closing i a R.-^ a

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
        max2 v  = max2L (zip (V.toList v) [0..]) ((0,0),(0,0))
        max2L :: [(Double,Int)] -> ((Double,Int),(Double,Int)) -> (Int,Int)
        max2L [] (a,b) = (snd a, snd b)
        max2L (x:xs) !acc@(!a,!b)
            | x <= b    = max2L xs acc
            | x > a     = max2L xs (x,a)
            | otherwise = max2L xs (a,x)
    in snd $ V.maximum (V.zip between (V.fromList [0..255])) -- fromIntegral $ uncurry (+) (max2 between) `div` 2

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

-- 3 == r
-- 2 == b
-- 1 == g
grayscale :: Convert DIM3 DIM2
grayscale a = traverse a (\(Z :. y :. x :. _) -> Z :. y :. x) f
  where
  f lk (Z :. y :. x) =
        let r = fromIntegral $ lk (Z :. y :. x :. 3) :: Double
            b = fromIntegral $ lk (Z :. y :. x :. 2)
            g = fromIntegral $ lk (Z :. y :. x :. 1)
        in floor (0.2126 * r + 0.7152 * g + 0.0722 * b)

threshold :: Word8 -> Morph DIM2
threshold w a = R.traverse a id f
 where
 f lk (Z :. y :. x) =
   -- | color == 0 = 255
   -- | otherwise = 
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
    
