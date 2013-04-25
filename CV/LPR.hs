{-# LANGUAGE FlexibleContexts, BangPatterns #-}
import Prelude hiding (min)
import CV.Image
import CV.ImageMathOp
import CV.ImageMath
import CV.Thresholding
import CV.ConnectedComponents
import System.Environment
import Control.Monad
import Control.Applicative
import CV.Morphology
import CV.Filters
import CV.Pixelwise
import qualified Data.Set as S
import Control.Concurrent

main = do
    [fp] <- getArgs
    Just img <- loadImage fp
    tmp <- unsafeImageTo8Bit `fmap` cloneImage img
    lpr1 img "lpr1"

type Op a = (String, a -> a)

mk :: (Image c d -> Image c d) -> String -> Op (Image c d)
mk o n = (n,o)

ops :: Save (Image c d) => String -> Image c d -> [Op (Image c d)] -> IO (Image c d)
ops base img os = foldM (\i o -> op base o i) img os


op :: (Save (Image c d)) => String -> Op (Image c d) -> Image c d -> IO (Image c d)
op base (n,o) i = do
    let i' = o i
    saveImage (base ++ "-" ++ n ++ ".jpg") i'
    return i'

lpr1 :: Image GrayScale D32 -> String -> IO [String]
lpr1 img baseName = do
    blobs <- obtainPotentials img baseName
    zipWithM blobToString blobs [1..]

    {-
    let circleGoodContours look pnt = if (pnt `S.member` goodPnts) then 255 else look pnt
        goodPnts = S.fromList $ map (\(x,y) -> (floor x, floor y)) . concat $ csGood
        masked = toImage . remap circleGoodContours. fromImage $ noisyPlate :: Image GrayScale D8
    saveImage "result.jpg" masked
    return dilated
    -}

-- Obtain potential plates using addition, erosion, thresholding, connected
-- component analysis (thus obtaining blobs) then dilation, and grouping to
-- capture areas of potential plates.
--
-- Each area is finally filtered for three or more connected components.
obtainPotentials :: Image GrayScale D32 -> String -> IO [Image GrayScale D8]
obtainPotentials img baseName = do
    let bw = unsafeImageTo8Bit
        st = mk id "startImage"
        df = mk (\i -> add i (erode basicSE 1 i)) "enhance"
        th = mk (threshold MaxAndZero 128 . thresholdOtsu ZeroAndMax) "threshold"
        er = mk (erode basicSE 1) "03.erode"
        cc = mk (fst . fillConnectedComponents) "connected-components"
        th2 = mk (threshold MaxAndZero 128) "threshold2"
        sc = mk (selectSizedComponents 20 400) "sized-components"
    startImage <- return $ bw img
    firstPass  <- ops baseName startImage [st,df,th,cc,th2] :: IO (Image GrayScale D8)
    tmpNoRT    <- cloneImage firstPass
    noisyPlate <- op baseName sc tmpNoRT
    tmp2 <- cloneImage noisyPlate
    let di = mk (dilate basicSE 9) "dilate"
    dilated <- op baseName di tmp2 :: IO (Image GrayScale D8)

    let cs = getContours dilated -- Get all blobs in the image.
        cAreas  = mapContours contourArea cs
        cPerims = mapContours contourPerimeter cs
        cRel    = zipWith getRel cAreas cPerims
        cPnts   = mapContours contourPoints cs
        -- A contour relates well if the permimeter and area are related in
        -- a manner simialar to a square, except the license plate has
        -- holes (letters) inside that subtract from the area.
        goodRelation (sideA,sideP) = sideA * 1.0 < sideP && sideP < sideA * 2
        getRel a p =
            let sideP = p/4
                sideA = Prelude.sqrt a
            in (sideA, sideP)
        csGood = map fst $ filter (goodRelation . snd) (zip cPnts cRel)
        csBad  = map fst $ filter (not . goodRelation . snd) (zip cPnts cRel)
        blobRects = map rectOf csGood
        potentials = map (\r -> copyRect r firstPass {- noisyPlate -}) blobRects
    return $ filter (\t -> let x = countBlobs t in x > 3 && x < 30) potentials

getAllBlobs :: Image GrayScale D8 -> [Image GrayScale D8]
getAllBlobs img =
    let cs        = getContours img
        cPnts     = mapContours contourPoints cs
        blobRects = map rectOf cPnts
    in map (\r -> copyRect r img) blobRects

blobToString :: Image GrayScale D8 -> Int -> IO String
blobToString blob bn = do
    {-
    let df = (\i -> add i (erode basicSE 1 i))
        th = (threshold MaxAndZero 128 . thresholdOtsu ZeroAndMax)
        er = (erode basicSE 1)
        cc = (fst . fillConnectedComponents)
        th2 = (threshold MaxAndZero 128)
        sc = (selectSizedComponents 20 400)
        newImg = sc . th2 . cc . er . th . df $ blob
        firstPass  = foldl (\b f -> f b) blob [df,th,cc,th2,sc] :: Image GrayScale D8
    -}
    let letters = getAllBlobs blob
    print ("nr letters in blob " ++ show bn,length letters)
    zipWithM_ (\l n -> saveImage ("blob-" ++ show bn ++ "-letter-" ++ show n ++ ".png") l) letters [1..]
    saveImage ("blob" ++ show bn ++ ".png") blob
    return ""

countBlobsSafe i = do
    i2 <- cloneImage i
    let !n = countBlobs i2
    return n

tileBlobs :: [Image GrayScale D8] -> Image GrayScale D8
tileBlobs [x] = x
tileBlobs (x:y:rest) =
    let new = tileImages x y ((\(x,y) -> (fromIntegral x, 0)) $ getSize x)
    in tileBlobs (new:rest)

-- Obtain the minimum and maximum X and Y values, returned as a tuple of
-- (X,Y) points.
rectOf :: [(Double,Double)] -> ((Int,Int),(Int,Int))
rectOf ps = 
    let xs = map (floor . fst) ps
        ys = map (floor . snd) ps
    in ((minimum xs, minimum ys), (maximum xs, maximum ys))

copyRect :: ((Int,Int), (Int,Int)) -> Image c d -> Image c d
copyRect (pnt@(xB,yB), (xT,yT)) img =
    let w = xT - xB
        h = yT - yB
    in getRegion pnt (w,h) img

{-
    let minOp  = mk (min dilated) "minByDilate"
    minImg <- op baseName minOp startImage
    noisyPlate2 <- ops (baseName ++ "-2") minImg [st,th,cc,sc] :: IO (Image GrayScale D8)
    return noisyPlate2
    -}
