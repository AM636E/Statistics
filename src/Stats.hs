module Stats(runW, n, nDjun, intervalWidth, groupSelection, realData) where
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Text.Printf
import Data.List(sort)

type Selection = [[Float]]

size :: Selection -> Int
size s = sum (map length s)

n :: Selection -> Float
n [] = 0
n s = 1 + 3.332 * logBase 10 ( fromIntegral $ size s )

nDjun :: Selection -> Float
nDjun [] = 0
nDjun s = 0.51 * sqrt (fromIntegral $ size s )

intervalWidth :: (Selection -> Float) -> Selection -> Float
intervalWidth _ [] = 0
intervalWidth f s = (max / min) / f s
    where
        max = maximum (map maximum s)
        min = minimum  (map minimum s)

allInRange :: (Selection -> Float) -> Float -> Selection -> [Float]
allInRange f start a =
    concat [ [x | x <- y, x >= start && x < (start + iw)  ] | y <- a ]
    where
        iw = intervalWidth f a

groupSelection :: (Selection -> Float) -> Selection -> [((Float, Float), [Float])]
groupSelection _ [] = []
groupSelection f a = [ tupleInRange x | x <- [0,iw..max'] ]
    where
        tupleInRange start = ( (start, start + iw), allInRange f start a )
        width = length (head a) -- selection is rectangular.
        iw = intervalWidth f a
        max' = maximum (map maximum a)

realData :: Selection
realData = [
  [x - 0.2 | x <- [2.9,4.1,4.2,5.2,5.3,5.4, 5.0, 7.0, 7.9, 8.4]]
 ,[3.1, 4.3, 4.4, 5.4, 5.5, 5.6, 6.2, 7.2, 8.1, 8.6]
 ,[9.9, 5.2, 6.9, 8.9, 9.1, 3.3, 4.5, 8.1, 9.5, 10.5]
 ,[6.5, 7.7,8.4,9.2,3.5,4.6,5.7,6.7,7.3,8.6]
 ,[5.4,6.3,7.6,8.5,5.6,6.2,7.5,8.5,9.8,5.3]
 ,[5.9,6.7,7.2,6.3,4.1,5.5,6.8,7.4,8.7,9.4]
 ,[4.3,5.3,6.7,7.3,4.4,5.1,6.2,7.8,8.4,4.2]
 ,[7.7,5.4,6.9,7.6,5.6,6.2,7.7,5.7,6.3,7.3]
 ,[6.4,6.6,6.5,6.8,6.2,5.5,6.1,7.4,5.5,6.9]
 ,[2.4,3.6,3.7,4.7,9.8,4.9,5.5,6.5,7.4,7.9]
 ,[ x + 0.2 | x <- [2.6,3.8,3.9,4.9,5.0] ] ++
  [ x - 0.2 | x <- [4.7,5.3,6.3,7.2,7.7] ]]

text' (x, y) string = translate x y . scale 0.03 0.03  . color black
                     $ text string
rect' :: Float -> Float -> Float -> Float -> Picture
rect' x y w h = line [ (x, y), (x + w, y + h)]

w = intervalWidth n realData
max' = maximum (map maximum realData)
dtN = filter (not.null.snd) $ groupSelection n realData
dtDj = filter (not.null.snd) $ groupSelection nDjun realData

ndata :: [(Float, Float)]
ndata = map (\((s, _), l) -> (s, (fromIntegral $ length l) :: Float )) $
        filter (not.null) dtN
pw :: Float
pw = prettify (intervalWidth  nDjun realData ) 3

prettify :: Float -> Int -> Float
prettify 0 _ = 0
prettify x 0 = x
prettify x 1 = fromIntegral $ floor x
prettify x n
    | n > length (show x) = x
    | otherwise = read ( take (n + 1) (show x) ) :: Float

smallClosest :: Float -> Float -> Float
smallClosest _ 0 = 0
smallClosest step n = last [0,step..n]

rectangle x y w h = color red $ translate (x + (w/2)) (y + (h/2)) $ rectangleSolid w h

indexate :: [a] -> [(Int, a)]
indexate [] = []
indexate xs = zipWith (curry flip') xs [1..(length xs)]

renderTuple :: ((Float, Float), [Float]) -> Float -> [Picture]
renderTuple (_, []) _ = []
renderTuple ((0, 0), _) _ = []
renderTuple ((l, r), xs) step = [ text' (5 + 20 * cls l, 5 + 10 * fromIntegral ( fst x - 1 )) (take 4 $ show $ snd x)
                            | x <- indexate $ sort xs]
                           where
                            cls = smallClosest step


pics :: (Selection -> Float) -> Selection -> [Picture]
pics f selection = [
    -- Coordinate lines.
    line [(-500, 0), (500, 0)],
    rotate 90 $ line [(-500, 0), (500, 0)] ] ++
    -- Coordinates text.
    [ text' (x * 20, -4) (take 4 $ show x) | x <- [0,step .. maxEl ]] ++

    --[ rectangle (5 * cl (fst x)) 0 20 (10 * snd x) | x <- sdata ] ++
     concat [ render x | x <- dt ]
    where
        step = prettify (intervalWidth f selection) 3
        maxEl = maximum (map maximum selection)
        dt = filter (not.null.snd) $ groupSelection f selection
        sdata = map (\((s, _), xs) -> (s, fromIntegral (length xs))) $ filter (not.null) dt
        cl = smallClosest step
        render = (`renderTuple` step)


flip' (a, b) = (b, a)
viewPort = ViewPort (-50, -50) (90*0) 2

runW = do
    let window = InWindow "W" (900, 800) (100, 100)
    display window  white ( applyViewPortToPicture viewPort $ pictures $ pics nDjun realData )

