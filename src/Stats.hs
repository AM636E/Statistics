module Stats(runW, n, nDjun, intervalWidth, groupSelection, realData) where
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Text.Printf
import Data.List(sort)
import Control.Arrow
import System.Random
import Control.Monad(replicateM)
import Crypto.Random

type Selection = [[Float]]
type Interval = (Float, Float)
type Histogram = [(Interval, [Float])]
type IntervalFunc = Selection -> Float

data HistogramMap a = HistogramMap {
                         calcFunc :: Histogram -> [a]
                       , name :: String
                       , renderF :: a -> Picture
                    }

size :: Selection -> Int
size s = sum (map length s)

hSize :: Histogram -> Int
hSize h = sum (map (length.snd) h)

n :: IntervalFunc
n [] = 0
n s = 1 + 3.332 * logBase 10 ( fromIntegral $ size s )

nDjun :: IntervalFunc
nDjun [] = 0
nDjun s = 0.51 * sqrt (fromIntegral $ size s )

intervalWidth :: IntervalFunc -> Selection -> Float
intervalWidth _ [] = 0
intervalWidth f s = round' 2 ((max / min) / f s)
    where
        max = maximum (map maximum s)
        min = minimum  (map minimum s)

allInRange :: IntervalFunc -> Float -> Selection -> [Float]
allInRange f start a =
    concat [ [x | x <- y, x >= start && x < ( (round' 2 start) + (round' 2 iw) - 0.001)  ] | y <- a ]
    where
        iw = intervalWidth f a

groupSelection :: IntervalFunc -> Selection -> Histogram
groupSelection _ [] = []
groupSelection f a = [ tupleInRange $ round' 2 x | x <- [0,iw..max'] ]
    where
        tupleInRange start = ( (round' 2 start, round' 2 (start + iw)), map (round' 2) $ allInRange f start a )
        width = length (head a) -- selection is rectangular.
        iw = intervalWidth f a
        max' = maximum (map maximum a)
createHistogram = groupSelection

ints :: Histogram -> [(Interval, Int)]
ints = map (second length)

intervals :: HistogramMap (Interval, Int)
intervals = HistogramMap  {calcFunc = ints, name = "Intervals", renderF = renderOpt }


renderOpt :: Show a => (Interval, a) -> Picture
renderOpt (_, n) = scale 0.03 0.03 . text $ show n

middleIntervals :: HistogramMap (Interval, Float)
middleIntervals = HistogramMap { calcFunc = map (\((a, b), _) -> ((a, b), (a + b) / 2) ), name = "Middles", renderF = renderOpt }


-- Ni/N
dentities :: HistogramMap (Interval, Float)
dentities = HistogramMap { calcFunc = \hist -> map (\(i, l) -> (i, fromIntegral l / (fromIntegral $ hSize hist)) ) (ints hist) , name = "Densts", renderF = renderOpt }

si :: HistogramMap (Interval, Int)
si = HistogramMap { calcFunc = \hist -> map (\(n, (i, _)) -> (i, summ hist n) ) (idx hist) , name = "Si", renderF = renderOpt }
    where
        idx g = zip [1..] (ints g)
        summ g n = sum [ snd ( (ints g)!!k) | k <- [0..(n-1)] ]

fi :: HistogramMap (Interval, Float)
fi = HistogramMap { calcFunc = \hist -> map (\(n, (i, _)) -> (i, summ hist n) )  (idx hist) , name = "Fi", renderF = renderOpt }
    where
        idx g = zip [1..] (ints g)
        summ :: Histogram -> Int -> Float
        summ g n = sum [ (fromIntegral (snd ((ints g)!!k))) / ( fromIntegral $ hSize g) | k <- [0..(n-1)] ]

applyOption :: Histogram -> HistogramMap a -> ([a], String)
applyOption g opt = (calcFunc opt g, name opt)


renderMap :: Histogram -> HistogramMap a -> [Picture]
renderMap hist opt = [translate 10 (fst x * 10) (snd x) | x <- pics]
    where
        pics = zip [1..] ( [renderF opt x | x <- calcFunc opt hist] ++ [scale 0.04 0.04 $ text $ name opt])

realData :: Selection
realData = map (map (round' 2)) [
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
 ,[ x + 0.2 | x <- [2.6,3.8,3.9,4.9,5.0] ] ++ [ x - 0.2 | x <- [4.7,5.3,6.3,7.2,7.7] ]]

randomData :: Int -> IO Selection
randomData n = fmap (map (*100)) (replicateM n (randomIO :: IO Float)) >>= (\l -> return [l])

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
renderTuple ((l, r), xs) step = [ text'
                                    (5 + 20 * cls l, 5 + 10 * fromIntegral ( fst x - 1 ))
                                    (take 4 $ show $ snd x)
                                    | x <- indexate $ sort xs] ++
                                    [text' ( 6 + 20 * cls l, 6 + 10 * fromIntegral (length xs) ) (show (length xs)) ] ++
                                    [translate (7.5 + 20 * cls l) (7.5 + 10 * fromIntegral (length xs)) $ circle 5 ]
                           where
                            cls = smallClosest step

pics :: (Selection -> Float) -> Selection -> [Picture]
pics f selection = [
    -- Coordinate lines.
    line [(-500, 0), (500, 0)],
    rotate 90 $ line [(-500, 0), (500, 0)] ] ++
    -- Coordinates text.
    [ text' (x * 20, -4)
            (show $ round' 2 x) | x <- [0,step .. maxEl ]] ++
    -- Optional rectangle wrappers for labels.
    --[ rectangle (5 * cl (fst x)) 0 20 (10 * snd x) | x <- sdata ] ++
     concat [ render x | x <- dt ]
    where
        step = prettify (intervalWidth f selection) 3
        maxEl = maximum (map maximum selection)
        dt = filter (not.null.snd) $ createHistogram f selection
        sdata = map (\((s, _), xs) -> (s, fromIntegral (length xs))) $ filter (not.null) dt
        cl = smallClosest step
        render = (`renderTuple` step)

mpics :: Monad m => IntervalFunc -> m Selection -> m [Picture]
mpics f s = do
    sel <- s
    return (pics f sel)

round' :: Int -> Float -> Float
round' n fl = fromInteger (round (fl * (10 ^ n))) / (10 ^ n)

flip' (a, b) = (b, a)
viewPort = ViewPort (-50, -100) (90*0) 6

runW = do
    let window = InWindow "W" (900, 800) (100, 100)
    let hist = createHistogram n realData
    display window  white ( applyViewPortToPicture viewPort $ pictures $ pics nDjun realData )
   -- display window white (applyViewPortToPicture viewPort $ pictures $ renderMap hist fi)


