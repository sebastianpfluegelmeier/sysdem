module Sounddesign where
import Data.WAVE
import Data.Int (Int32)
import Data.List.Split (splitOn)
import Data.Fixed

samplesPS = 44000
bitrate = 32

header = WAVEHeader 2 samplesPS bitrate Nothing

mapDoubleToSample x = map doubleToSample x

twist :: [[Int32]] -> [[Int32]]
twist x  
    | elem [] x = []
    | otherwise = map (head) x : twist (map (tail) x)

-- *** export functions

  -- ** signal functions
conSig :: Double -> [Double]
conSig x = [x, x ..]

linearSig :: Double -> Double -> Int -> [Double]
linearSig start end length =
        [start + (end - start) * (fromIntegral current / fromIntegral length) | current <- [0,1 .. length]]

linearSegments :: [(Double, Int)] -> Double -> [Double]
linearSegments ((amp, len):[])              lastAmp = linearSig amp lastAmp len
linearSegments ((amp,len):(amp2,len2):rest) lastAmp = 
                linearSig amp amp2 len ++ linearSegments ((amp2, len2):rest) lastAmp


  -- ** audio functions

    -- * filters
applyFilterKernel :: [Double] -> [Double] -> [Double]
applyFilterKernel kernel (x:xs) 
    | length (x:xs) > length kernel = (kernelize (take (length kernel) (x:xs)) kernel) : applyFilterKernel kernel xs
    | otherwise           = []

kernelize :: [Double] -> [Double] -> Double
kernelize segment kernel = foldl (+) 0 (zipWith (*) kernel segment)

linearKernel :: Int -> [Double]
linearKernel size = normalize $ map fromIntegral $ halfAKernel ++ (reverse halfAKernel)
        where halfAKernel = ([x | x <- [0..size]])

normalize :: [Double] -> [Double]
normalize x = map (/ (foldl (+) 0 x)) x

recursiveFilter :: [Double] -> [Double] -> [Double]
recursiveFilter beta signal = recursiveFilter' beta signal 0

recursiveFilter' :: [Double] -> [Double] -> Double -> [Double]
recursiveFilter' beta         []         _   = []
recursiveFilter' []           x          _   = []
recursiveFilter' (beta:betas) (sig:sigs) avg = avg : recursiveFilter' betas sigs (avg * beta + (sig * (1 - beta)))
    
    -- * oscillators
additiveFn :: (Double -> Double -> Double) -> [Double] -> Int -> [Double]
additiveFn fn freq otones = additiveFn' fn freq otones 1

additiveFn' :: (Double -> Double -> Double) -> [Double] -> Int -> Int -> [Double]
additiveFn' fn freq otones nth 
            | nth > otones = [0.0, 0.0 ..]
            | otherwise    = zipWith (+) (partialFn (fn (fromIntegral nth)) freq) (additiveFn' fn freq otones (nth + 1))

partialFn :: (Double -> Double) -> [Double] -> [Double]
partialFn fn freq = zipWith (*) (fnToOsc (sine) freq) [(fn x) | x <- [1.0 ..]]

additive :: [[Double]] -> [Double] -> [Double]
additive amp freq = additive' amp freq 1

additive' :: [[Double]] -> [Double] -> Int -> [Double]
additive' [] _ _   = [0, 0 ..]
additive' (amp:amps) freq nth = zipWith (+) (partial amp freq) (additive' amps (map (* fromIntegral nth) freq) (nth + 1))

partial :: [Double] -> [Double] -> [Double]
partial amp freq = zipWith (*) (fnToOsc (sine) freq) amp


fnToOsc :: (Double -> Double) -> [Double] -> [Double]
fnToOsc fn x = fnToOsc' fn x 0

fnToOsc' :: (Double -> Double) -> [Double] -> Double -> [Double]
fnToOsc' fn [] phase = []
fnToOsc' fn x  phase = (fn phase) : fnToOsc' fn (tail x) (head x / (fromIntegral samplesPS) + phase)

sine :: Double -> Double
sine x = sin (x * pi / 2)

saw :: Double -> Double
saw x = (mod' x 1) * 2 - 1

square :: Double -> Double
square x
    | mod' x 1 < 0.5   = -1
    | otherwise = 1

tri :: Double -> Double
tri x 
    | mod' x 1 < 0.5 = x * 2 - 1
    | otherwise      = - (x * 2 - 1)

  -- ** utility functions
zipN :: (a -> a -> a) -> [[a]] -> [a]
zipN fn (x:[]) = x
zipN fn (x:xs) = zipWith (fn) x (zipN fn xs)

seconds :: Double -> Int
seconds x = round $ x * fromIntegral samplesPS

loop :: Int -> [Double] -> [Double]
loop 0 list = []
loop x list = list ++ loop (x - 1) list

stereo :: [Double] -> [[Double]]
stereo x = [x, x]

sdlw :: [[Double]] -> IO ()
sdlw smp = putWAVEFile "out.wav" (WAVE header $ twist $ map (mapDoubleToSample) smp)

  -- ** archive
  -- additive amp freq = zipN (+) ([partial x freq | x <- map (* y) amp ])
