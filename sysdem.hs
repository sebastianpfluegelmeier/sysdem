module Sysdem where
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
conSig :: Double -> Signal
conSig x = [x, x ..]

linearSig :: Double -> Double -> Int -> Signal
linearSig start end length =
        [start + (end - start) * (fromIntegral current / fromIntegral length) | current <- [0,1 .. length]]

linearSegments :: [(Double, Int)] -> Double -> Signal
linearSegments ((amp, len):[])              lastAmp = linearSig amp lastAmp len
linearSegments ((amp,len):(amp2,len2):rest) lastAmp = 
                linearSig amp amp2 len ++ linearSegments ((amp2, len2):rest) lastAmp


  -- ** audio functions
    -- * distortion
    
hardDist :: Kernel -> Signal
hardDist x = map (hardDistSample) x

hardDistSample :: Double -> Double
hardDistSample sig
        | sig >  1.0   =  1.0
        | sig < -1.0   = -1.0
        | otherwise  = sig
{--
fnDist :: (Double -> Double) -> Signal -> Signal
fnDist fn sig 
    | sig > 0.0  = map (fn) sig
    | otherwise  = map ((* (-1)).fn.(* (-1))) sig
    --}

    -- * filters
applyFilterKernel :: Signal -> Signal -> Signal
applyFilterKernel kernel (x:xs) 
    | length (x:xs) > length kernel = (kernelize (take (length kernel) (x:xs)) kernel) : applyFilterKernel kernel xs
    | otherwise           = []

kernelize :: Signal -> Kernel -> Double
kernelize segment kernel = foldl (+) 0 (zipWith (*) kernel segment)

linearKernel :: Int -> Kernel
linearKernel size = normalizeKernel $ map fromIntegral $ halfAKernel ++ (reverse halfAKernel)
        where halfAKernel = ([x | x <- [0..size]])

normalizeKernel :: Kernel -> Kernel
normalizeKernel x = map (/ (foldl (+) 0 x)) x

recursiveFilter :: Signal -> Signal -> Signal
recursiveFilter beta signal = recursiveFilter' beta signal 0

recursiveFilter' :: Signal -> Signal -> Double -> Signal
recursiveFilter' beta         []         _   = []
recursiveFilter' []           x          _   = []
recursiveFilter' (beta:betas) (sig:sigs) avg = avg : recursiveFilter' betas sigs (avg * beta + (sig * (1 - beta)))
    
    -- * oscillators
additiveFn :: (Double -> Double -> Double) -> Signal -> Int -> Signal
additiveFn fn freq otones = additiveFn' fn freq otones 1

additiveFn' :: (Double -> Double -> Double) -> Signal -> Int -> Int -> Signal
additiveFn' fn freq otones nth 
            | nth > otones = [0.0, 0.0 ..]
            | otherwise    = zipWith (+) (partialFn (fn (fromIntegral nth)) freq) (additiveFn' fn freq otones (nth + 1))

partialFn :: (Double -> Double) -> Signal -> Signal
partialFn fn freq = zipWith (*) (fnToOsc (sine) freq) [(fn x) | x <- [1.0 ..]]

additive :: MultiSignal -> Signal -> Signal
additive amp freq = additive' amp freq 1

additive' :: MultiSignal -> Signal -> Int -> Signal
additive' [] _ _   = [0, 0 ..]
additive' (amp:amps) freq nth = zipWith (+) (partial amp freq) (additive' amps (map (* fromIntegral nth) freq) (nth + 1))

partial :: Signal -> Signal -> Signal
partial amp freq = zipWith (*) (fnToOsc (sine) freq) amp


fnToOsc :: (Double -> Double) -> Signal -> Signal
fnToOsc fn x = fnToOsc' fn x 0

fnToOsc' :: (Double -> Double) -> Signal -> Double -> Signal
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
nestedMap :: (a -> b) -> [[a]] -> [[b]]
nestedMap fn list = map (map fn) list 

normalize :: MultiSignal -> MultiSignal
normalize sig = nestedMap (/peak) sig
        where peak = maximum $ map maximum sig 

zipN :: (a -> a -> a) -> [[a]] -> [a]
zipN fn (x:[]) = x
zipN fn (x:xs) = zipWith (fn) x (zipN fn xs)

seconds :: Double -> Int
seconds x = round $ x * fromIntegral samplesPS

loop :: Int -> Signal -> Signal
loop 0 list = []
loop x list = list ++ loop (x - 1) list

stereo :: Signal -> MultiSignal
stereo x = [x, x]

sdlw :: MultiSignal -> IO ()
sdlw smp = putWAVEFile "out.wav" (WAVE header $ twist $ map (mapDoubleToSample) smp)

  -- ** archive

  --  *** constructors 
type Kernel = [Double]
type Signal = [Double]
type MultiSignal = [Signal]
