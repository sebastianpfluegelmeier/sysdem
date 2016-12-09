module Sounddesign where
import Data.WAVE
import Data.Int (Int32)
import Data.List.Split (splitOn)

samplesPS = 44000
bitrate = 32

header = WAVEHeader 2 samplesPS bitrate Nothing

fromDouble :: [Double] -> [Int32]
fromDouble list = map round $ map (* 100000000) list

mapDoubleToSample x = map doubleToSample x

twist :: [[Int32]] -> [[Int32]]
twist x  
    | elem [] x = []
    | otherwise = map (head) x : twist (map (tail) x)

-- export functions

  -- signal functions
conSig :: Double -> [Double]
conSig x = [x, x ..]

linearSig :: Double -> Double -> Int -> [Double]
linearSig start end length = [start + (end - start) * (fromIntegral current / fromIntegral length) | current <- [0,1 .. length]]

  -- audio functions
testSine :: [Double]
testSine = take 100000 $ (map sin [0.0, 0.1 ..]) 

fnToOsc :: (Double -> Double) -> [Double] -> [Double]
fnToOsc fn x = fnToOsc' fn x 0

fnToOsc' :: (Double -> Double) -> [Double] -> Double -> [Double]
fnToOsc' fn [] phase = []
fnToOsc' fn x  phase = (fn phase) : fnToOsc' fn (tail x) (phase + (head x))

  -- utility functions
loop :: Int -> [Double] -> [Double]
loop 0 list = []
loop x list = list ++ loop (x - 1) list

stereo :: [Double] -> [[Double]]
stereo x = [x, x]

sdlw :: [[Double]] -> IO ()
sdlw smp = putWAVEFile "out.wav" (WAVE header $ twist $ map (mapDoubleToSample) smp)
