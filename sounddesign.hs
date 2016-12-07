module Sounddesign where
import Data.WAVE
import Data.Int (Int32)
import Data.List.Split (splitOn)

samplesPS = 44000
bitrate = 32

header = WAVEHeader 1 samplesPS bitrate Nothing

fromDouble :: [Double] -> [Int32]
fromDouble list = map round $ map (* 100000000) list

sound :: [Int32]
sound = take 100000 $ fromDouble $ (map sin [0.0, 0.1 ..]) 

samples :: [Int32] -> [[Int32]]
samples smp = map (:[]) $ smp


waveData :: [Int32] -> WAVE
waveData smp = WAVE header $ samples smp


makeWavFile :: [Int32] -> IO ()
makeWavFile smp = putWAVEFile "out.wav" (waveData smp)

main = sdlw sound
sdlw :: [Int32] -> IO ()
sdlw smp = makeWavFile smp
