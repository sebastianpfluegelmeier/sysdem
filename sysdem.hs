module Sysdem where
import Data.WAVE
import Data.Int (Int32)
import Data.List.Split (splitOn)
import Data.Fixed

samplesPS = 44100
bitrate = 32

header = WAVEHeader 2 samplesPS bitrate Nothing

twist :: [[Int32]] -> [[Int32]]
twist x  
    | elem [] x = []
    | otherwise = map (head) x : twist (map (tail) x)

-- *** export functions

  -- ** signal functions
c :: Sample -> Signal
c x = [x, x ..]

linSig :: Sample -> Sample -> Length -> Signal
linSig start end length =
        [start + (end - start) * (fromIntegral current / fromIntegral length) | current <- [0,1 .. length]]

linSigs :: [(Sample, Length)] -> Sample -> Signal
linSigs ((amp, len):[])                lastAmp = linSig amp lastAmp len
linSigs ((amp, len):(amp2, len2):rest) lastAmp = 
                linSig amp amp2 len ++ linSigs ((amp2, len2):rest) lastAmp


  -- ** audio functions
    -- * delay
--delay :: Signal -> Signal -> Signal -> Signal -> Signal
--delay feedback length amounth signal =  

    -- * distortion
    
hardDist :: Kernel -> Signal
hardDist x = map (hardDistSample) x

hardDistSample :: Sample -> Sample
hardDistSample sig
        | sig >  1.0   =  1.0
        | sig < -1.0   = -1.0
        | otherwise  = sig

fnDist :: (Sample -> Sample) -> Signal -> Signal
fnDist fn sig = map (\x -> if x >= 0 then fn x else (tmo.fn.tmo) x) sig
        where tmo x = x * (-1.0)

    -- * filters

-- frequency stuff not working yet
lp :: Signal -> Signal -> Signal
lp freq signal = lp' (map (\x -> (1.0 / (x / 4000))) freq) signal 0

lp' :: Signal -> Signal -> Sample -> Signal
lp' beta         []         _   = []
lp' []           x          _   = []
lp' (beta:betas) (sig:sigs) avg = avg : lp' betas sigs (avg * beta + (sig * (1 - beta)))

hp :: Signal -> Signal -> Signal
hp beta input = zipWith (-) input (lp beta input)

bp :: Signal -> Signal -> Signal -> Signal
bp freq bandwidth input = input =>> (lp freq) =>> (hp (zipWith (+) freq bandwidth))

notch :: Signal -> Signal -> Signal -> Signal
notch freq bandwidth input = zipWith (-) (notch freq bandwidth input) (input)

hishelf :: Signal -> Signal -> Signal -> Signal
hishelf  freq gain input = zipWith (+) low (zipWith (*) high gain)
       where low  = lp freq input
             high = zipWith (-) low input

-- biquad b0 b1 b2 a0 a1 a2 x:xs:xss:xsss = [x | x <- biquad' b0 b1 b2 a0 a1 a2 x xs xss]
-- biquad :: Signal -> Signal -> Signal -> Signal -> Signal -> Signal -> Signal


-- not finished
lowShelf :: Signal -> Signal -> Signal -> Signal
lowShelf  freq gain input = zipWith (+) high (zipWith (*) low gain)
       where high  = hp freq input
             low = zipWith (-) low input
    
    -- * oscillators
additiveFn :: (Sample -> Sample -> Sample) -> Signal -> Int -> Signal
additiveFn fn freq otones = additiveFn' fn freq otones 1

additiveFn' :: (Sample -> Sample -> Sample) -> Signal -> Int -> Int -> Signal
additiveFn' fn freq otones nth 
            | nth > otones = [0.0, 0.0 ..]
            | otherwise    = zipWith (+) (partialFn (fn (fromIntegral nth)) freq) (additiveFn' fn freq otones (nth + 1))

partialFn :: (Sample -> Sample) -> Signal -> Signal
partialFn fn freq = zipWith (*) (fnToOsc (sine) freq) [(fn x) | x <- [1.0 ..]]

additive :: MultiSignal -> Signal -> Signal
additive amp freq = additive' amp freq 1

additive' :: MultiSignal -> Signal -> Int -> Signal
additive' [] _ _   = [0, 0 ..]
additive' (amp:amps) freq nth = zipWith (+) (partial amp freq) (additive' amps (map (* fromIntegral nth) freq) (nth + 1))

partial :: Signal -> Signal -> Signal
partial amp freq = zipWith (*) (fnToOsc (sine) freq) amp


fnToOsc :: (Sample -> Sample) -> Signal -> Signal
fnToOsc fn x = fnToOsc' fn x 0

fnToOsc' :: (Sample -> Sample) -> Signal -> Sample -> Signal
fnToOsc' fn [] phase = []
fnToOsc' fn x  phase = (fn phase) : fnToOsc' fn (tail x) (head x / (fromIntegral samplesPS) + phase)

sine :: Sample -> Sample
sine x = sin (x * pi / 2)

saw :: Sample -> Sample
saw x = (mod' x 4) / 2 - 1

square :: Sample -> Sample
square x
    | mod' x 4 < 2.0   = -1
    | otherwise = 1

tri :: Sample -> Sample
tri x = 2 * abs (tri (x / 2)) - 1

  -- ** utility functions
  
wide :: (Signal -> Signal) -> Signal -> Signal -> MultiSignal
wide fn mono diff = [fn (zipWith (+) mono diff), fn (zipWith (-) mono diff)]

      -- signal pipe
a =>> b = b $ a

normalize :: MultiSignal -> MultiSignal
normalize sig = map (map (/peak)) sig
        where peak = maximum $ map maximum sig 

zipN :: (a -> a -> a) -> [[a]] -> [a]
zipN fn (x:[]) = x
zipN fn (x:xs) = zipWith (fn) x (zipN fn xs)

s :: Sample -> Length
s x = round $ x * fromIntegral samplesPS

loop :: Int -> [a] -> [a]
loop 0 list = []
loop x list = list ++ loop (x - 1) list

stereo :: Signal -> MultiSignal
stereo x = [x, x]

sysdem :: MultiSignal -> IO ()
sysdem smp = putWAVEFile "out.wav" (WAVE header $ twist $ map (map doubleToSample) smp)

  -- ** note functions
n :: Note -> Sample
n note =  2 ** ((fromIntegral note - 49) / 12) * 440

melody :: [Note] -> Signal
melody []                 = []
melody (note:length:rest) = loop length [(n note)] ++ melody rest

notesLengths :: [Note] -> [Length] -> Signal
notesLengths notes lengths = melody $ recursiveNL notes lengths
    where recursiveNL lista listb = if lista == [] || listb == [] then [] else (head lista) : (head listb) : recursiveNL (tail lista) (tail listb) 

  -- ** archive

  --  *** constructors 
type Kernel = [Sample]
type Signal = [Sample]
type Note   = Int
type Length = Int
type Sample = Double 
type MultiSignal = [Signal]
