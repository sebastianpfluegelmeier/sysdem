#/!bin/bash
ghc -O2 sysdem.hs csd.hs
./csd
aplay out.wav
rm sysdem.hi sysdem.o csd.hi csd.o csd
