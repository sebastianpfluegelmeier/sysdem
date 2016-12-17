import Sysdem

main = r5

g x = s 0.14 * x
r4 = wide (fnToOsc (saw)) (melody [60, g 3, 65, g 1, 67, g 3, 66, g 2, 61, g 3, 63, g 2, 61, g 2]) (c 120) =>> sysdem

s1 = fnToOsc (saw) (c 440)
s2 = [0,0 ..]
r5 = [s2 ,s1 ] =>> map (take (s 1)) =>> sysdem 
